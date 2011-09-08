#lang racket
(require rackunit
         rackunit/text-ui
         "libgit2.rkt")

;;; SYNTAX
(define-for-syntax test-cases/ '())
(define-syntax (git-test stx)
  (set! test-cases/
        (cons
         (syntax-case stx ()
           [(_ desc body ...) #'(test-case desc body ...)])
         test-cases/))
  #'(void))

(define-syntax (git-run-tests stx)
  #`(run-tests (test-suite "libgit2"
                           #,@test-cases/)))

;;; GLOBALS
(define (source-rel-path . rel)
  (apply build-path (cons (let-values ([(dir _ __)
                            (split-path (syntax-source #'here))]) dir)
                          rel)))

(define repo-path (source-rel-path ".git"))

(define first-commit-str "0ddbfc8f266e73d777591ce31d3ddb4ccb7a9d37")
(define second-commit-str "4d210ffc8633da215ed65fbfbfd7d21b87509575")
(define tree-str "3695f941bc2e3ccdcdbdd8606649e7367bc2b085")
(define first-readme-blob-str "045804cac227d14cc7945d80bdaeb818a6d14af7")

;;; TESTS
(git-test
 "OID Conversion"
 (define test-hex "599955586da1c3ad514f3e65f1081d2012ec862d")
 (check-equal? test-hex
               (oid->string (string->oid test-hex))))


(git-test
 "Commit"
 (let* ([repo (git-repository-open repo-path)]
        [first-commit (git-commit-lookup repo first-commit-str)]
        [second-commit (git-commit-lookup repo second-commit-str)])
   (check-equal? (git-signature-name (git-commit-committer first-commit))
                 #"Joan Arnaldich")
   (check-equal? (git-signature-email (git-commit-committer first-commit))
                 #"jarnaldich@gmail.com")
   (check-equal? (git-commit-message first-commit)
                 #"First commit\n")
   (check-equal? (git-commit-id first-commit)
                 (string->oid first-commit-str))
   (check-equal? (git-commit-message-encoding first-commit)
                 #f) ; Default encoding
   (check-equal? (git-signature-name (git-commit-author first-commit))
                 #"Joan Arnaldich")
   (check-equal? (git-signature-email (git-commit-author first-commit))
                 #"jarnaldich@gmail.com")
   (check-equal? (git-commit-time first-commit)
                 1314857888)
   (check-equal? (git-commit-time-offset first-commit)
                 120)
   (check-equal? (oid->string (git-commit-tree-oid first-commit))
                 tree-str
                 "3695f941bc2e3ccdcdbdd8606649e7367bc2b085")
   (check-equal? (git-commit-parentcount first-commit)
                 0)
   (check-equal? (git-commit-parentcount second-commit)
                 1)
   (check-equal? (git-commit-parent-oid second-commit 0)
                 (string->oid first-commit-str))))

(git-test
 "Tree"
 (let* ([repo (git-repository-open repo-path)]
        [tree-oid (string->oid tree-str)]
        [tree (git-tree-lookup repo tree-str)])
   (check-equal? (git-commit-tree-oid
                  (git-commit-lookup repo first-commit-str))
                 tree-oid)
   (check-equal? (git-tree-id tree) tree-oid)
   (check-equal? (git-tree-entrycount tree) 1)
   (let ([tree-entry (git-tree-entry-byindex tree 0)])
     (check-equal? tree-entry
                   (git-tree-entry-byname tree
                                          (git-tree-entry-name tree-entry)))
     (check-equal? (git-tree-entry-attributes tree-entry)
                   #o100644)
     (check-equal? (git-tree-entry-id tree-entry)
                   (string->oid first-readme-blob-str))
     (check-equal? (git-tree-entry-type tree-entry)
                   'GIT_OBJ_BLOB)
     (check-equal? (git-object-id (git-tree-entry-2object repo tree-entry))
                   (string->oid first-readme-blob-str))
     ;; Tree creation tests pending
     )))

(git-test
 "Blob"
 (let* ([repo (git-repository-open repo-path)]
        [blob-oid (string->oid first-readme-blob-str)]
        [blob (git-blob-lookup repo first-readme-blob-str)])
   (check-equal? (git-blob-rawsize blob)
                 (bytes-length (git-blob-rawcontent blob)))
   (check regexp-match #rx"Description" (git-blob-rawcontent blob))))

(git-test
 "ODB"
 (let* ([repo (git-repository-open repo-path)]
        [odb (git-repository-database repo)]
        [first-readme-oid (string->oid first-readme-blob-str)])
   (check-not-equal? (git-odb-exists odb first-readme-oid) 0)
   (let-values ([(len type)  (git-odb-read-header odb first-readme-oid)])
     (check-equal? type 'GIT_OBJ_BLOB)
     (check-equal? len 118))
   (let ([odb-obj (git-odb-read odb first-readme-oid)])
     (check-equal? (git-odb-object-size odb-obj) 118)
     (check-equal? (git-odb-object-type odb-obj) 'GIT_OBJ_BLOB)
     (check-equal? (git-odb-object-id odb-obj) first-readme-oid)
     (check-equal? (git-odb-hash (git-odb-object-data odb-obj) 'GIT_OBJ_BLOB)
                   first-readme-oid)
     (let ([tmp (make-temporary-file "RGT~a")])  ; for Racket Git Test
       (display-to-file (git-odb-object-data odb-obj) tmp #:exists 'truncate/replace)
       (check-equal? (oid->string (git-odb-hashfile tmp 'GIT_OBJ_BLOB))
                     (oid->string first-readme-oid))))))

(git-test
 "REVWALK"
 (let* ([repo (git-repository-open repo-path)]
        [walk (git-revwalk-new repo)]
        [commit-list '("f60e30f7c1737e14fde79512b2b01f88277cffc4" 
                       "30c03590df8c71dc32bde333ceebc9f373b44e34" 
                       "1b22b09d8b36138940cf49147e81fedec623887d" 
                       "4d210ffc8633da215ed65fbfbfd7d21b87509575" 
                       "0ddbfc8f266e73d777591ce31d3ddb4ccb7a9d37" 
                       "76f62effc71d0230c81bbec6c70f635f42102e0b")])

   (define (check-walk results)
     (let loop ([oid (git-revwalk-next walk)]
                [commit-list results])
       (if oid
           (begin (check-equal? (car commit-list)
                                (oid->string oid))
                  (loop (git-revwalk-next walk)
                        (cdr commit-list)))
           (check-equal? commit-list
                         '()))))
   (check-equal? (git-revwalk-repository walk) repo)
   (git-revwalk-push walk (string->oid "f60e30f7c1737e14fde79512b2b01f88277cffc4"))
   (check-walk commit-list)
   
   (git-revwalk-reset walk)
   (git-revwalk-push walk (string->oid "f60e30f7c1737e14fde79512b2b01f88277cffc4"))   
   (git-revwalk-sorting walk 'GIT_SORT_REVERSE)
   (check-walk (reverse commit-list))

   (git-revwalk-reset walk)
   (git-revwalk-push walk (string->oid "f60e30f7c1737e14fde79512b2b01f88277cffc4"))
   (git-revwalk-hide walk (string->oid "30c03590df8c71dc32bde333ceebc9f373b44e34"))   
   (check-walk '())))

(git-test
 "TAG"
 (let* ([repo (git-repository-open repo-path)]
        [first-commit-oid (string->oid first-commit-str)]
        [first-commit (git-commit-lookup repo first-commit-oid)]
        [tag-oid (git-tag-create repo
                                 #"test-tag"
                                 first-commit
                                 (make-git-signature #"Joan Arnaldich"
                                                     #"jarnaldich@gmail.com"
                                                     (make-git-time-in-signature (current-seconds)
                                                                                 2))
                                 #"A tag for unit testing"
                                 1)]
        [tag (git-tag-lookup repo tag-oid)])

   (define (check-delete name)
     (let ([ref-name (bytes-append #"refs/tags/" name)])
       (check member ref-name (git-tag-list repo))
       (git-tag-delete repo name)
       (check-false (member ref-name (git-tag-list repo)))))

   (check-equal? (git-tag-id tag) tag-oid)
   (check-equal? (git-tag-target-oid tag) first-commit-oid)
   (check-equal? (git-tag-type tag) 'GIT_OBJ_COMMIT)
   (check-equal? (git-object-id (git-tag-target tag))
                 first-commit-oid)
   (check-equal? (git-tag-name tag) #"test-tag")
   (check-equal? (git-tag-message tag)    #"A tag for unit testing")
   (check-equal? (git-signature-name (git-tag-tagger tag))
                 #"Joan Arnaldich")
   (git-tag-create-lightweight repo #"test-tag-light" first-commit 1)
   (for-each check-delete '(#"test-tag" #"test-tag-light"))))

(git-test
 "INDEX"
 (let* ([index-path (build-path repo-path "index")]
        [repo (git-repository-open repo-path)]        
        [index (git-index-open index-path)]
        [repo-index (git-repository-index repo)])
   (for/list ([i (in-range (git-index-entrycount index))])
             (let ([entry (git-index-get index i)]
                   [repo-entry (git-index-get repo-index i)])
               (check-equal?  (git-index-entry-path entry)
                              (git-index-entry-path repo-entry))))))

(git-test
 "REPO"
 (let* ([index-path (build-path repo-path "index")]
        [tree-path (source-rel-path ".")]
        [repo (git-repository-open repo-path)])
   (check-equal? (git-repository-path repo 'GIT_REPO_PATH)
                 (path->directory-path repo-path))
   (check-equal? (git-repository-is-bare repo)
                 0)
   (check-equal? (git-repository-is-empty repo) 0)
;   (check-equal? (git-repository-discover (make-bytes 256) repo-path 1 repo-path) repo-path)
   (check-equal? (git-repository-path
                  (git-repository-open3 repo-path
                                        (git-repository-database repo)
                                        index-path
                                        tree-path)
                  'GIT_REPO_PATH)
                 (git-repository-path repo 'GIT_REPO_PATH))
   (check-equal? (git-repository-path
                  (git-repository-open2 repo-path
                                        (source-rel-path ".git" "objects")
                                        index-path
                                        tree-path)
                  'GIT_REPO_PATH)
                 (git-repository-path repo 'GIT_REPO_PATH))))


(git-run-tests)

