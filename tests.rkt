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
(define repo-path
  (build-path (let-values ([(dir _ __)
                            (split-path (syntax-source #'here))]) dir)
              (string->path ".git")))

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
 "Repository"
 (check-exn exn:fail? ;TODO: Improve error message...
            (lambda () (git-repository-open "potato")))
 (check-not-exn
  (lambda ()
    (git-repository-open repo-path))))

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


(git-run-tests)

