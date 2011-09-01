#lang racket
(require ffi/unsafe)

;;; MODULE GLOBALS & CONSTANTS
(define GIT_OID_RAWSZ 20)
(define GIT_OID_HEXSZ (* 2 GIT_OID_RAWSZ))
(define GIT_OID_MINPREFIXLEN 4)
(define libgit2 (ffi-lib "libgit2"))

;;; SYNTAX
;; This is a common pattern to help define library funcs 
(define-syntax defgit
  (syntax-rules (:)
    [(_ name : type ...)
     (define name
       (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_") (#rx"[*?]$" "")))
                    libgit2 (_fun type ...)))]))

(define-syntax defgit/provide
  (syntax-rules ()
    [(_ name x ...) (begin (provide name) (defgit name x ...))]))

;; Helps checking error results
(define-fun-syntax _status
  (syntax-id-rules (_status)
    [_status (type: _int
                    post: (r => (when (not (zero? r))
                                  (error 'status-failed (format "failed with code: ~a" r)))))]))

(define-for-syntax (build-name-stx upcase? stx parts)
    (datum->syntax
     stx
     (string->symbol
      (apply string-append
             (map (lambda (p)
                    (if (syntax? p)
                        (let ([str (symbol->string (syntax-e p))])
                          (if upcase?
                              (string-upcase str)
                              str))
                        p))
                  parts)))))

(define-syntax (define-cpointer-type/finalizer stx)
  (define (build-name #:upcase? [upcase? #f] . parts) (build-name-stx upcase? stx parts))
  (syntax-case stx ()
    [(_ name release-func)
     #`(define-cpointer-type #,(build-name "_git_" #'name) #f #f
         (lambda (p) (register-finalizer p #,(build-name "git-" #'name "-" #'release-func)) p))]))

(define-syntax (define-object-type stx)
  (define (build-name #:upcase? [upcase? #f] . parts) (build-name-stx upcase? stx parts))
  (syntax-case stx ()
    [(_ name) #`(begin
                  (define #,(build-name "_git_" #'name) 
                    (_cpointer '#,(build-name "_git_" #'name) _git_object #f
                               (lambda (p) (register-finalizer p git-object-close) p)))
                  (provide #,(build-name "git-" #'name "-lookup"))
                  (define (#,(build-name "git-" #'name "-lookup") repo oid)
                    (let ([obj (git-object-lookup repo oid '#,(build-name #:upcase? #t "GIT_OBJ_" #'name))])
                      (cpointer-push-tag! obj '#,(build-name "_git_" #'name))
                      obj))
                  (define (#,(build-name "git-" #'name "-lookup-prefix") repo oid)
                    (let ([obj (git-object-lookup-prefix repo oid '#,(build-name #:upcase? #t "GIT_OBJ_" #'name))])
                      (cpointer-push-tag! obj (symbol->string '#,(build-name "git_" #'name)))
                      obj))                  
                  (define (#,(build-name "git-" #'name "-close") obj)
                    (git-object-close obj))
                  
                                        ; Blob does not have the git-blob-id... 
                                        ;(defgit  #,(build-name "git-" #'name "-id") : #,(build-name "_git_" #'name) -> _oid)
                  )]))

;;; OIDs
; OIDs are just an array of length GIT_OID_RAWSZ.
(provide string->oid
         oid->string )

(define-fun-syntax _oid
  (syntax-id-rules (o i)
    [(_ o) (_bytes o GIT_OID_RAWSZ)]
    [(_ i) (type: _bytes
                  pre: (x => (cond [(bytes? x) x]
                                   [(string? x) (string->oid x)]
                                   [#t (error "Bad input oid")]))) ]
    [_ (_bytes o GIT_OID_RAWSZ) ]))

(defgit git-oid-fromstr :
  [oid :  (_oid o) ]
  [hex : _string]
  -> _status
  -> oid)

(defgit git-oid-fmt :
  [str : (_bytes o GIT_OID_HEXSZ)]
  (_oid i)
  -> _void
  -> (bytes->string/utf-8 str))

(define (string->oid s)
  (if (regexp-match (pregexp (format "^[a-fA-F0-9]{~a}$" GIT_OID_HEXSZ))
                    s)
      (git-oid-fromstr s)
      (error 'string->oid "Bad oid string representation")))

(define (oid->string oid)
  (if (= (bytes-length oid) GIT_OID_RAWSZ)
      (git-oid-fmt oid)
      (error 'oid->string "Bad oid")))

;;; REPOSITORY
(define-cpointer-type _git_repository #f
  (lambda (x) x)
  (lambda (p) (register-finalizer p git-repository-free) p))

; Not to be called from the outside to prevent double free
; (used by the finalizer)
(defgit git-repository-free : _git_repository -> _void)

(defgit/provide git-repository-open : [repo : (_ptr o _git_repository)]  _path -> _status -> repo)

;;; OBJECT
(define _git_otype
  (_enum '(GIT_OBJ_ANY = -2    
                       GIT_OBJ_BAD = -1    
                       GIT_OBJ__EXT1 = 0   
                       GIT_OBJ_COMMIT = 1  
                       GIT_OBJ_TREE = 2    
                       GIT_OBJ_BLOB = 3    
                       GIT_OBJ_TAG = 4     
                       GIT_OBJ__EXT2 = 5   
                       GIT_OBJ_OFS_DELTA = 6
                       GIT_OBJ_REF_DELTA = 7)))

(define-cpointer-type _git_object #f #f
  (lambda (p) (register-finalizer p git-object-close) p))

(defgit git-object-lookup :
  [object : (_ptr o _git_object)]  _git_repository (_oid i)  _git_otype -> _status -> object)

(defgit git-object-lookup-prefix : [object : (_ptr o _git_object)]  _git_repository _bytes _int _git_otype -> _status -> object)

(defgit git-object-close : _git_object -> _void )

;;; STRUCTS
(provide (struct-out git-signature)
         (struct-out git_time_in_signature))

(define-cstruct _git_time_in_signature
  ([time _int64]
   [offset _int]))

(define-cstruct _git-signature
  ([name _bytes]
   [email _bytes]
   [when _git_time_in_signature]))

(define _git_time_t _uint64)

;;; ODB
(define-cpointer-type/finalizer odb close)
(define-cpointer-type _git_odb_obj #f #f
  (lambda (p) (register-finalizer p git-odb-object-close) p))

;TODO: Custom backend functions ignored
;TODO: Stream-related functions ignored
(defgit git-odb-open : [odb : (_ptr o _git_odb)] _path -> _status -> odb)
(defgit git-odb-close : _git_odb -> _void )
(defgit git-odb-read : [obj : (_ptr o _git_odb_obj)] _git_odb _oid -> _status -> obj)
(defgit git-odb-read-prefix : [obj : (_ptr o _git_odb_obj)] _git_odb _oid _int -> _status -> obj)
(defgit git-odb-read-header : [len : (_ptr o _int)] [type : (_ptr o _git_otype)] _git_odb _oid
  -> _status -> (values len type ))
(defgit git-odb-exists : _git_odb _oid -> _bool)
(defgit git-odb-write : _oid _git_odb [buf : _bytes] [len : _int = (bytes-length buf)] _git_otype -> _status)

(defgit git-odb-hash : (_oid o) [buf : _bytes] [len : _int = (bytes-length buf)] _git_otype -> _status)
(defgit git-odb-hashfile : (_oid o) _path _git_otype -> _status)
(defgit git-odb-object-close : _git_odb_obj -> _void)
(defgit git-odb-object-id : _git_odb_obj -> _oid)
(defgit git-odb-object-data : _git_odb_obj -> _bytes)
(defgit git-odb-object-size : _git_odb_obj -> _int)
(defgit git-odb-object-type : _git_odb_obj -> _git_otype )

;;; TAG
(define-object-type tag)
(defgit git-tag-id : _git_tag -> _oid)
(defgit git-tag-target : [obj : (_ptr o _git_object)] _git_tag -> _status -> obj)
(defgit git-tag-target-oid : _git_tag -> _oid)
(defgit git-tag-type : _git_tag -> _git_otype )
(defgit git-tag-name : _git_tag -> _string )
(defgit git-tag-message : _git_tag -> _string )
(defgit git-tag-tagger : _git_tag -> _git-signature )
(defgit git-tag-create : _oid _git_repository _bytes _git_object _git-signature _bytes _int -> _status )
(defgit git-tag-create-frombuffer : _oid _git_repository _string _int -> _status )
(defgit git-tag-create-lightweight : _oid _git_repository _status _git_object _int -> _status )
(defgit git-tag-delete : _git_repository _string -> _status)
(defgit git-tag-list : _git_repository _string -> _status)
;;TODO: git-tag-list ... Need to port the strarray datatype

;;; TREE
(define-object-type tree)
(define _git_tree_entry (_cpointer 'git_tree_entry))
(define _git_index (_cpointer 'git_index))
(define _git_treebuilder (_cpointer 'git_treebuilder))
(define _git_config (_cpointer 'git_config))
(define _git_config_file (_cpointer 'git_config_file))
(define _git_reflog_entry (_cpointer 'git_reflog_entry))

(defgit git-tree-id : _git_tree -> _oid)
(defgit git-tree-entrycount : _git_tree -> _uint)
(defgit git-tree-entry-byname : _git_tree _path -> _git_tree_entry )
(defgit git-tree-entry-byindex : _git_tree _uint -> _git_tree_entry )
(defgit git-tree-entry-attributes : _git_tree_entry -> _uint )
(defgit git-tree-entry-name : _git_tree_entry -> _path )
(defgit git-tree-entry-id : _git_tree_entry -> _oid)
(defgit git-tree-entry-type : _git_tree_entry -> _git_otype )
(defgit git-tree-entry-2object :
  [obj : (_ptr o _git_object)]
  _git_repository
  _git_tree_entry
  -> _status
  -> obj)

(defgit git-tree-create-fromindex : _oid _git_index -> _status )
(defgit git-treebuilder-create :
    [obj : (_ptr o _git_treebuilder)]
    _git_tree
    -> _status
    -> obj )

(defgit git-treebuilder-clear : _git_treebuilder ->  _void )
(defgit git-treebuilder-free : _git_treebuilder -> _void ) ;;FREE
(defgit git-treebuilder-get : _git_treebuilder _path -> _git_tree_entry )
(defgit git-treebuilder-remove : _git_treebuilder _path -> _status )
;(defgit git-treebuilder-filter :) ;;TODO: Callback
(defgit git-treebuilder-write : _oid _git_repository _git_treebuilder -> _status )

;;; BLOB
(define-object-type blob)
(defgit git-blob-rawcontent : _git_blob -> _pointer )
(defgit git-blob-rawsize : _git_blob -> _int )
(defgit git-blob-create-fromfile : _oid _git_repository _path -> _int )
(defgit git-blob-create-frombuffer :
  _oid
  _git_repository
  [buf : _bytes ]
  [len : _int = (bytes-length buf) ] -> _status)

;;; COMMIT
(define-object-type commit)

(defgit/provide git-commit-message : _git_commit -> _bytes)
(defgit/provide git-commit-id : _git_commit ->  _oid )
(defgit/provide git-commit-message-encoding : _git_commit -> _string)
(defgit/provide git-commit-time : _git_commit -> _git_time_t )
(defgit/provide git-commit-author : _git_commit -> _git-signature-pointer )
(defgit/provide git-commit-committer : _git_commit -> _git-signature-pointer )

(defgit/provide git-commit-time-offset : _git_commit -> _int)
(defgit/provide git-commit-tree-oid : _git_commit -> _oid)
(defgit/provide git-commit-parentcount : _git_commit -> _uint)

(defgit git-commit-parent :
  [parent : (_ptr o _git_commit)]
  _git_commit
  _uint
  -> _status 
  -> parent)

(defgit git-commit-parent-oid : _git_commit _uint -> _oid)

