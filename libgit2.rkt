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
  (define str
    (apply string-append
           (for/list ([p parts])
                     (if (syntax? p)
                         (symbol->string (syntax-e p))
                         p))))
  (datum->syntax
   stx
   (string->symbol (if upcase?
                       (string-upcase str)
                       str))))

(define-syntax (defptr/release stx)
  (define (build-name #:upcase? [upcase? #f] . parts)
    (build-name-stx upcase? stx parts))
  (syntax-case stx ()
    [(_ name release-func)
     (with-syntax ([type-name (build-name "_git_" #'name)]
                   [finalizer-name (build-name "git-" #'name "-" #'release-func) ])
       #'(begin
           (defgit finalizer-name : _pointer -> _void)
           (define-cpointer-type type-name #f #f
             (lambda (p) (register-finalizer p finalizer-name) p))))]))

(define-syntax (define-object-type stx)
  (define (build-name #:upcase? [upcase? #f] . parts) (build-name-stx upcase? stx parts))
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([type-name (build-name "_git_" #'name)]
                   [lookup-name (build-name "git-" #'name "-lookup")]
                   [close-name (build-name "git-" #'name "-close")]                   
                   [lookup-prefix-name (build-name "git-" #'name "-lookup-prefix")]                   
                   [enum-type-name (build-name #:upcase? #t "GIT_OBJ_" #'name)])
       #'(begin
           (define type-name 
             (_cpointer 'type-name _git_object #f
                        (lambda (p) (register-finalizer p git-object-close) p)))
           (provide lookup-name)
           (define (lookup-name repo oid)
             (let ([obj (git-object-lookup repo oid 'enum-type-name)])
               (cpointer-push-tag! obj 'type-name)
               obj))
           (define (lookup-prefix-name repo oid)
             (let ([obj (git-object-lookup-prefix repo oid 'enum-type-name)])
               (cpointer-push-tag! obj (symbol->string 'type-name))
               obj))
           (define (close-name obj)
             (git-object-close obj))))]))

;;; OIDs
; OIDs are just an array of length GIT_OID_RAWSZ.
(provide string->oid
         oid->string )

(define-fun-syntax _oid
  (syntax-id-rules (o i io :)
                   [(_ o) (_bytes o GIT_OID_RAWSZ)]
                   [(_ io) (_bytes io GIT_OID_RAWSZ)]                   
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

(defgit/provide git-object-lookup :
  [object : (_ptr o _git_object)]  _git_repository (_oid i)  _git_otype -> _status -> object)

(defgit git-object-lookup-prefix : [object : (_ptr o _git_object)]  _git_repository _bytes _int _git_otype -> _status -> object)

(defgit git-object-close : _git_object -> _void )
(defgit/provide git-object-type : _git_object -> _git_otype )
(defgit/provide git-object-id : _git_object -> _oid )

;TODO: no ID Shortening funcs. Some other functions missing, but I
;don't think they're really needed from Racket

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
(defptr/release odb close)
(defptr/release odb_object close)


;TODO: Custom backend functions ignored
;TODO: Stream-related functions ignored
(defgit/provide git-repository-database : _git_repository -> _git_odb )

(defgit git-odb-open : [odb : (_ptr o _git_odb)] _path -> _status -> odb)
(defgit/provide git-odb-read : [obj : (_ptr o _git_odb_object)] _git_odb (_oid i) -> _status -> obj)
(defgit git-odb-read-prefix : [obj : (_ptr o _git_odb_object)] _git_odb _oid _int -> _status -> obj)
(defgit/provide git-odb-read-header :
  [len : (_ptr o _int)]
  [type : (_ptr o _git_otype)]
  _git_odb
  (_oid i)
  -> _status
  -> (values len type))

(defgit/provide git-odb-exists : _git_odb (_oid i)  -> _bool)
(defgit git-odb-write : _oid _git_odb [buf : _bytes] [len : _int = (bytes-length buf)] _git_otype -> _status)

(defgit/provide git-odb-hash : [hash : (_oid o) ]  [buf : _bytes] [len : _int = (bytes-length buf)] _git_otype -> _status -> hash)
(defgit/provide git-odb-hashfile : [hash : (_oid o) ]  _path _git_otype -> _status -> hash)
(defgit git-odb-object-close : _git_odb_object -> _void)
(defgit/provide git-odb-object-id : _git_odb_object -> _oid)
(defgit/provide git-odb-object-data : [obj : _git_odb_object]  -> [o : _bytes]  -> (subbytes o 0 (git-odb-object-size obj)))
(defgit/provide git-odb-object-size : _git_odb_object -> _int)
(defgit/provide git-odb-object-type : _git_odb_object -> _git_otype )

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
(define _git_treebuilder (_cpointer 'git_treebuilder))
(define _git_config (_cpointer 'git_config))
(define _git_config_file (_cpointer 'git_config_file))
(define _git_reflog_entry (_cpointer 'git_reflog_entry))

(defgit/provide git-tree-id : _git_tree -> _oid)
(defgit/provide git-tree-entrycount : _git_tree -> _uint)
(defgit/provide git-tree-entry-byname : _git_tree _path -> _git_tree_entry )
(defgit/provide git-tree-entry-byindex : _git_tree _uint -> _git_tree_entry )
(defgit/provide git-tree-entry-attributes : _git_tree_entry -> _uint )
(defgit/provide git-tree-entry-name : _git_tree_entry -> _path )
(defgit/provide git-tree-entry-id : _git_tree_entry -> _oid)
(defgit/provide git-tree-entry-type : _git_tree_entry -> _git_otype )

(defgit/provide git-tree-entry-2object :
  [obj : (_ptr o _git_object)]
  _git_repository
  _git_tree_entry
  -> _status
  -> obj)

;(defgit git-tree-create-fromindex : _oid _git_index -> _status )
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
(defgit/provide git-blob-rawcontent : [b : _git_blob ]  -> [o : _bytes]  -> (subbytes o 0 (git-blob-rawsize b)))
(defgit/provide git-blob-rawsize : _git_blob -> _int )

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

(defgit git-commit-parent :             ; Private, by now...
  [parent : (_ptr o _git_commit)]
  _git_commit
  _uint
  -> _status 
  -> parent)

(defgit/provide git-commit-parent-oid : _git_commit _uint -> _oid)

;;; REVWALK

(defptr/release revwalk free )
