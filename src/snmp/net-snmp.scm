;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 2.0.2
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.

(define-module (snmp net-snmp))
 

(eval-when (eval load compile)

  (use-modules (oop goops))
  (use-modules (srfi srfi-39))

  ; The module will hook these up with architecture specific
  ; srfi-4 routines
  (define empty-oidvec #f)
  (define oidvector #f)
  (define make-oidvector #f)
  (define oidvector? #f)
  (define oidvector-length #f)
  (define list->oidvector #f)
  (define oidvector->list #f)
  (define oidvector-ref #f)
  (define oidvector-set! #f)

  (define-class <oid> ()
		(_vec #:init-value empty-oidvec
		      #:init-keyword #:value))

  (define oid-translate (make-parameter #t))

    (define-method (display (this <oid>) port)
      (if (oid-translate)
        (let* ((node     (get-tree this (get-tree-head)))
               (basename (slot-ref node 'label))
               (diff     (- (oid-from-tree-node node) this)))
          (format port "~a~{.~d~}" basename (oid->list diff)))
        (format port "~{.~d~}" (oid->list this))))
  
    (define-method (write (this <oid>) port)
      (if (oid-translate)
       (let* ((node     (get-tree this (get-tree-head)))
               (basename (slot-ref node 'label))
               (diff     (- (oid-from-tree-node node) this)))
          (format port "#<oid: ~a~{.~d~}>#" basename (oid->list  diff)))
        (format port "#<oid: ~{.~d~}>#" (oid->list this))))

  (define-method (equal? (a <oid>) (b <oid>))
		 (equal? (slot-ref a '_vec) (slot-ref  b '_vec)))

  (define-method (equal? (a <oid>) b)
		 (equal? (slot-ref a '_vec) b))

  (define-method (equal? (a <oid>) b)
		 (equal? (slot-ref a '_vec) b))

  (define-method (equal? a (b <oid>))
		 (equal? a (slot-ref  b '_vec)))

  (define-method (oid->list (this <oid>))
		 (oidvector->list (slot-ref  this '_vec)))

  (define-method (list->oid this)
		 (make <oid> #:value (list->oidvector this)))

  (export 
    <oid> 
    oid-translate
    oidvector 
    make-oidvector 
    oidvector? 
    oidvector-length
    list->oidvector 
    oidvector->list
    oidvector-ref
    oidvector-set!
    list->oid 
    oid->list 
    empty-oidvec)

  (load-extension "libguile_snmp_net-snmp" "scm_init_snmp_net_snmp_module"))

(use-modules (oop goops))
(use-modules ((snmp net-snmp-primitive) :renamer (symbol-prefix-proc 'primitive:)))

;(define oid-from-varbind primitive:oid-from-varbind)
;(define guile-snmp-async-response primitive:guile-snmp-async-response)

(define-class <snmp-constant> ()
  (value #:init-keyword #:value) 
  (symbol #:init-keyword #:symbol)) 

(define-method (display (this <snmp-constant>) port)
  (format port "~a" (slot-ref this 'symbol)))
(define-method (write (this <snmp-constant>) port)
  (format port "~a" (slot-ref this 'symbol)))

(define constant-classes (make-hash-table 20))
(export constant-classes)

(define-syntax define-constant-class
  (syntax-rules ()
    ((_ name)
     (begin
       (define-class name (<snmp-constant>))
       (hashq-set! constant-classes name (make-hash-table 32))  
       (export name)))))

(define-syntax define-constant
  (syntax-rules ()
    ((_ type name)
     (begin
       (define name (make type 
                          #:value (local-ref (list  
                          (string->symbol (string-append "primitive:_wrap_" 
                                          (symbol->string (quote name))))))
			  #:symbol (quote name)))
       (hashq-set! (hashq-ref constant-classes type) 
		   (local-ref (list  (string->symbol (string-append "primitive:_wrap_" 
                                          (symbol->string (quote name)))))) 
		   name) 
       (export name)))))

(define (constant-name-from-value class val)
  (hashq-ref (hashq-ref constant-classes class) val))
(export constant-name-from-value)

(define-constant-class <snmp-version>)
(define-constant <snmp-version> SNMP-VERSION-1) 
(define-constant <snmp-version> SNMP-VERSION-2c) 
(define-constant <snmp-version> SNMP-VERSION-2u) 
(define-constant <snmp-version> SNMP-VERSION-3) 
(define-constant <snmp-version> SNMP-VERSION-sec) 
(define-constant <snmp-version> SNMP-VERSION-2p) 
(define-constant <snmp-version> SNMP-VERSION-2star) 

(define-constant-class <snmp-msg>)
(define-constant <snmp-msg> SNMP-MSG-GET)
(define-constant <snmp-msg> SNMP-MSG-GETNEXT)
(define-constant <snmp-msg> SNMP-MSG-RESPONSE)
(define-constant <snmp-msg> SNMP-MSG-SET)
(define-constant <snmp-msg> SNMP-MSG-TRAP)
(define-constant <snmp-msg> SNMP-MSG-GETBULK)
(define-constant <snmp-msg> SNMP-MSG-INFORM)
(define-constant <snmp-msg> SNMP-MSG-TRAP2)
(define-constant <snmp-msg> SNMP-MSG-REPORT)

(define-constant-class <asn-type>)
(define-constant <asn-type> ASN-BOOLEAN)
(define-constant <asn-type> ASN-INTEGER)
(define-constant <asn-type> ASN-BIT-STR)
(define-constant <asn-type> ASN-NULL)
(define-constant <asn-type> ASN-OBJECT-ID)
(define-constant <asn-type> ASN-SEQUENCE)
(define-constant <asn-type> ASN-SET)
(define-constant <asn-type> ASN-OCTET-STR)
(define-constant <asn-type> ASN-IPADDRESS)
(define-constant <asn-type> ASN-COUNTER)
(define-constant <asn-type> ASN-GAUGE)
(define-constant <asn-type> ASN-UNSIGNED)
(define-constant <asn-type> ASN-TIMETICKS)
(define-constant <asn-type> ASN-OPAQUE)
(define-constant <asn-type> ASN-NSAP)
(define-constant <asn-type> ASN-COUNTER64)
(define-constant <asn-type> ASN-UINTEGER)
(define-constant <asn-type> ASN-FLOAT)
(define-constant <asn-type> ASN-DOUBLE)
(define-constant <asn-type> ASN-INTEGER64)
(define-constant <asn-type> ASN-UNSIGNED64)

(define-constant-class <snmp-status>)
(define-constant <snmp-status> SNMP-NOSUCHOBJECT)
(define-constant <snmp-status> SNMP-NOSUCHINSTANCE)
(define-constant <snmp-status> SNMP-ENDOFMIBVIEW)
(define-constant <snmp-status> STAT-SUCCESS)
(define-constant <snmp-status> STAT-ERROR)
(define-constant <snmp-status> STAT-TIMEOUT)

(define-constant-class <snmp-err-status>)
(define-constant <snmp-err-status> SNMP-ERR-NOERROR)
(define-constant <snmp-err-status> SNMP-ERR-TOOBIG)
(define-constant <snmp-err-status> SNMP-ERR-NOSUCHNAME)
(define-constant <snmp-err-status> SNMP-ERR-BADVALUE)
(define-constant <snmp-err-status> SNMP-ERR-READONLY)
(define-constant <snmp-err-status> SNMP-ERR-GENERR)
(define-constant <snmp-err-status> SNMP-ERR-NOACCESS)
(define-constant <snmp-err-status> SNMP-ERR-WRONGTYPE)
(define-constant <snmp-err-status> SNMP-ERR-WRONGLENGTH)
(define-constant <snmp-err-status> SNMP-ERR-WRONGENCODING)
(define-constant <snmp-err-status> SNMP-ERR-WRONGVALUE)
(define-constant <snmp-err-status> SNMP-ERR-NOCREATION)
(define-constant <snmp-err-status> SNMP-ERR-INCONSISTENTVALUE)
(define-constant <snmp-err-status> SNMP-ERR-RESOURCEUNAVAILABLE)
(define-constant <snmp-err-status> SNMP-ERR-COMMITFAILED)
(define-constant <snmp-err-status> SNMP-ERR-UNDOFAILED)
(define-constant <snmp-err-status> SNMP-ERR-AUTHORIZATIONERROR)
(define-constant <snmp-err-status> SNMP-ERR-NOTWRITABLE)
(define-constant <snmp-err-status> SNMP-ERR-INCONSISTENTNAME)

;(define-class <snmp-session> ()
;  (version #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-version-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-version-set obj value)))
;  (retries #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-retries-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-retries-set obj value)))
;  (timeout #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-timeout-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-timeout-set obj value)))
;  (subsession #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-subsession-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-subsession-set obj value)))
;  (next #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-next-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-next-set obj value)))
;  (peername #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-peername-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-peername-set obj value)))
;  (remote-port #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-remote-port-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-remote-port-set obj value)))
;  (localname #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-localname-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-localname-set obj value)))
;  (local-port #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-local-port-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-local-port-set obj value)))
;  (contextName #:allocation #:virtual
;   #:slot-ref (lambda (obj) (primitive:snmp-session-contextName-get obj))
;   #:slot-set! (lambda (obj value) (primitive:snmp-session-contextName-set obj value)))
;  #:new-function primitive:new-snmp-session
;)

;(define snmp-sess-init primitive:snmp-sess-init)
;(define snmp-open primitive:snmp-open)
;(define snmp-close primitive:snmp-close)
;(define snmp-close-sessions primitive:snmp-close-sessions)

(define-class primitive:<tree> ()
  ptr
  (label #:allocation #:virtual
   #:slot-ref primitive:tree-label-get
   #:slot-set! primitive:tree-label-set)
  #:name '<tree>)

(define-macro (re-export name)
  `(begin
     (define ,name ,(string->symbol
		       (string-append
		         "primitive:"
		         (symbol->string name))))
     (export ,name)))

(re-export <tree>)
(re-export <snmp-session>)
(re-export <values>)

(re-export init-mib)
(re-export init-snmp)
(re-export snmp-parse-oid)
(re-export get-tree)
(re-export get-tree-head)
(re-export oid-from-tree-node)


;(export 
;  <snmp-session> 
;  <oid>)
