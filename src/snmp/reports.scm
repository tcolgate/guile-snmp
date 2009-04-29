;;-------------------------------------------------------------------
;; Copyright (C) 2009 Tristan Colgate 
;;
;; utilities.scm - This file defines classes and utilities to provide
;; a more expressive environment for SNMP reporting
;;
;;-------------------------------------------------------------------

(define-module (snmp reports)
  #:use-syntax (oop goops)
  #:use-syntax (ice-9 syncase)
  #:use-module (snmp net-snmp))

; This routine is lifted from guile-gnome-platform by Andy Wingo
(define-macro (re-export-modules . args)
  (if (not (null? args))
      (begin
        (or (list? (car args))
            (error "Invalid module specification" (car args)))
        `(begin
           (module-use! (module-public-interface (current-module))
                        (resolve-interface ',(car args)))
           (re-export-modules ,@(cdr args))))))

 
(enable-primitive-generic! +)
(define-method (+ (id1 <uvec>) (id2 <uvec>))
  (list->u32vector 
    (append 
      (uniform-vector->list id1) 
      (uniform-vector->list id2)))) 
(define-method (+ (id1 <uvec>) (id2 <integer>))
  (list->u32vector 
    (append 
      (uniform-vector->list id1) 
      (list id2)))) 
(define-method (+ (id1 <integer>) (id2 <uvec>))
  (list->u32vector 
    (append 
      (list id1) 
      (uniform-vector->list id2)))) 
(define-method (+ (id1 <uvec>))
  id1)
(define-method (+ . args)
  (+ (car args)
    (apply + (cdr args))))
(define-method (+)
   0)

(enable-primitive-generic! -)
(define-method (- (base <uvec>) (id <uvec>))
  (let* ((baselist (uniform-vector->list base))
         (idlist (uniform-vector->list  id))
         (prefixlen (length baselist))
         (idlen (length idlist))
         (prefix    (list-head idlist prefixlen)))
    (if (equal? baselist prefix)
      (list->u32vector(list-tail idlist prefixlen))
      id)))
(define-method (- (id1 <uvec>))
  id1)
(define-method (-)
   0)

(define-generic %)

(define-method (% (s <integer>)(e <integer>) (id <uvec>))
  (sub-objid s e id))

(define-method (% (s <integer>)(id <uvec>))
  (sub-objid s s id))

(define (sub-objid s e id)
  (list->u32vector 
      (list-tail (list-head (uniform-vector->list id)  e ) (- s 1))))


(define reports:autotranslate #f)
(define-syntax init-reports
  (syntax-rules ()
    ((init-reports)
      (begin 
        (define (oid-lazy-binder mod sym def?)
          (if reports:autotranslate
              (let ((oid (snmp-parse-oid (symbol->string sym))))
                (if (unspecified? oid)
                  #f
                  (let* ((var (make-variable oid)))
                    (module-add! mod sym var)
                    var)))
              #f))
        (define module (make-module 31 '() oid-lazy-binder))
        (set-module-uses! (current-module) 
          (append (module-uses (current-module)) (list module)))
        (init-mib)))))

(define current-session #f)
(define current-context 'ctx-snmpget)
(define failure-continuation #f)

(define-syntax session
  (syntax-rules ()
    ((session hostname hostcommunity)
      (let ((newsession (make <snmp-session>)))
        (snmp-sess-init newsession)
 	(slot-set! newsession 'version (SNMP-VERSION-2c))
	(slot-set! newsession 'peername hostname)
	(slot-set! newsession 'community hostcommunity)
        (slot-set! newsession 'community-len (string-length hostcommunity))
        (snmp-open newsession)))
    ((session hostname hostcommunity statements ...)
      (let ((newsession (make <snmp-session>)))
        (snmp-sess-init newsession)
 	(slot-set! newsession 'version (SNMP-VERSION-2c))
	(slot-set! newsession 'peername hostname)
	(slot-set! newsession 'community hostcommunity)
        (slot-set! newsession 'community-len (string-length hostcommunity))
	;;; almost violently non-threadsafe
        (set! current-session (snmp-open newsession))
        (set! current-context 'ctx-snmpget)
        statements ...))))

;; Given a varbind list and the oids requested that
;; generated it. Tag each reult with the based oid
;; and compute the iid.
;; We only match the first, but should try and
;; find the most specific match.
(define prop-tag (make-object-property))
(define prop-iid (make-object-property))
(define (tag-varbinds result bases)
  (let nextvarbind ((varitem result))
    (if (not (null? varitem))
      (begin
        (let nextbase ((baseitems bases))
          (if (equal? (netsnmp-oid-is-subtree (car baseitems) (oid-from-varbind varitem)) 0)
            (begin
              (set! (prop-tag varitem) (car baseitems))
              (set! (prop-iid varitem) (- (car baseitems) (oid-from-varbind varitem)))
              (display "tagged ")
              (display (oid-from-varbind varitem))
              (display " with ")
              (display (prop-tag varitem))
              (display " iid ")
              (display (prop-iid varitem))
              (newline))
            (if (not (equal? (cdr baseitems) '()))
              (nextbase (cdr baseitems))
              (begin
                ; fallback, if we get here (which we shouldn't!0
                ; set the iid and tags to gueeses
                (set! (prop-tag varitem) (oid-from-varbind varitem))
                (set! (prop-iid varitem) #u32())
                (display "guess tagged ")
                (display (oid-from-varbind varitem))
                (display " with ")
                (display (prop-tag varitem))
                (display " iid ")
                (display (prop-iid varitem))
                (newline)))))
        (nextvarbind (slot-ref varitem 'next-variable))))))

(define (make-varbind-func varbinds)
      (lambda( . msg)
        (if (not (null? varbinds))
          (cond
            ((eq? msg '()) varbinds )
            ((uniform-vector? (car msg))
              (let nextvarbind ((var varbinds))
                (if (null? var)
                  (display "No such oid in varbind")
                  (begin 
                  (display "comparing: ")
                  (display (car msg))
                  (display " to ")
                  (display (oid-from-varbind var))
                  (display (prop-tag var))
                  (newline)
                  (if (eq? (snmp-oid-compare (prop-tag var) (car msg)) 0)
                      (cond 
                        ((equal? (cdr msg) '())   var) 
                        ((equal? (cdr msg) (list 'oid))   (oid-from-varbind var))
                        ((equal? (cdr msg) (list 'tag))   (prop-tag var))
                        ((equal? (cdr msg) (list 'iid))   (prop-iid var))
                        ((equal? (cdr msg) (list 'type))  (slot-ref var 'type))
                        ((equal? (cdr msg) (list 'value)) var)
                        (#t var))
                      (nextvarbind  (slot-ref var 'next-variable)))))))
            ((equal? (car msg) 'oid) (oid-from-varbind varbinds))
            ((equal? (car msg) 'tag) (prop-tag varbinds))
            ((equal? (car msg) 'iid) (prop-iid varbinds))
            ((equal? (car msg) 'type) (slot-ref varbinds 'type))
            ((equal? (car msg) 'nextvar) (slot-ref varbinds 'next-variable))
            ((equal? (car msg) 'value) varbinds )
            (#t varbinds ))
          #f)))

(define-syntax oid
  (syntax-rules ()
    ((tag varbind args ...) (varbind args ... 'oid))))

(define-syntax tag
  (syntax-rules ()
    ((tag varbind args ...) (varbind args ... 'tag))))

(define-syntax iid
  (syntax-rules ()
    ((iid varbind args ...) (varbind args ... 'iid))))

(define-syntax type
  (syntax-rules ()
    ((type varbind args ...) (varbind args ... 'type))))

(define-syntax value
  (syntax-rules ()
    ((value varbind args ...) (varbind args ... 'value))))

(define-syntax nextvar
  (syntax-rules ()
    ((nextvar varbind ) (make-varbind-func (varbind 'nextvar)))
    ((nextvar varbind args ...) ((make-varbind-func (varbind 'nextvar)) args ...))))

(define-syntax get
  (syntax-rules ()
    ((get oid-terms ...)
      (let* ((oids (list oid-terms ...))
             (newpdu (snmp-pdu-create (SNMP-MSG-GET))))
       (for-each 
         (lambda(oid)
           (snmp-add-null-var newpdu oid)) 
         oids)
       (let ((status (snmp-synch-response current-session newpdu)))
         (let ((results (slot-ref status 'variables)))
           (tag-varbinds results oids)
           (make-varbind-func results)))))))

; This is used to track our failure
(define failure-cont
  (lambda () (display "Options exhausted")))

;; With built in backtracking
(define (getnext-with-failure-cont baseoids)
  (let ((old-failure-cont failure-cont))
    (call/cc
      (lambda (continuation)
        (define (try initialoids pdu)
          (let ((status (snmp-synch-response current-session pdu)))
            (if (eq? status 100)
              (begin
                ;; we got to the end of the tree or failed somehow
                (set! failure-cont old-failure-cont)
                (failure-cont))
              (begin
                ;; we succceeded. set up next set of oids and call try
                ;; again if we are deemed to have failed later on
                (let ((nextpdu (snmp-pdu-create(SNMP-MSG-GETNEXT))))
                  (let add-oids ((varbind (slot-ref status 'variables)))
                    (if (not (null? varbind))
                      (begin
                        (snmp-add-null-var nextpdu (oid-from-varbind varbind))
                        (add-oids  (slot-ref varbind 'next-variable))))
                  (set! failure-cont
                    (lambda () (continuation (try initialoids nextpdu)))))
                ;; return the value we got before
                (let ((results (slot-ref status 'variables)))
                  (tag-varbinds results initialoids)
                  (make-varbind-func results)))))))
        (let ((firstpdu (snmp-pdu-create(SNMP-MSG-GET))))
          (for-each 
            (lambda(oid)
              (snmp-add-null-var firstpdu oid)) 
            baseoids)
          (try baseoids firstpdu))))))

(define-syntax walk
  (syntax-rules ()
    ((walk oid-terms ...)
      (getnext-with-failure-cont (list oid-terms ...)))))

(define-syntax print
  (syntax-rules ()
    ((print var )
       (print-variable (oid var) (var)))
    ((print var oid )
       (print-variable oid (var oid)))))

; Set up the reports environement
;
(init-snmp (car(command-line)))
(init-mib)
(set! reports:autotranslate #f)

(export current-session current-context reports:autotranslate)
(export-syntax init-reports session oid-list walk get <objid> ids sub-objid %)
(export-syntax oid tag iid type value nextvar print prop-tag prop-iid)
(export failure-cont)
(export getnext-with-failure-cont make-varbind-func tag-varbinds)

(re-export-modules (oop goops) (ice-9 syncase) (snmp net-snmp))

