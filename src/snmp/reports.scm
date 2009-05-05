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


(define reports:autotranslate #t)
(define-syntax init-reports
  (syntax-rules ()
    ((init-reports)
      (begin 
        (define (oid-lazy-binder mod sym def?)
          (if reports:autotranslate
              (let ((oid (snmp-parse-oid (symbol->string sym))))
                (if (unspecified? oid)
                  (begin 
                    (display "Failed to resolve ")
                    (display (symbol->string sym))
                    (display " as oid")(newline)
                    (error "Exitting")
                    #f)
                  (let* ((var (make-variable oid)))
                    (module-add! mod sym var)
                    var)))
              #f))
        (define module (make-module 31 '() oid-lazy-binder))
        (set-module-uses! (current-module) 
          (append (module-uses (current-module)) (list module)))
        (init-mib)))))

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

(define-class <report-varlist> (<variable-list>)
  (nextvar #:init-value #f)
  (tag #:init-value #u32())
  (base #:init-value #u32())
  (iid #:init-value #u32()))

; Walking variable->next_variable results in new 
; SCMs being create so any tagging of the results
; is not persisted the next time we try and
; walk the list.  We create persistant SCMs for
; each item here and tag that.
(define (split-varbinds result)
  (let nextvarbind ((thisvarbind result))
    (if (not (null? thisvarbind))
        (begin
          (change-class thisvarbind <report-varlist>)
          (slot-set! thisvarbind 'nextvar (slot-ref thisvarbind 'next-variable))
          (nextvarbind (slot-ref thisvarbind 'nextvar)))
        result)))

(define (filter-valid-next result initialoids)
  (let findnext ((thisres result)
                 (valids (list)))
    (if (null? thisres)
      ; We've colalted all valid results into valids. Re join them
      ; into one list
      (if (not (null? valids)) 
        (let ((first (car valids)))
          (let  joinup ((a valids))
            (if (not (null? a))
              (if (not (null? (cdr a)))
                (let ((b (cadr a)))
                  (slot-set! (car a) 'nextvar b)
                  (joinup (cdr a)))
                (slot-set! (car a) 'nextvar '())))
          first))
          ; if the valid list is empty, just return the empty list
          valids)
      (begin
        (cond
          ((equal? (char->integer (slot-ref thisres 'type)) (SNMP-ENDOFMIBVIEW)) 
             (display "Wandered off end of MIB"))
          ((equal? (char->integer (slot-ref thisres 'type)) (SNMP-NOSUCHOBJECT)) 
             (display "No Such Object"))
          ((equal? (char->integer (slot-ref thisres 'type)) (SNMP-NOSUCHINSTANCE)) 
             (display "No Such Instance"))
          ; if we were unable to establish a base during tagging we
          ; assume it was not an oid we requested
          ((null? (slot-ref thisres 'base)) #f)
          (#t (begin
               (set! valids (cons thisres valids)))))
        (findnext (slot-ref thisres 'nextvar) valids)))))
  
 
;; Given a varbind list and the oids requested that
;; generated it. Tag each reult with the based oid
;; and compute the iid.
;; We only match the first, but should try and
;; find the most specific match.
;;
(define (tag-varbinds result bases)
  (let nextvarbind ((varitem result)
                    (baseitems bases))
    (if (not (null? varitem))
      (begin
        (if (equal? (netsnmp-oid-is-subtree (car baseitems) (oid-from-varbind varitem)) 0)
          (begin
            (slot-set! varitem 'tag (car baseitems))
            (slot-set! varitem 'base (car baseitems))
            (slot-set! varitem 'iid (- (car baseitems) (oid-from-varbind varitem))))
          (begin
            ; fallback, if we get here (which we shouldn't!0
            ; set the iid and tags to gueeses
            (slot-set! varitem 'tag (oid-from-varbind varitem))
            (slot-set! varitem 'base '())
            (slot-set! varitem 'iid #u32())))
        (nextvarbind (slot-ref varitem 'nextvar) (cdr baseitems))))))

(define (make-varbind-func varbinds)
      (lambda( . msg)
        (if (not (null? varbinds))
          (cond
            ((eq? msg '()) (snprint-value (oid-from-varbind varbinds) varbinds))
            ((uniform-vector? (car msg))
              (let nextvarbind ((var varbinds))
                (if (null? var)
                  (display "No such oid in varbind")
                  (if (eq? (snmp-oid-compare (slot-ref var 'tag) (car msg)) 0)
                      (cond 
                        ((equal? (cdr msg) '())
                           (snprint-value var (oid-from-varbind var))) 
                        ((equal? (cdr msg) (list 'oid))     (oid-from-varbind var))
                        ((equal? (cdr msg) (list 'tag))     (slot-ref var 'tag))
                        ((equal? (cdr msg) (list 'iid))     (slot-ref var 'iid))
                        ((equal? (cdr msg) (list 'type))    (slot-ref var 'type))
                        ((equal? (cdr msg) (list 'varbind)) var)
                        ((equal? (cdr msg) (list 'value))   (snprint-value (oid-from-varbind var)))
                        (#t (snprint-value (oid-from-varbind var) var)))))))
            ((equal? (car msg) 'oid)     (oid-from-varbind varbinds))
            ((equal? (car msg) 'tag)     (slot-ref varbinds 'tag))
            ((equal? (car msg) 'iid)     (slot-ref varbinds 'iid))
            ((equal? (car msg) 'type)    (slot-ref varbinds 'type))
            ((equal? (car msg) 'nextvar) (slot-ref varbinds 'nextvar))
            ((equal? (car msg) 'varbind) varbinds )
            ((equal? (car msg) 'value)   (snprint-value (oid-from-varbind varbinds) varbinds))
            (#t                          (snprint-value (oid-from-varbind varbinds))))
          (failure-cont))))

	
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

;(define-syntax nextvar
;  (syntax-rules ()
;    ((nextvar varbind ) (make-varbind-func (varbind 'nextvar)))
;    ((nextvar varbind args ...) ((make-varbind-func (varbind 'nextvar)) args ...))))

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
           (split-varbinds results)
           (tag-varbinds results oids)
           (make-varbind-func results)))))))

(define-syntax getnext
  (syntax-rules ()
    ((get oid-terms ...)
      (let* ((oids (list oid-terms ...))
             (newpdu (snmp-pdu-create (SNMP-MSG-GETNEXT))))
       (for-each 
         (lambda(oid)
           (snmp-add-null-var newpdu oid)) 
         oids)
       (let ((status (snmp-synch-response current-session newpdu)))
         (let ((results (slot-ref status 'variables)))
           (split-varbinds results)
           (tag-varbinds results oids)
           (make-varbind-func results)))))))


; This is used to track our failure
(define failure-cont
  (lambda () (display "Options exhausted")(newline)#f))

(define-syntax all
  (syntax-rules ()
    ((all terms ...)
       (begin
         terms ...
         (failure-cont)))))
;;
;; With built in backtracking
(define (getnext-with-failure-cont baseoids)
  (let ((old-failure-cont failure-cont))
    (call/cc
      (lambda (continuation)
        (define (try initialoids pdu)
          (if (not (equal? initialoids '()))
            (let ((status (snmp-synch-response current-session pdu)))
              (if (not (equal? (slot-ref status 'errstat) (SNMP-ERR-NOERROR)))
                (begin
                  ;; we got to the end of the tree or failed somehow
                  (error "some error occurred")
                  (set! failure-cont old-failure-cont)
                  (failure-cont))
                ;; we succceeded. set up next set of oids and call try
                ;; again if we are deemed to have failed later on
                (let ((results (slot-ref status 'variables)))
                  (split-varbinds results)
                  (tag-varbinds results initialoids)
                  (let ((cleanresults (filter-valid-next results initialoids)))
                    (let ((nextpdu (snmp-pdu-create(SNMP-MSG-GETNEXT)))
                          (nextoids (list)))
                      (let add-oids ((varbind cleanresults))
                        (if (not (null? varbind))
                          (begin
                            (snmp-add-null-var nextpdu (oid-from-varbind varbind))
                            (set! nextoids (cons (slot-ref varbind 'base) nextoids))
                            (add-oids  (slot-ref varbind 'nextvar))))
                      (set! failure-cont
                        (lambda () (continuation (try nextoids nextpdu))))))
                    (make-varbind-func cleanresults)))))
            (begin 
              (set! failure-cont old-failure-cont)
              (failure-cont))))
        (let ((firstpdu (snmp-pdu-create(SNMP-MSG-GETNEXT))))
          (for-each 
            (lambda(oid)
              (snmp-add-null-var firstpdu oid)) 
            baseoids)
          (try baseoids firstpdu))))))

(define-syntax walk
  (syntax-rules ()
    ((walk oid-terms ...)
      (getnext-with-failure-cont (list oid-terms ...)))))
;
(define-syntax print
  (syntax-rules ()
    ((print var )
       (fprint-variable (current-output-port) (oid var) (var)))
    ((print var reqoid )
       (fprint-variable (current-output-port) (oid var reqoid) (var reqoid)))))

; Set up the reports environement
;
(init-snmp (car(command-line)))
(init-mib)
(set! reports:autotranslate #t)

(export current-session current-context reports:autotranslate <reports-varlist>)
(export-syntax init-reports session oid-list walk get getnext  ids sub-objid %)
(export-syntax oid tag iid type value nextvar print all)
(export failure-cont)
(export getnext-with-failure-cont make-varbind-func tag-varbinds split-varbinds)

(re-export-modules (oop goops) (ice-9 syncase) (snmp net-snmp))

