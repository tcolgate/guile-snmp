;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate 
;;
;; reports.scm - This file defines classes and utilities to provide
;; a more expressive environment for SNMP reporting
;;
;;-------------------------------------------------------------------

(define-module (snmp reports)
  #:use-module (srfi srfi-39)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (snmp net-snmp)
  #:use-module (snmp oids)
  #:export-syntax (session init-reports)
  #:export (
    base-session current-session old-session
    current-community current-port current-peername
    current-version current-context
    reports:autotranslate <reports-varlist> oid-list walk
    get getnext nextvar print all walk-on-fail walk-func 
    fail old-fail one-of iid oid type tag value 
    make-varbind-func tag-varbinds split-varbinds 
    filter-valid-next new-snmp-session))

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

(define reports:autotranslate #t)
(define-syntax init-reports
  (syntax-rules ()
    ((init-reports)
      (begin 
        (define (oid-lazy-binder mod sym def?)
          (if (and reports:autotranslate (not def?))
              (let ((oid (snmp-parse-oid (symbol->string sym))))
                (if (unspecified? oid)
                  #f
                  (let* ((var (make-variable oid)))
                    (module-add! mod sym var)
                    var)))
              #f))
        (define module (make-module 31 '() oid-lazy-binder))
        (set-module-uses! (current-module) 
          (append (list module) (module-uses (current-module))))))))

(define current-community (make-parameter "public"))
(define current-port (make-parameter 161))
(define current-peername (make-parameter "localhost"))
(define current-version (make-parameter (SNMP-VERSION-2c)))
(define current-context (make-parameter #f))

(define base-session (make <snmp-session>))
(snmp-sess-init base-session)

(define (new-snmp-session)
  (let* ((ns (snmp-sess-open base-session))
         (sp (snmp-sess-session ns)))
    (slot-set! sp 'version (current-version))
    (slot-set! sp 'peername (current-peername))
    (slot-set! sp 'remote-port (current-port))
    (slot-set! sp 'community (current-community))
    (slot-set! sp 'community-len (string-length (current-community)))
    ns))

(define current-session (make-parameter (new-snmp-session)))

(define-syntax session
  (lambda(stx)
    (let ((args (cdr (syntax->datum stx)))
          (handler (lambda* (#:key (host #f)
                                   (community #f)
                                   (port #f)
                                   (version #f)
                                   (context #f)
                             #:rest forms)
                     (let* ((remove-keys (lambda(ls)
                                           (let loop ((ls ls) (acc '()))
                                             (if (null? ls)
                                               (reverse acc)
                                               (let ((kw? (keyword? (car ls))))
                                                 (loop ((if kw? cddr cdr) ls)
                                                   (if kw? acc (cons (car ls) acc))))))))
                            (clean-forms (remove-keys forms)))
                       (datum->syntax stx
                        `(parameterize ((current-version   (if (eq? ,version #f)
                                                             (current-version)
                                                             ,version))
                                        (current-peername  (if (eq? ,host #f)
                                                             (current-peername)
                                                             ,host))
                                        (current-port      (if (eq? ,port #f)
                                                             (current-port)
                                                             ,port))
                                        (current-community (if (eq? ,community #f)
                                                             (current-community)
                                                             ,community)))
                           (parameterize ((current-session (new-snmp-session)))
                             (begin
                               ,@clean-forms))))))))
          (apply handler args))))


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
;; generated it. Tag each result with the based oid
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
            (slot-set! varitem 'iid (- (car baseitems) (oid-from-varbind varitem)))
            (slot-set! varitem 'base (car baseitems)))
          (begin
            ; fallback, if we get here (which we shouldn't!0
            ; set the iid and tags to gueeses
            (slot-set! varitem 'tag (oid-from-varbind varitem))
            (slot-set! varitem 'base '())
            (slot-set! varitem 'iid #u32())))
        (nextvarbind (slot-ref varitem 'nextvar) (cdr baseitems)))
      #t)))

(define (make-varbind-func varbinds)
      (lambda( . msg)
        (if (not (null? varbinds))
          (cond
            ((eq? msg '()) (slot-ref varbinds 'value))
            ((uniform-vector? (car msg))
              (let nextvarbind ((var varbinds))
                (if (null? var)
                  (begin
                    (display "No such oid returned in result set")(newline)
                    (fail))
                  (if (eq? (snmp-oid-compare (slot-ref var 'tag) (car msg)) 0)
                      (cond 
                        ((equal? (cdr msg) '())
                           (slot-ref var 'value))
                        ((equal? (cdr msg) (list 'oid))     (oid-from-varbind var))
                        ((equal? (cdr msg) (list 'tag))     (slot-ref var 'tag))
                        ((equal? (cdr msg) (list 'iid))     (slot-ref var 'iid))
                        ((equal? (cdr msg) (list 'type))    (slot-ref var 'type))
                        ((equal? (cdr msg) (list 'varbind)) var)
                        ((equal? (cdr msg) (list 'value))   (slot-ref var 'value))
                        (#t (slot-ref var 'value)))
                     (nextvarbind (slot-ref var 'nextvar))))))
            ((equal? (car msg) 'oid)     (oid-from-varbind varbinds))
            ((equal? (car msg) 'tag)     (slot-ref varbinds 'tag))
            ((equal? (car msg) 'iid)     (slot-ref varbinds 'iid))
            ((equal? (car msg) 'type)    (slot-ref varbinds 'type))
            ((equal? (car msg) 'nextvar) (slot-ref varbinds 'nextvar))
            ((equal? (car msg) 'varbind) varbinds )
            ((equal? (car msg) 'value)   (slot-ref varbinds 'value))
            (#t                          (slot-ref varbinds 'value)))
          (fail))))

	
(define-syntax oid
  (syntax-rules ()
    ((_ varbind args ...) (varbind args ... 'oid))))

(define-syntax tag
  (syntax-rules ()
    ((_ varbind args ...) (varbind args ... 'tag))))

(define-syntax iid
  (syntax-rules ()
    ((_ varbind args ...) (varbind args ... 'iid))))

(define-syntax index
  (syntax-rules ()
    ((_ varbind args ...) (varbind args ... 'index))))

(define-syntax type
  (syntax-rules ()
    ((_ varbind args ...) (varbind args ... 'type))))

(define-syntax value
  (syntax-rules ()
    ((_ varbind args ...) (varbind args ... 'value))))


;(define-syntax nextvar
;  (syntax-rules ()
;    ((nextvar varbind ) (make-varbind-func (varbind 'nextvar)))
;    ((nextvar varbind args ...) ((make-varbind-func (varbind 'nextvar)) args ...))))

(define (get . oid-terms)
      (let* ((oids oid-terms)
             (newpdu (snmp-pdu-create (SNMP-MSG-GET))))
       (for-each 
         (lambda(oid)
           (snmp-add-null-var newpdu oid)) 
         oids)
       (let ((status (snmp-sess-synch-response (current-session) newpdu)))
         (if (or
               (unspecified? status)
               (not (equal? (slot-ref status 'errstat) (SNMP-ERR-NOERROR))))
           (begin
             ;; we got to the end of the tree or failed somehow
             (fail))
           (let ((results (slot-ref status 'variables)))
             (split-varbinds results)
             (tag-varbinds results oids)
             (make-varbind-func results))))))

(define (getnext . oid-terms)
      (let* ((oids oid-terms)
             (newpdu (snmp-pdu-create (SNMP-MSG-GETNEXT))))
       (for-each 
         (lambda(oid)
           (snmp-add-null-var newpdu oid)) 
         oids)
       (let ((status (snmp-sess-synch-response (current-session) newpdu)))
         (if (or
               (unspecified? status)
               (not (equal? (slot-ref status 'errstat) (SNMP-ERR-NOERROR))))
           (begin
             ;; we got to the end of the tree or failed somehow
             (fail))
           (let ((results (slot-ref status 'variables)))
             (split-varbinds results)
             (tag-varbinds results oids)
             (make-varbind-func results))))))


; This is used to track our failure
(define fail
  (lambda () (display "Options exhausted")(newline)(exit)))

(define-syntax all
  (syntax-rules ()
    ((all terms ...)
       (begin
         terms ...
         (fail)))))
;;
;; With2 - simplfied version of walk without continuationsw
;;
(define (walk-func . baseoids)
  (let ((curroids    baseoids)
        (currbases   baseoids))
    (lambda()
      (if (not (equal? curroids '()))
        (let ((pdu (snmp-pdu-create(SNMP-MSG-GETNEXT))))
          (for-each 
            (lambda(oid)
              (snmp-add-null-var pdu oid)) 
            curroids)
          (let ((status (snmp-sess-synch-response (current-session) pdu)))
            (if (or
                  (unspecified? status)
                  (not (equal? (slot-ref status 'errstat) (SNMP-ERR-NOERROR))))
              (begin
                ;; we got to the end of the tree or failed somehow
                (fail))
              ;; we succceeded. set up next set of oids 
              (let ((results (slot-ref status 'variables)))
                (split-varbinds results)
                (tag-varbinds results currbases)
                (let ((cleanresults (filter-valid-next results currbases)))
                  (let ((nextoids  (list))
                        (nextbases (list)))
                    (let add-oids ((varbind cleanresults))
                      (if (not (null? varbind))
                        (begin
                          (set! nextoids (cons (oid-from-varbind varbind) nextoids))
                          (set! nextbases (cons (slot-ref varbind 'base) nextbases))
                          (add-oids  (slot-ref varbind 'nextvar)))))
                    (set! curroids nextoids)
                    (set! currbases nextbases))
                  (make-varbind-func cleanresults))))))
          (begin 
            (fail))))))

(define (one-of itemlist)
  (let ((old-fail2 fail))
    (call/cc
      (lambda (continuation)
        (define (try items)
          (if (null? items)
            (begin 
              (set! fail old-fail2)
              (fail))
            (begin
              (set! fail (lambda()(continuation (try (cdr items)))))
              (car items))))
        (try itemlist)))))

;;
;; With built in backtracking
(define (walk-on-fail baseoids)
  (let ((old-fail fail))
    (call/cc
      (lambda (continuation)
        (define (try initialoids pdu)
          (if (not (equal? initialoids '()))
            (let ((status (snmp-sess-synch-response (current-session) pdu)))
              (if (or
                    (unspecified? status)
                    (not (equal? (slot-ref status 'errstat) (SNMP-ERR-NOERROR))))
                (begin
                  ;; we got to the end of the tree or failed somehow
                  (set! fail old-fail)
                  (fail))
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
                      (set! fail
                        (lambda () (continuation (try nextoids nextpdu))))))
                    (make-varbind-func cleanresults)))))
            (begin 
              (set! fail old-fail)
              (fail))))
        (let ((firstpdu (snmp-pdu-create(SNMP-MSG-GETNEXT))))
          (for-each 
            (lambda(oid)
              (snmp-add-null-var firstpdu oid)) 
            baseoids)
          (try baseoids firstpdu))))))

(define-syntax walk
  (syntax-rules ()
    ((walk oid-terms ...)
      (walk-on-fail (list oid-terms ...)))))
;
(define-syntax print
  (syntax-rules ()
    ((print var )
       (fprint-variable (current-output-port) (oid var) (var)))
    ((print var reqoid )
       (fprint-variable (current-output-port) (oid var reqoid) (var reqoid)))))


(re-export-modules (oop goops) (srfi srfi-39) (snmp net-snmp) (snmp oids))

; Set up the reports environement
;
(set! reports:autotranslate #t)
(init-mib)

