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
    get getnext nextvar all walk-on-fail walk-func 
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
        (let*((oidmodule (make-module 31 '() oid-lazy-binder))
              (snmpdupli (lambda(module name int1 val1 int2 val2 var val)
                                                         (if (equal? oidmodule
                                                                     int1)
                                                           (module-local-variable int2 name)
                                                           #f)))
              (dh (module-duplicates-handlers (current-module))))
          (module-define! duplicate-handlers 'snmpdupli snmpdupli) 
          (set-module-duplicates-handlers! (current-module)
                                           (append (list snmpdupli)
                                             (if (eq? dh #f)
                                               (default-duplicate-binding-procedures)
                                               dh)))
          (set-module-name! oidmodule "(snmp oidbinder)")
          (set-module-uses! (current-module) 
            (append (list oidmodule) (module-uses (current-module)))))))))

(define current-community (make-parameter "public"))
(define current-peername (make-parameter "localhost"))
(define current-version (make-parameter (SNMP-VERSION-2c)))
(define current-context (make-parameter #f))

(define (new-snmp-session)
  (let* ((bs (make <snmp-session>))
         (jn (snmp-sess-init bs)))
    (slot-set! bs 'version (current-version))
    (slot-set! bs 'peername (current-peername))
    (slot-set! bs 'community (current-community))
    (slot-set! bs 'community-len (string-length (current-community)))
    (snmp-sess-open bs)))

(define current-session (make-parameter (new-snmp-session)))

(define-syntax session
  (lambda(stx)
    (let ((args (cdr (syntax->datum stx)))
          (handler (lambda* (#:key (host #f)
                                   (community #f)
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
                        `(parameterize ((current-version   (if (eqv? ,version #f)
                                                             (current-version)
                                                             ,version))
                                        (current-peername  (if (eqv? ,host #f)
                                                             (current-peername)
                                                             ,host))
                                        (current-community (if (eqv? ,community #f)
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
      result)))

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

	
; These are convenience macros for accessing elements of a result
;
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


; Functions and macros for queries.
;
(define (synch-query querytype oids)
  (let ((newpdu (snmp-pdu-create querytype)))
    (for-each 
      (lambda(oid)
        (snmp-add-null-var newpdu oid)) 
         oids)
    (let ((status (snmp-sess-synch-response (current-session) newpdu)))
      (if (or
            (unspecified? status)
            (not (equal? (slot-ref status 'errstat) (SNMP-ERR-NOERROR))))
         (throw 'snmperror status)
         (let ((results (slot-ref status 'variables)))
           (split-varbinds results))))))

(define (get . oid-terms)
  (make-varbind-func 
    (tag-varbinds
      (synch-query (SNMP-MSG-GET) oid-terms)
      oid-terms)))

(define (getnext . oid-terms)
  (make-varbind-func 
    (tag-varbinds
      (synch-query (SNMP-MSG-GETNEXT) oid-terms)
      oid-terms)))

; This is the basic walk function and returns an iterator which returns a new element
; each time it is called.
(define (walk-func . baseoids)
  (let ((curroids    baseoids)
        (currbases   baseoids))
    (lambda()
      (if (not (equal? curroids '()))
        (let* ((results      (tag-varbinds
                               (synch-query (SNMP-MSG-GETNEXT) curroids)
                               currbases))
               (cleanresults (filter-valid-next results currbases)))
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
          (if (equal? cleanresults '())
            (throw 'walkend '())
            (make-varbind-func cleanresults)))
        (throw 'walkend '())))))

; This is the simplest walk to use, returning all results in a list
(define (walk . baseoids)
  (let ((vals (apply walk-func baseoids)))
    (let loop ((result '())
               (finished #f))
      (catch 'walkend
        (lambda() (set! result (append result (list (vals)))))
        (lambda(ex . args)
          (set! finished #t)))
      (if finished
        result
        (loop result finished)))))

; This is used to track our failures
(define fail
  (lambda () (lambda() #f)))

;; The is a walk that uses  built in backtracking. Any call to (fail) will result
;; in the next walk item being returned.
(define (walk-on-fail . baseoids)
  (let ((vals (apply walk-func baseoids))
        (old-fail fail))
    (call/cc
      (lambda (continuation)
        (define (try)
          (let ((result (catch 'walkend
                          (lambda() (vals))
                          (lambda(ex . args)
                            (set! fail old-fail)
                            (fail)))))
            (set! fail (lambda()(continuation (try))))
            result))
        (try)))))

; Version of get and getnext that can be used in conjuncion with walk-on-fail
(define (get-or-fail . oid-terms)
  (catch 'snmperror
    (lambda() (apply get oid-terms))
    (lambda(ex . args)
      (fail))))

(define (getnext-or-fail . oid-terms)
  (catch 'snmperror
    (lambda() (apply getnext oid-terms))
    (lambda(ex . args)
      (fail))))

; Allows including a manual list of optional values into the walk-on-fail backtracking
; mechanism
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

(define-syntax all
  (syntax-rules ()
    ((all terms ...)
       (begin
         terms ...
         (fail)))))


(re-export-modules (oop goops) (srfi srfi-39) (snmp net-snmp) (snmp oids))

; Set up the reports environement
;
(set! reports:autotranslate #t)
(init-mib)

