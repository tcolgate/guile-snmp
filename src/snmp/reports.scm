;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate 
;;
;; reports.scm - This file defines classes and utilities to provide
;; a more expressive environment for SNMP reporting
;;
;;-------------------------------------------------------------------

(define-module (snmp reports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-18)
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
    filter-valid-next new-snmp-session reach-each
    cache-key query-cache-enabled report-query-cache))

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

; This class is used to represent answers. We use our own dedicated class
; to allow free'ing of results, avoid link lists and better allow cacheing
(define-class <report-varlist> ()
  (oid #:init-value #u32())
  (type #:init-value #f)
  (value #:init-value #f)
  (tag #:init-value #u32())
  (base #:init-value #u32())
  (iid #:init-value #u32())
  (rawvarbind #:init-value #f))

; split the returned list of varbinds into an associated list of variables
(define (split-varbinds input)
  (let nextvarbind ((thisvarbind input)
                    (result (list)))
    (if (not (null? thisvarbind))
          (let ((newvar (make <report-varlist>)))
            (slot-set! newvar 'rawvarbind thisvarbind)
            (slot-set! newvar 'oid (oid-from-varbind thisvarbind))
            (slot-set! newvar 'type (slot-ref thisvarbind 'type))
            (slot-set! newvar 'value (slot-ref thisvarbind 'value))
            (set! result (acons (slot-ref newvar 'oid)  newvar result))
            (nextvarbind (slot-ref thisvarbind 'next-variable) result))
        result)))

(define (filter-valid-next results)
  (filter
    (lambda(thisres)   
      (cond
        ((equal? (char->integer (slot-ref (cdr thisres) 'type)) (SNMP-ENDOFMIBVIEW)) 
          #f)
        ((equal? (char->integer (slot-ref (cdr thisres) 'type)) (SNMP-NOSUCHOBJECT)) 
          #f)
        ((equal? (char->integer (slot-ref (cdr thisres) 'type)) (SNMP-NOSUCHINSTANCE)) 
          #f)
        ((null? (slot-ref (cdr thisres) 'base)) 
          #f)
        (#t #t)))
    results))
 
;; Given a varbind list and the oids requested that
;; generated it. Tag each result with the based oid
;; and compute the iid.
;; We only match the first, but should try and
;; find the most specific match.
;;
(define (tag-varbinds result bases)
  (for-each
    (lambda(varitem baseitem)
      (if (equal? (netsnmp-oid-is-subtree baseitem (slot-ref (cdr varitem) 'oid)) 0)
        (begin
          (slot-set! (cdr varitem) 'tag baseitem)
          (slot-set! (cdr varitem) 'iid (- baseitem (slot-ref (cdr varitem) 'oid)))
          (slot-set! (cdr varitem) 'base baseitem))
        (begin
          ; fallback, if we get here (which we shouldn't!0
          ; set the iid and tags to gueeses
          (slot-set! (cdr varitem) 'tag (slot-ref (cdr varitem) 'oid))
          (slot-set! (cdr varitem) 'base '())
          (slot-set! (cdr varitem) 'iid #u32()))))
    result bases)
  result)

(define (make-varbind-func varbinds)
  (lambda( . msg)
    (if (not (null? varbinds))
      (cond
        ((eq? msg '()) (slot-ref (cdr (car varbinds)) 'value))
        ((uniform-vector? (car msg))
          (let ((var (assoc (car msg) varbinds)))
            (if (not var)
              (begin
                (format #t "No such oid in result set~%") #f)
              (cond 
                ((equal? (cdr msg) '()) (slot-ref (cdr var) 'value))
                ((equal? (cdr msg) (list 'oid))     (car var))
                ((equal? (cdr msg) (list 'tag))     (slot-ref (cdr var) 'tag))
                ((equal? (cdr msg) (list 'iid))     (slot-ref (cdr var) 'iid))
                ((equal? (cdr msg) (list 'type))    (slot-ref (cdr var) 'type))
                ((equal? (cdr msg) (list 'varbind)) (slot-ref (cdr var) 'rawvarbind))
                ((equal? (cdr msg) (list 'value))   (slot-ref (cdr var) 'value))
                (#t (slot-ref var 'value))))))
        ((equal? (car msg) 'oid)     (car (car varbinds)))
        ((equal? (car msg) 'tag)     (slot-ref (cdr (car varbinds)) 'tag))
        ((equal? (car msg) 'iid)     (slot-ref (cdr (car varbinds)) 'iid))
        ((equal? (car msg) 'type)    (slot-ref (cdr (car varbinds)) 'type))
        ((equal? (car msg) 'varbind) (slot-ref (cdr (car varbinds)) 'rawvarbind))
        ((equal? (car msg) 'value)   (slot-ref (cdr (car varbinds)) 'value))
        (#t                          (slot-ref (cdr (car varbinds)) 'value)))
      #f)))

	
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


; Cache functions

(define (cache-key req oid)
  (with-output-to-string
    (lambda ()
      (format #t "~a!~a!~a!~a!~a!~a"
        req
        (current-peername)
        (current-version)
        (current-community)
        (current-context)
        oid))))

; Functions and macros for queries.
;
(define query-cache-enabled (make-parameter #t)) 
(define report-query-cache  (list))
(define (synch-query querytype oids)
  (let ((crs (list))  ; records retrived from cache
        (cms (list))) ; records not in cache
    (for-each 
      (lambda(oid)
        (let ((cr (assoc (cache-key querytype oid) report-query-cache string=? )))
          (if (not cr)
            ; Cache miss
            (set! cms (append cms (list oid)))
            ; Cache hist
            (set! crs (append crs (list cr))))))
      oids)
    (let ((qrs (let ((newpdu (snmp-pdu-create querytype)))
                 (let addoids ((qs cms))
                   (if (not (eq? qs '()))
                     (begin
                       (snmp-add-null-var newpdu (car qs))
                       (addoids (cdr qs)))))
                 (let ((status (snmp-sess-synch-response (current-session) newpdu)))
                   (if (or
                         (unspecified? status)
                         (not (equal? (slot-ref status 'errstat) (SNMP-ERR-NOERROR))))
                    (throw 'snmperror status)
                    (let ((results (slot-ref status 'variables)))
                      (split-varbinds results)))))))
       (if (query-cache-enabled)
         (for-each 
           (lambda(cr ce)
             (set! report-query-cache (acons
                                        (cache-key querytype cr)
                                        (cdr ce)
                                        report-query-cache)))
           (reverse cms) qrs))
       (append crs qrs))))

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
               (cleanresults (filter-valid-next results)))
          (let ((nextoids  (list))
                (nextbases (list)))
            (for-each 
              (lambda(varbind)
                (set! nextoids (cons (slot-ref (cdr varbind) 'oid) nextoids))
                (set! nextbases (cons (slot-ref (cdr varbind) 'base) nextbases)))
              cleanresults)
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

; Calls the provided function in paralell with each argument and
; returns the answer from the first thread to return.
(define-syntax race-each
  (syntax-rules ()
  ((_ func parargs)
   (let* ((result #f)
          (result-ready (make-condition-variable))
          (junktex (make-mutex))
          (junk    (mutex-lock! junktex))
          (resulttex (make-mutex))
          (dotask (lambda(arg)
                     (let ((thisresult (session (func arg))))
                       (with-exception-handler
                         (lambda(ev)
                           (thread-terminate! (current-thread)))
                         (lambda() (mutex-lock! resulttex)))
                       (set! result thisresult)
                       (condition-variable-signal! result-ready)
                       (thread-terminate! (current-thread)))))
          (threads (map (lambda(x)
                          (thread-start! (make-thread (lambda()
                                                         (dotask x))))) parargs)))
      (mutex-unlock! junktex result-ready)
      (map (lambda(old-thread) (thread-terminate! old-thread)) threads)
      result))))

(re-export-modules (oop goops) (srfi srfi-39) (snmp net-snmp) (snmp oids))

; Set up the reports environement
;
(set! reports:autotranslate #t)
(init-mib)

