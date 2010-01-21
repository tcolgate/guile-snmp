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
  #:use-module (snmp reports session)
  #:use-module (snmp reports cache)
  #:export-syntax (init-reports set build-one build-set)
  #:export (
    reports:autotranslate <reports-varlist> oid-list walk
    get getnext nextvar all walk-on-fail walk-func 
    fail old-fail one-of iid oid type tag value 
    make-varbind-func tag-varbinds split-varbinds 
    filter-valid-next reach-each)
  #:re-export (
    current-session
    current-community
    current-port
    current-host
    current-version
    current-context
    new-snmp-session
    enable-query-cache
    disable-query-cache
    query-cache-enabled
    dump-query-cache))

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
;; generated it. return a copy with each result tagged
;; with the based oid and compute the iid.
;; We only match the first, but should try and
;; find the most specific match.
;; Currently assumes an ordered list
;;
(define (tag-varbinds data bases)
  (let ((result (copy-tree data)))
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
    result))

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
                (#t (slot-ref (cdr var) 'value))))))
        ((equal? (car msg) 'oidlist) (map car varbinds))
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

; Functions and macros for queries.

; Perform an synchronous SNMP query
(define (synch-query querytype oids)
  (let ((crs (list))  ; records retrived from cache
        (cms (list))) ; records not in cache
    (for-each 
      (lambda(oid)
        (let ((cr (if (query-cache-enabled)
                    (query-cache-lookup querytype oid)
                    #f)))
          (if (not cr)
            ; Cache miss
            (set! cms (append cms (list oid)))
            ; Cache hist
            (set! crs (append crs cr)))))
      oids)
    (let ((qrs (if (equal? cms '())
                 '() ; Don't do anything if we have nothing to do
                 (let ((newpdu (snmp-pdu-create querytype)))
                   (let addoids ((qs cms))
                     (if (not (eq? qs '()))
                       (begin
                         (snmp-add-null-var newpdu (car qs))
                         (addoids (cdr qs)))))
                   (let ((status (snmp-sess-synch-response (current-session) newpdu)))
                     (if (or
                           (unspecified? status)
                           (not (equal? (slot-ref status 'errstat) (SNMP-ERR-NOERROR))))
                      (throw 'snmperror (snmp-sess-error (current-session)))
                      (let ((results (slot-ref status 'variables)))
                        (split-varbinds results))))))))
       (if (query-cache-enabled)
         (for-each 
           (lambda(cm qr)
             (query-cache-insert querytype cm qr))
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

(define (synch-set oid-value-pairs)
  (if (equal? oid-value-pairs '())
    #t
    (let ((newpdu (snmp-pdu-create (SNMP-MSG-SET))))
      (let addoids ((sos oid-value-pairs))
        (if (not (eq? sos '()))
          (let ((so (car sos)))
            (if (not (pair? so))
              (throw 'snmperror "Invalid set")
              (let ((var     (car so))
                    (valspec (cdr so)))
                (if (pair? valspec)
                  ; (type value)
                  (begin 
                    (let* ((ty       (car valspec))
                           (tychar   (if (char? ty)
                                       ty
                                       (integer->char ty)))
                           (val      (cdr valspec)))
                      (snmp-pdu-add-variable newpdu 
                                             var 
                                             (cons tychar val)))
                    (addoids (cdr sos)))
                  ; (value) infer type from oid
                  (let ((type (mib-to-asn-type (get-oid-type var))))
                    ; need to lob an exception if we don't get an type back
                    (if (equal? 255 (char->integer type))
                      (throw 'snmperror "Could not determine type in set")
                      (addoids (append (list (cons var (cons type valspec)))
                                       (cdr sos)))))))))))
      (let* ((response (snmp-sess-synch-response (current-session) newpdu))
             (status   (slot-ref response 'errstat)))
        (if (not (equal? status (SNMP-ERR-NOERROR)))
          (throw 'snmperror (snmp-sess-error (current-session)))
          #t)))))

(define-syntax set
  (syntax-rules ()
    ((set (name1 val1)  ... )
     (synch-set (build-set (name1 val1) ... )))))

(define-syntax build-set
  (syntax-rules ()
    ((build-set (name1 val1))
     (list (build-one name1 val1)))
    ((build-set (name1 val1) (name2 val2) ...)
     (append (list (build-one name1 val1))
             (build-set (name2 val2) ...)))))

(define-syntax build-one
  (syntax-rules ()
    ((build-one name1 (typ1 val1))
     (cons name1 (cons typ1 val1)))
    ((build-one name1 val1)
     (cons name1 val1))))

; This is the basic walk function and returns an iterator which returns a new element
; each time it is called.
(define (walk-func baseoid)
  (let ((curroid   baseoid)
        (currbase  baseoid))
    (lambda()
      (if (not (equal? curroid #f))
        (let* ((results      (tag-varbinds
                               (synch-query (SNMP-MSG-GETNEXT) (list curroid))
                               (list currbase)))
               (cleanresults (filter-valid-next results)))
          (if (equal? cleanresults '())
            (throw 'walkend '())
            (begin
              (set! curroid (slot-ref (cdr (car cleanresults)) 'oid))
              (set! currbase (slot-ref (cdr (car cleanresults)) 'base))
              (make-varbind-func cleanresults))))
        (throw 'walkend '())))))

; This is the simplest walk to use, returning all results in a list
(define (walk baseoid)
  (let ((val  (walk-func baseoid)))
    (let loop ((result '())
               (finished #f))
      (catch 'walkend
        (lambda() (set! result (append result (list (val)))))
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

(re-export-modules (oop goops) (srfi srfi-39) (snmp reports session) (snmp net-snmp) (snmp oids))

; Set up the reports environement
;
(set! reports:autotranslate #t)
(init-mib)

