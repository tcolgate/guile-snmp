;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate 
;;
;; reports.scm - This file defines classes and utilities to provide
;; a more expressive environment for SNMP reporting
;;
;;-------------------------------------------------------------------

(define-module (snmp reports)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-18)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 format)
  #:use-module (snmp net-snmp)
  #:use-module (snmp oids)
  #:use-module (snmp reports session)
  #:use-module (snmp reports cache)
  #:export-syntax (init-reports set build-one build-set)
;  #:re-export-syntax (session default-session)
  #:export (
    use-mibs
    reports:autotranslate <snmp-reports-result> oid-list walk bulk-walk
    get getnext getbulk get-or-fail nextvar all walk-on-fail walk-func 
    bulk-walk-func results
    set set-or-fail
    fail old-fail one-of iid oid type tag value rawvarbind
    make-varbind-func tag-varbinds split-varbinds 
    filter-valid-next reach-each
    debug-reports)
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
    dump-query-cache
    for-each
    assoc
    map
    format))

(define debug-reports (make-parameter #f))

(define-syntax use-mibs
  (syntax-rules ()
    ((_ names ...)
     (eval-when (eval load compile)
       (for-each 
          (lambda(mib)
            (read-module (symbol->string mib)))
          '(names  ...))))))

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
                                                           (module-variable int2 name)
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
(define-class <snmp-reports-result> ()
  (oid #:init-value empty-oidvec #:getter oid)
  (type #:init-value #f #:getter type)
  (value #:init-value #f #:getter value)
  (tag #:init-value empty-oidvec #:getter tag)
  (base #:init-value empty-oidvec)
  (iid #:init-value empty-oidvec #:getter iid)
  (rawvarbind #:init-value #f #:getter rawvarbind))

(define-method (display (this <snmp-reports-result>) port)
  (format port "#<snmp-reports-result>"))

(define-method (write (this <snmp-reports-result>) port)
  (format port "#<snmp-reports-result>#"))

(define-method  (oid (var <snmp-reports-result>))
  (slot-ref var 'oid))
(define-method  (type (var <snmp-reports-result>))
  (slot-ref var 'type))
(define-method  (value (var <snmp-reports-result>))
  (slot-ref var 'value))
(define-method  (tag (var <snmp-reports-result>))
  (slot-ref var 'tag))
(define-method  (iid (var <snmp-reports-result>))
  (slot-ref var 'iid))
(define-method  (rawvarbind (var <snmp-reports-result>))
  (slot-ref var 'rawvarbind))

(define-class <snmp-reports-result-set> (<applicable-struct>)
  (results #:init-value '() #:init-keyword #:results #:accessor results))

(define-method (initialize (result-set <snmp-reports-result-set>) initargs)
  (let ((results (get-keyword #:results initargs '())))
    (next-method)
    (slot-set!  result-set 'procedure
      (lambda( . msg)
        (if (not (null? results))
          (cond
            ((eq? msg '()) (slot-ref (cdr (car results)) 'value))
            ((eq? <oid> (class-of (car msg)))
              (let ((var (assoc (car msg) results)))
                (if (not var)
                  (begin
                    (format #t "No such oid in result set~%") #f)
                  (cond 
                    ((equal? (cdr msg) '()) (slot-ref (cdr var) 'value))
                    ((equal? (cdr msg) (list 'oid))     (car var))
                    ((equal? (cdr msg) (list 'tag))     (slot-ref (cdr var) 'tag))
                    ((equal? (cdr msg) (list 'iid))     (slot-ref (cdr var) 'iid))
                    ((equal? (cdr msg) (list 'type))    (slot-ref (cdr var) 'type))
                    ((equal? (cdr msg) (list 'rawvarbind)) (slot-ref (cdr var) 'rawvarbind))
                    ((equal? (cdr msg) (list 'value))   (slot-ref (cdr var) 'value))
                    (#t (slot-ref (cdr var) 'value))))))
            ((equal? (car msg) 'oidlist) (map car results))
            ((equal? (car msg) 'oid)     (car (car results)))
            ((equal? (car msg) 'tag)     (slot-ref (cdr (car results)) 'tag))
            ((equal? (car msg) 'iid)     (slot-ref (cdr (car results)) 'iid))
            ((equal? (car msg) 'type)    (slot-ref (cdr (car results)) 'type))
            ((equal? (car msg) 'rawvarbind) (slot-ref (cdr (car results)) 'rawvarbind))
            ((equal? (car msg) 'value)   (slot-ref (cdr (car results)) 'value))
            (#t                          (slot-ref (cdr (car results)) 'value)))
          #f)))))

(define-method (display (this <snmp-reports-result-set>) port)
  (format port "#<snmp-reports-result-set ~a: ~s ~@[~a~]>#" (this 'oid) (this 'value) (if (> (length (this 'oidlist)) 1) "..." #f)))

(define-method (write (this <snmp-reports-result-set>) port)
  (format port "#<snmp-reports-result-set ~a: ~s ~@[~a~]>#" (this 'oid) (this 'value) (if (> (length (this 'oidlist)) 1) "..." #f)))

; Functions and macros for queries.

(define-method  (oid (varset <snmp-reports-result-set>))
  (varset 'oid))
(define-method  (oid (varset <snmp-reports-result-set>) (item <oid>))
  (varset item 'oid))

(define-method  (type (varset <snmp-reports-result-set>))
  (varset 'type))
(define-method  (type (varset <snmp-reports-result-set>) (item <oid>))
  (varset item 'type))

(define-method  (value (varset <snmp-reports-result-set>))
  (varset  'value))
(define-method  (value (varset <snmp-reports-result-set>) (item <oid>))
  (varset item 'value))

(define-method  (tag (varset <snmp-reports-result-set>))
  (varset 'tag))
(define-method  (tag (varset <snmp-reports-result-set>) (item <oid>))
  (varset item 'tag))

(define-method  (iid (varset <snmp-reports-result-set>))
  (varset 'iid))
(define-method  (iid (varset <snmp-reports-result-set>) (item <oid>))
  (varset item 'iid))

(define-method  (rawvarbind (varset <snmp-reports-result-set>))
  (varset 'rawvarbind))
(define-method  (rawvarbind (varset <snmp-reports-result-set>) (item <oid>))
  (varset item 'rawvarbind))

; split the returned list of varbinds into an associated list of variables
(define (split-varbinds input)
  (map
    (lambda (thisvarbind)
      (let ((newvar (make <snmp-reports-result>)))
        (slot-set! newvar 'rawvarbind thisvarbind)
        (slot-set! newvar 'oid (slot-ref thisvarbind 'name))
        (slot-set! newvar 'type (slot-ref thisvarbind 'type))
	(slot-set! newvar 'value (slot-ref thisvarbind 'value))
        (cons (slot-ref newvar 'oid)  newvar)))
    input))

(define-generic filter-valid-next)

(define-method (filter-valid-next (results <snmp-reports-result-set>))
  (make <snmp-reports-result-set> #:results (filter
    (lambda(thisres)   
      (cond
        ((equal? (slot-ref (cdr thisres) 'type) SNMP-ENDOFMIBVIEW)
          #f)
        ((equal? (slot-ref (cdr thisres) 'type) SNMP-NOSUCHOBJECT) 
          #f)
        ((equal? (slot-ref (cdr thisres) 'type) SNMP-NOSUCHINSTANCE) 
          #f)
        ((null? (slot-ref (cdr thisres) 'base)) 
          #f)
        (#t #t)))
    (slot-ref results 'results))))

(define-method (filter-valid-next any)
  ; Dump some error
  )
 
;; Given a varbind list and the oids requested that
;; generated it. return a copy with each result tagged
;; with the based oid and compute the iid.
;; We only match the first, but should try and
;; find the most specific match.
;; Currently assumes an ordered list
;;
(define-generic tag-varbinds)
(define-method (tag-varbinds (data <snmp-reports-result-set>) bases)
  (let ((result (copy-tree (slot-ref data 'results))))
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
            (slot-set! (cdr varitem) 'iid empty-oidvec))))
      result bases)
    (make <snmp-reports-result-set> #:results result)))

	

; Perform an synchronous SNMP query
(define (synch-query querytype oids)
  (if (debug-reports) (format (current-error-port) 
                               "Attempting to perform a ~a for ~a from host ~a using ~a ~%"
                              querytype
                              oids
                              (current-host)
                              (current-community))) 
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
		   (if (eq? SNMP-MSG-GETBULK querytype)
		     (begin 
		       (set! (non-repeaters newpdu) 0)
		       (set! (max-repetitions newpdu) 10)))
                   (let addoids ((qs cms))
                     (if (not (eq? qs '()))
                       (begin
                         (snmp-add-null-var newpdu (car qs))
                         (addoids (cdr qs)))))
                   (let ((response (snmp-sess-synch-response (current-session) newpdu)))
                     (if (not (equal?  (class-of response) <pdu>)) 
                      (throw 'snmperror (snmp-sess-error (current-session)))
                      (let ((results (slot-ref response 'variables)))
                        (split-varbinds results))))))))
       (if (query-cache-enabled)
         (for-each 
           (lambda(cm qr)
             (query-cache-insert querytype cm qr))
           (reverse cms) qrs))
       (make <snmp-reports-result-set> #:results (append crs qrs)))))

(define (get . oid-terms)
  (tag-varbinds
    (synch-query SNMP-MSG-GET oid-terms)
    oid-terms))

(define (getnext . oid-terms)
  (tag-varbinds
    (synch-query SNMP-MSG-GETNEXT oid-terms)
    oid-terms))

(define (getbulk nrs rs . oid-terms)
  (tag-varbinds
    (synch-query SNMP-MSG-GETBULK oid-terms)
    oid-terms))

(define (synch-set oid-value-pairs)
  (if (equal? oid-value-pairs '())
    #t
    (let ((newpdu (snmp-pdu-create SNMP-MSG-SET)))
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
                           (val      (cdr valspec)))
                      (snmp-add-var newpdu 
                                    var 
                                    (cons ty val)))
                    (addoids (cdr sos)))
                  ; (value) infer type from oid
                  (let ((type (mib-to-asn-type (get-oid-type var))))
                    ; need to lob an exception if we don't get an type back
                    (if (not  (equal? (class-of type) <asn-type>)) 
                      (throw 'snmperror "Could not determine type in set")
                      (addoids (append (list (cons var (cons type valspec)))
                                       (cdr sos)))))))))))
      (let* ((response (snmp-sess-synch-response (current-session) newpdu)))
        (if (not (equal?  (class-of response) <pdu>))
          (throw 'snmperror "error during set")
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
                               (synch-query SNMP-MSG-GETNEXT (list curroid))
                               (list currbase)))
               (cleanresults (filter-valid-next results)))
          (if (equal? (slot-ref  cleanresults 'results) '())
            (throw 'walkend '())
            (begin
              (set! curroid (slot-ref (cdr (car (slot-ref cleanresults 'results))) 'oid))
              (set! currbase (slot-ref (cdr (car (slot-ref cleanresults 'results))) 'base))
              cleanresults)))
        (throw 'walkend '())))))

; This is the basic walk function and returns an iterator which returns a new element
; each time it is called.
; TODO: This needs collapsing into walk as an option
(define (bulk-walk-func baseoid)
  (let ((curroid   baseoid)
        (currbase  baseoid)
	(prevres  '()))
    (lambda()
      (if (equal? '() prevres)
	(if (not (equal? curroid #f))
	  (let* ((results      (synch-query SNMP-MSG-GETBULK(list curroid)))
		 (tresults     (tag-varbinds
				 results 
				 (make-list (length (slot-ref results 'results)) baseoid)))
		 (cleanresults (filter-valid-next results)))
	    (if (equal? (slot-ref  cleanresults 'results) '())
	      (throw 'walkend '())
	      (begin
		(set! curroid (slot-ref (cdr (car (slot-ref cleanresults 'results))) 'oid))
		(set! currbase (slot-ref (cdr (car (slot-ref cleanresults 'results))) 'base))
		; We want to divide this individual result set into lots of seperate ones
		; we can dish out one by one
		(let ((supset (map 
				(lambda (r)
				  (make <snmp-reports-result-set> #:results (list r)))
				(slot-ref  cleanresults 'results))))
		  (set! prevres (cdr supset))
		  (car supset)))))
	  (throw 'walkend '()))
	(let ((rem (cdr prevres))
	      (val (car prevres)))
	   (set! prevres rem)
	   (set! curroid (slot-ref (cdr (car (slot-ref val 'results))) 'oid))
	   (set! currbase (slot-ref (cdr (car (slot-ref val 'results))) 'base))
	   val)))))

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

; This is the simplest bulk walk to use, returning all results in a list
; TODO: This needs collapsing into walk as an option
(define (bulk-walk baseoid)
  (let ((val  (bulk-walk-func baseoid)))
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
  (lambda () (lambda( . args) (throw 'noMoreAlternatives "No more alternative paths"))))

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
              (lambda(. argv)(car items)))))
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


; Set up the reports environement
;
(set! reports:autotranslate #t)
(eval-when (eval load compile)
  (init-mib))

