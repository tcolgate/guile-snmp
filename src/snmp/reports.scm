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

(define-generic oid)

(define-class <objid>()
    (ids #:init-value #u32() #:accessor ids))

(define-method (+ (id1 <objid>) (id2 <objid>))
  (display "add oids")(newline))

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
                  (let* ((obj (make <objid>))
                         (var (make-variable obj)))
                    (set! (ids obj) oid)
                    (module-add! mod sym var)
                    var)))
              #f))
        (define module (make-module 31 '() oid-lazy-binder))
        (set-module-uses! (current-module) 
          (append (module-uses (current-module)) (list module)))
        (init-mib)))))

(define current-session #f)
(define current-context 'ctx-snmpget)

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
        (list 
          statements ...)))))

(define-syntax get
  (syntax-rules ()
    ((get oid-terms ...)
      (let ((oids (`(oid-terms ...))))
        (let ((newpdu (snmp-pdu-create (SNMP-MSG-GET))))
         (for-each 
           (lambda(oids)
             (snmp-add-null-var newpdu oid)) 
           oids)
         (snmp-synch-response current-session newpdu))))))

(define-syntax walk
  (syntax-rules ()
    ((walk oid-terms ...)
      (let ((oids (`(oid-terms ...))))
;;	SETUP GENERATIVE LIST THING HERE
        (let ((newpdu (snmp-pdu-create (SNMP-MSG-GETNEXT))))
         (for-each 
           (lambda(oids)
             (snmp-add-null-var newpdu oid)) 
           oids)
         (snmp-synch-response current-session newpdu))))))
;;	SETUP FAILURE CONTINUATION HERa <---- if our failure con gets called we jump back here
;;	RUN GENERATIVE FUNCTION HERE <---- Generates first get-next

(export current-session current-context reports:autotranslate)
(export-syntax init-reports session oid-list walk get <objid> ids + )
