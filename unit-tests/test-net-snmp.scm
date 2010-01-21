#!/usr/bin/guile -s
!#

;;-------------------------------------------------------------------
;; Copyright (C) 2009 Tristan Colgate 
;;
;; test.scm -  A simple example of the basic SNMP wrapper
;;
;;-------------------------------------------------------------------

(use-modules (snmp net-snmp)
             (unit-test)
             (oop goops))


(init-snmp "tester")
(init-mib)

(define-class <test-net-snmp> (<test-case>)
  (testsess #:accessor testsess)
  (oid-syslocation0 #:getter oid-syslocation0
               #:init-value #u32(1 3 6 1 2 1 1 6 0)))

(define-method (set-up-test (self <test-net-snmp>))
  (set! (testsess self) (make <snmp-session>))
  (snmp-sess-init (testsess self))
  (slot-set! (testsess self) 'version (SNMP-VERSION-2c))
  (slot-set! (testsess self) 'community "public")
  (slot-set! (testsess self) 'community-len 6)
  (slot-set! (testsess self) 'peername "localhost:10161"))

(define-method (test-oid-resolve-oid (self <test-net-snmp>))
  (assert-equal (oid-syslocation0 self)
                (snmp-parse-oid "sysLocation.0")))

(define-method (test-basic-get (self <test-net-snmp>))
  (let* ((ss  (snmp-open (testsess self)))
         (pdu (snmp-pdu-create (SNMP-MSG-GET))))
    (snmp-add-null-var pdu (oid-syslocation0 self))
    (let* ((status (snmp-synch-response ss pdu))
           (vals   (slot-ref status 'variables)))
      (assert-equal "Testing Guile-SNMP"
                    (slot-ref vals 'value))
      (snmp-free-pdu status)
      (snmp-close ss))))
      
(define-method (test-basic-set (self <test-net-snmp>))
  (let* ((ss  (snmp-open (testsess self)))
         (pdu (snmp-pdu-create (SNMP-MSG-SET))))
    (snmp-pdu-add-variable pdu (oid-syslocation0 self) (cons (integer->char (ASN-OCTET-STR)) "Testing Guile-SNMP"))
    (let* ((status (snmp-synch-response ss pdu)))
      (snmp-free-pdu status)
      (snmp-close ss))))

(exit-with-summary (run-all-defined-test-cases))

