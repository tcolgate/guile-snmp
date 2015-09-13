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

(define-class <test-net-snmp-v3> (<test-case>)
  (testsess #:accessor testsess)
  (oid-syslocation0 #:getter oid-syslocation0
               #:init-value (list->oid (list 1 3 6 1 2 1 1 6 0)))
  (oid-gstTestInt320 #:getter oid-gstTestInt320
               #:init-value (list->oid (list 1 3 6 1 3 1977 1 1 0)))
  (oid-gstTestString0 #:getter oid-gstTestString0
               #:init-value (list->oid (list 1 3 6 1 3 1977 1 2 0)))
  (oid-gstTestInt640 #:getter oid-gstTestInt640
               #:init-value (list->oid (list 1 3 6 1 3 1977 1 3 0))))

(define-method (set-up-test (self <test-net-snmp-v3>))
  (set! (testsess self) (make <snmp-session>))
  (slot-set! (testsess self) 'peername "localhost:10161")
  (slot-set! (testsess self) 'version SNMP-VERSION-3)
  (slot-set! (testsess self) 'securityLevel SNMP-SEC-LEVEL-AUTHNOPRIV)
  (slot-set! (testsess self) 'securityName "gsttest")
  (slot-set! (testsess self) 'securityAuthProto (snmp-parse-oid "usmHMACMD5AuthProtocol"))
  (slot-set! (testsess self) 'securityAuthKey "Top_Secret_v3_user_password")
  (slot-set! (testsess self) 'securityPrivProto (snmp-parse-oid "usmDESPrivProtocol"))
  (slot-set! (testsess self) 'securityPrivKey "Top_Secret_v3_user_password"))

(define-method (test-oid-resolve-oid (self <test-net-snmp-v3>))
  (assert-equal (oid-syslocation0 self)
                (snmp-parse-oid "sysLocation.0")))

(define-method (test-basic-get-int32 (self <test-net-snmp-v3>))
  (let* ((ss  (snmp-sess-open (testsess self)))
         (pdu (snmp-pdu-create SNMP-MSG-GET)))
    (snmp-add-null-var pdu (oid-gstTestInt320 self))
    (let* ((status (snmp-sess-synch-response ss pdu))
           (vals   (slot-ref status 'variables)))
      (assert-equal 499
                    (slot-ref (car vals) 'value))
      (snmp-free-pdu status)
      (snmp-sess-close ss))))

(define-method (test-basic-get-octetstr (self <test-net-snmp-v3>))
  (let* ((ss  (snmp-sess-open (testsess self)))
         (pdu (snmp-pdu-create SNMP-MSG-GET)))
    (snmp-add-null-var pdu (oid-gstTestString0 self))
    (let* ((status (snmp-sess-synch-response ss pdu))
           (vals   (slot-ref status 'variables)))
      (assert-equal "Guile-SNMP test string"
                    (slot-ref (car vals) 'value))
      (snmp-free-pdu status)
      (snmp-sess-close ss))))

(define-method (test-basic-get-counter64 (self <test-net-snmp-v3>))
  (let* ((ss  (snmp-sess-open (testsess self)))
         (pdu (snmp-pdu-create SNMP-MSG-GET)))
    (snmp-add-null-var pdu (oid-gstTestInt640 self))
    (let* ((status (snmp-sess-synch-response ss pdu))
           (vals   (slot-ref status 'variables)))
      (assert-equal 1499
                    (slot-ref (car vals) 'value))
      (snmp-free-pdu status)
      (snmp-sess-close ss))))

(define-method (test-basic-set (self <test-net-snmp-v3>))
  (let* ((ss  (snmp-sess-open (testsess self)))
         (pdu (snmp-pdu-create SNMP-MSG-SET)))
    (snmp-add-var pdu (oid-syslocation0 self) (cons ASN-OCTET-STR "Testing Guile-SNMP"))
    (let* ((status (snmp-sess-synch-response ss pdu)))
      (snmp-free-pdu status)
      (snmp-sess-close ss))))

(exit-with-summary (run-all-defined-test-cases))

