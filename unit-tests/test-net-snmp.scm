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

(define session (make <snmp-session>))
(snmp-sess-init session)
(slot-set! session 'version (SNMP-VERSION-2c))
(slot-set! session 'community "public")
(slot-set! session 'community-len 6)
(slot-set! session 'peername "localhost")
(define ss (snmp-open session))
(define pdu (snmp-pdu-create (SNMP-MSG-GET)))
(define oid (snmp-parse-oid "sysLocation.0"))
(define oid2(snmp-parse-oid "sysName.0"))
(snmp-add-null-var pdu oid)
(snmp-add-null-var pdu oid2)
(define status (snmp-synch-response ss pdu))
(define vals (slot-ref status 'variables))
(print-value oid  vals)
(print-value oid2  (slot-ref vals 'next-variable))

(snmp-free-pdu status)
(snmp-close ss)

(exit-with-summary (run-all-defined-test-cases))

