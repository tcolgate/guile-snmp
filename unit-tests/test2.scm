#!/usr/bin/guile -s
!#

;;-------------------------------------------------------------------
;; Copyright (C) 2009 Tristan Colgate 
;;
;; test2.scm -  A siple test of the net-snmp bindings
;;
;;-------------------------------------------------------------------

(use-modules (snmp net-snmp))
(use-modules (snmp reports))
(use-modules (oop goops))

(init-snmp "tester")
(init-mib)
(init-reports)

(set! reports:autotranslate #t)


(define session (make <snmp-session>))
(snmp-sess-init session)
(slot-set! session 'version (SNMP-VERSION-2c))
(slot-set! session 'community "public")
(slot-set! session 'community-len 6)
(slot-set! session 'peername "localhost")
(define ss (snmp-open session))
(define pdu (snmp-pdu-create (SNMP-MSG-GET)))
(snmp-add-null-var pdu sysLocation.0)
(snmp-add-null-var pdu sysName.0)
(define status (snmp-synch-response ss pdu))
(define vals (slot-ref status 'variables))
(display (snprint-value sysLocation.0  vals))
(display (snprint-value sysName.0  (slot-ref vals 'next-variable)))

(snmp-free-pdu status)
(snmp-close ss)

