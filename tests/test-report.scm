;;-------------------------------------------------------------------
;; Copyright (C) 2009 Tristan Colgate 
;;
;; test-report.scm -  A simple report testing various features of the
;; reporting system.
;;
;;-------------------------------------------------------------------

(use-modules (oop goops))
(use-modules (snmp net-snmp))
(use-modules (snmp reports))
(init-snmp (car(command-line)))
(init-mib)
(init-reports)
(set! reports:autotranslate #t)

(define reply (session "localhost" "public" (get sysName.0 sysDescr.0)))
(for-each 
  (lambda(res)(print-value sysLocation.0 (slot-ref res 'variables )))
  reply)



