(use-modules (snmp reports)
             (srfi srfi-1)
             (unit-test)
             (oop goops))

(init-reports)

(define-class <test-reports> (<test-case>))
  
(define-method (test-get (self <test-reports>))
  (assert-equal "Testing Guile SNMP"
                (session #:peer "127.0.0.1:10161" 
                  ((get (snmp-parse-oid "sysLocation.0"))))))

(exit-with-summary (run-all-defined-test-cases))

