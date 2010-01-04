(use-modules (snmp reports)
             (srfi srfi-1)
             (unit-test)
             (oop goops))

(use-modules (ice-9 format))

(init-reports)

;(session "localhost" "public" 
;   (let ((description (walk ifDescr)))
;      (format #t "~a ~a ~%" (iid description) (description))
;      (failure-cont)))

(session #:peer "127.0.0.1:10161" 
     (display ((get (snmp-parse-oid "sysLocation.0")))))

(newline)


(exit-with-summary (run-all-defined-test-cases))

