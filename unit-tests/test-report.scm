(use-modules (snmp reports))
(use-modules (ice-9 format))

(init-reports)

;(session "localhost" "public" 
;   (let ((description (walk ifDescr)))
;      (format #t "~a ~a ~%" (iid description) (description))
;      (failure-cont)))

(session #:host "127.0.0.1" 
     (display ((get (snmp-parse-oid "sysLocation.0")))))

(newline)



