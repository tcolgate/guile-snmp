(use-modules (snmp reports))
(use-modules (ice-9 format))


(init-reports)

;(session "localhost" "public" 
;   (let ((description (walk ifDescr)))
;      (format #t "~a ~a ~%" (iid description) (description))
;      (failure-cont)))

(session "localhost" "public" 
  (all
      (display (value (walk ifDescr)))
      (newline)))



