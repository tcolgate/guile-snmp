(use-modules (snmp reports))
(use-modules (ice-9 format))


(init-reports)

;(session "localhost" "public" 
;   (let ((description (walk ifDescr)))
;      (format #t "~a ~a ~%" (iid description) (description))
;      (failure-cont)))

(display 
  (value 
    (session #:host "192.168.3.58" #:community "c0ntaCt546"
      (let* ((ahahah 1))
        (getnext sysDescr)))))
(newline)



