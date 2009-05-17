(use-modules (snmp reports))
(use-modules (ice-9 format))

(init-reports)

;(session "localhost" "public" 
;   (let ((description (walk ifDescr)))
;      (format #t "~a ~a ~%" (iid description) (description))
;      (failure-cont)))

(define l3list (list "localhost"))

(let ((router (one-of l3list)))
  (session #:host router #:community "public"
    (let* ((arp (walk sysORTable))
           (id (% 1 (iid arp)))
           (val (iid arp)))
     (all (display val)(newline)))))




