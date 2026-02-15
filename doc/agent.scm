(use-modules (oop goops)
             (snmp net-snmp)
             (ice-9 format))

(define fork #f)
(define syslog #f)
(define agentx #t)


(define (table-handfunc handler reginfo reqinfo req)
  (if (eq? (mode reqinfo) MODE-GET)
    (begin
      (let* ((table-entry (netsnmp-extract-iterator-context req))
             (table-info  (netsnmp-extract-table-info req))
             (col         (colnum table-info)))

        (if (not (eq? table-entry #f))
          (begin
            ;(snmp-set-var-typed-value (requestvb req) ASN-INTEGER 1)
            (snmp-set-var-typed-value (requestvb req) ASN-OBJECT-ID (snmp-parse-oid ".1.2.3.4"))
            SNMP-ERR-NOERROR)
          (begin
            (netsnmp-set-request-error reqinfo req SNMP-NOSUCHINSTANCE)
            SNMP-ERR-NOERROR))))))

#!
(define (setup-agent)
  (let* ((handler (netsnmp-create-handler "test1" handfunc))
         (handreg (netsnmp-handler-registration-create "test1reg"
                                                       handler
                                                       (snmp-parse-oid ".1.2.3.4.5.6.7")
                                                       HANDLER-CAN-RWRITE)))
    (netsnmp-register-scalar handreg)))
!#

(define (gfdp-proc a b c d)
  (format #t "gfdp: ~a ~a ~a ~a~%" a b c d)
  (variable-set! a (list 1 2))
  (gndp-proc a b c d))

(define (gndp-proc a b c d)
  (format #t "gndp: ~a ~a ~a ~a~%" a b (variables c) d)
  (let* ((data (variable-ref a)))
    (if (not (equal? data '()))
      (begin
        (variable-set! a (cdr data))
        (variable-set! b (car data))
        (map
          snmp-set-var-typed-value
          (variables c)
          (list ASN-INTEGER
                ASN-OBJECT-ID)
          (list (variable-ref b)
                (snmp-parse-oid "ifName")))
        c)
      #f)))

(define (setup-agent)
  (let* ((handler (netsnmp-create-handler "test1" table-handfunc))
         (handreg (netsnmp-handler-registration-create "test1reg"
                                                       handler
                                                       (snmp-parse-oid ".1.2.3.4.5.6.7")
                                                       HANDLER-CAN-RWRITE))
         (table-info (make <netsnmp-table-registration-info>))
         (iinfo (make <netsnmp-iterator-info>)))
    (netsnmp-table-helper-add-index table-info ASN-INTEGER)
    (netsnmp-table-helper-add-index table-info ASN-OBJECT-ID)
    (set! (min-column table-info) 1)
    (set! (max-column table-info) 4)
    (set! (get-first-data-point iinfo) gfdp-proc)
    (set! (get-next-data-point iinfo) gndp-proc)
    (set! (table-reginfo iinfo) table-info)
    (netsnmp-register-table-iterator handreg iinfo)))

(if agentx
  (snmp-set-agent-agentx #t))

(init-agent "snmpd")

(setup-agent)

(if (not agentx)
  (begin
    (init-vacm)
    (init-usm)))

(init-snmp "snmpd")

(if (not agentx)
  (init-master-agent))

(let loop ((keep-running #t))
  (agent-check-and-process #t)
  (if keep-running (loop #t)))

(snmp-shutdown "test-agent")

