;;-------------------------------------------------------------------
;; Copyright (C) 2009 Tristan Colgate 
;;
;; goal.scm - Some info 
;;
;;-------------------------------------------------------------------

(use-modules (oop goops))
(use-modules (snmp net-snmp))
(use-modules (snmp reports))
(init-snmp (car(command-line)))
(init-mib)
(init-reports)

(define l3list '(localhost))

(define (find-host )
  (for-each
    (lambda(router)
      (session router
        (let* ((arp (get (+ ipNetToMediaPhysAddressq 
                               (% 2 (iid (walk vtpVlanState)))
                               (ip-as-oid hostip))))
               (vlan (% 1 (iid arp)))
               (mac (arp)))
          (sesion l3-host (+ community vlan)
                  (let* ((interface (get (+ dot1dBasePortIfIndex
                                               (get (+ dot1dTpFdbPor
                                                          (mac-as-oid mac))))))
                         (neighbour (walk (+ '(cdpCacheAddress cdpCacheAddressType)  
                                                (iid (walk pagpGroupIfIndex (pagpGroupIfIndex interface)))) ;uses a filter
                                          (cdpCacheAddressType ipaddress)))
                         (port (session neighbour (+ community vlan) (get (+ ifDescr
                                                   (get (+ dot1dBasePortIfIndex
                                                              (get (+ dot1dTpFdbPort (mac-as-oid mac))))))))))
                    (format #t "Found ~a on ~a port ~a" hostip neighbour port)))))
      l3list)))



