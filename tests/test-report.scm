(use-modules (snmp reports))
(use-modules (ice-9 format))


(init-reports)

(session "localhost" "public" 
   (let* ((orid (walk sysORID))
          (ordesc (get (+ sysORDescr (iid orid)))))
      (format #t "DESCR: ~a " (ordesc))(newline)
      (failure-cont)))



