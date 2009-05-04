(use-modules (snmp reports))

(init-reports)

(session "localhost" "public" 
  (let ((vals (walk sysORDescr sysORUptime)))
        (all (print vals))(newline)))


