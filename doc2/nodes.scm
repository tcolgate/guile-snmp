; Dfine the spin l2/l3 nodes
;
(use-modules (ice-9 optargs))
(use-modules (ice-9 pretty-print))

(define nodes '())

(define-class <node> ()
    (ip #:init-value "0.0.0.0" #:init-keyword #:ip)
    (community #:init-value "public" #:init-keyword #:community)
    (version #:init-value SNMP-VERSION-2c #:init-keyword #:version))

(defmacro* add-device (name #:key ip community version)
  `(set! nodes (append nodes (list (cons ,name (make <node> #:ip ,ip #:community ,community #:version ,version))))))

(add-device "net-dc1-2950a" #:ip "net-dc1-2950a.mydomain" #:community "mycomm" #:version SNMP-VERSION-2c) 
(add-device "net-dc1-2950b" #:ip "net-dc1-2950b.mydomain" #:community "mycomm" #:version SNMP-VERSION-2c) 
(add-device "net-dc1-2950e" #:ip "net-dc1-2950e.mydomain" #:community "mycomm" #:version SNMP-VERSION-2c) 
(add-device "net-dc1-2950f" #:ip "net-dc1-2950f.mydomain" #:community "mycomm" #:version SNMP-VERSION-2c) 
(add-device "net-dc1-2950h" #:ip "net-dc1-2950h.mydomain" #:community "mycomm" #:version SNMP-VERSION-2c) 
(add-device "net-dc1-2950i" #:ip "net-dc1-2950i.mydomain" #:community "mycomm" #:version SNMP-VERSION-2c) 

;(display nodes)(newline)
