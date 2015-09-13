```scheme
; simple trap reception
(let ((trs (netsnmp-tdomain-transport "127.0.0.1:10162" #t "udp"))
      (sess (make <snmp-session>)))
  (slot-set! sess 'peername "localhost")
  (slot-set! sess 'version SNMP-VERSION-2c)
  (slot-set! sess 'community "public")
  (slot-set! sess 'timeout 100000)
  (slot-set! sess 'retries 10)
  (slot-set! sess
             'callback
             (lambda(a b c d)
               (format #t "trap ~a ~a ~a ~a~%" a b c (split-varbinds (variables d)))
               1))
  (snmp-add sess trs))

(let ((fdinfo (snmp-select-info)))
  (snmp-select fdinfo)
  (snmp-read fdinfo))

;   snmptrap -v 2c -c public 127.0.0.1:10162 10 ifLinkUp ifDescr = "hello"

```
