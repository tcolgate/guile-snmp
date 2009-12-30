(use-modules (srfi srfi-1))
(use-modules (ipv4-router))
(use-modules (ice-9 debug))
(use-modules (ice-9 pretty-print))

(define mytable (new-ipv4-table))

(define routes (list 
  (new-route "0.0.0.0 0.0.0.0" "10.0.0.1")
  (new-route "10.0.0.0 255.255.0.0" "10.0.0.12")
  (new-route "192.168.4.0 255.255.255.128" "10.0.0.3")
  (new-route "192.168.4.128 255.255.255.128" "10.0.0.4")
  (new-route "192.168.6.128 255.255.255.128" "10.0.0.4")
  (new-route "192.168.7.128 255.255.255.128" "10.0.0.4")
  (new-route "192.168.8.128 255.255.255.128" "10.0.0.4")
  (new-route "192.168.9.128 255.255.255.128" "10.0.0.4")
  (new-route "192.168.4.64 255.255.255.192" "10.0.0.6")
  (new-route "10.0.1.0 255.255.255.128" "10.0.0.8")
  (new-route "10.0.1.128 255.255.255.128" "10.0.0.9")
  (new-route "10.0.2.0 255.255.255.0" "10.0.0.10")
  ))
                  
(define mytable (fold 
                  (lambda(r t)
                    (let ((result (add-ipv4-route t r)))
;                     (trie-node->dot #t result)
                     result)) 
                (new-ipv4-table) 
                routes))

(display (find-ipv4-route mytable (new-ip "10.0.1.1")))(newline)
(display (find-ipv4-route mytable (new-ip "10.0.1.129")))(newline)
(display (find-ipv4-route mytable (new-ip "10.0.0.1")))(newline)
(display (find-ipv4-route mytable (new-ip "10.0.2.1")))(newline)
(display (find-ipv4-route mytable (new-ip "192.168.5.1")))(newline)


;(trie-node->dot #t mytable)
(pretty-print (gc-stats))
