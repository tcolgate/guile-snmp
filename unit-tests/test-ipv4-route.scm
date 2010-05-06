(use-modules (ipv4-route)
             (srfi srfi-1)
             (unit-test)
             (oop goops))

(define-class <test-ipv4-route> (<test-case>)
  (test-string #:getter test-string
               #:init-value "The quick brown fox.")
  ;; this answer generated with /usr/bin/md5 for comparison purposes...
  (test-answer #:getter test-answer
               #:init-value  "2e87284d245c2aae1c74fa4c50a74c77"))

(define-method (test-default-port (self <test-md5>))
  (assert-equal (test-answer self)
    (with-input-from-string (test-string self)
      (lambda () (md5)))))

(define-method (test-given-port (self <test-md5>))
  (assert-equal (test-answer self)
    (md5 (open-input-string (test-string self)))))

(define mytable (make <ipv4-table>))

(define routes (list 
  (make <ipv4-route> #:net "0.0.0.0 0.0.0.0" "10.0.0.1")
  (make <ipv4-route> #:net "10.0.0.0 255.255.0.0" "10.0.0.12")
  (make <ipv4-route> #:net "192.168.4.0 255.255.255.128" "10.0.0.3")
  (make <ipv4-route> #:net "192.168.4.128 255.255.255.128" "10.0.0.4")
  (make <ipv4-route> #:net "192.168.6.128 255.255.255.128" "10.0.0.4")
  (make <ipv4-route> #:net "192.168.7.128 255.255.255.128" "10.0.0.4")
  (make <ipv4-route> #:net "192.168.8.128 255.255.255.128" "10.0.0.4")
  (make <ipv4-route> #:net "192.168.9.128 255.255.255.128" "10.0.0.4")
  (make <ipv4-route> #:net "192.168.4.64 255.255.255.192" "10.0.0.6")
  (make <ipv4-route> #:net "10.0.1.0 255.255.255.128" "10.0.0.8")
  (make <ipv4-route> #:net "10.0.1.128 255.255.255.128" "10.0.0.9")
  (make <ipv4-route> #:net "10.0.2.0 255.255.255.0" "10.0.0.10")
  ))
                  
(define mytable (fold 
                  (lambda(r t)
                    (let ((result (add-ipv4-route t r)))
                     result)) 
                (make <ipv4-ipv4-table>) 
                routes))

(display (find-ipv4-route mytable (make <ipv4-ip> "10.0.1.1")))(newline)
(display (find-ipv4-route mytable (make <ipv4-ip> "10.0.1.129")))(newline)
(display (find-ipv4-route mytable (make <ipv4-ip> "10.0.0.1")))(newline)
(display (find-ipv4-route mytable (make <ipv4-ip> "10.0.2.1")))(newline)
(display (find-ipv4-route mytable (make <ipv4-ip> "192.168.5.1")))(newline)

(exit-with-summary (run-all-defined-test-cases))
