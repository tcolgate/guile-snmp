(use-modules (snmp oids)
             (unit-test)
             (oop goops))

(define-class <test-oids> (<test-case>)
  (oid1 #:getter oid1
               #:init-value #u32(1 2 3 4))
  (oid2 #:getter oid2
               #:init-value #u32(1 2 3 4 5 6)))

(define-method (test-add-oids (self <test-oids>))
  (assert-equal #u32(1 2 3 4 1 2 3 4 5 6)
                (+ (oid1 self) (oid2 self))))

(define-method (test-minus-oids (self <test-oids>))
  (assert-equal #u32(5 6)
                (- (oid2 self) (oid1 self))))

(exit-with-summary (run-all-defined-test-cases))

