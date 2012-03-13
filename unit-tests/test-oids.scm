(use-modules (snmp net-snmp)
             (snmp oids)
             (unit-test)
             (oop goops))

(define-class <test-oids> (<test-case>)
  (oid1 #:getter oid1
               #:init-value (list->oid (list 1 2 3 4)))
  (oid2 #:getter oid2
               #:init-value (list->oid (list 1 2 3 4 5 6))))

(define-method (test-compare (self <test-oids>))
	       (assert-equal (list->oid (list 1 2 3 4 1 2 3 4 5 6)) 
			     (list->oid (list 1 2 3 4 1 2 3 4 5 6))))

(define-method (test-length (self <test-oids>))
  (assert-equal 4 (oid-length (oid1 self))))

(define-method (test-add-oids (self <test-oids>))
  (assert-equal (list->oid (list 1 2 3 4 1 2 3 4 5 6))
                (+ (oid1 self) (oid2 self))))

(define-method (test-minus-oids (self <test-oids>))
  (assert-equal (list->oid (list 5 6))
                (- (oid1 self) (oid2 self))))

(define-method (test-index1-oids (self <test-oids>))
  (assert-equal 3
                (% 3 (oid1 self))))

(define-method (test-index2-oids (self <test-oids>))
  (assert-equal (list->oid (list 3 4))
                (% 3 4 (oid1 self))))

(define-method (test-suboid-check1 (self <test-oids>))
  (assert-equal #t
                (/ (oid2 self) (oid1 self))))

(define-method (test-suboid-check2 (self <test-oids>))
  (assert-equal #f
                (/ (oid1 self) (oid2 self))))

(exit-with-summary (run-all-defined-test-cases))

