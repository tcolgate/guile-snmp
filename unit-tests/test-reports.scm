(use-modules (snmp reports)
             (srfi srfi-1)
             (unit-test)
             (oop goops))

(putenv "MIBDIRS=+.")
(putenv "MIBS=+GUILE-SNMP-TEST-MIB")
(init-reports)

(define-class <test-reports> (<test-case>))
  
(define-method (test-autoresolve (self <test-reports>))
  (assert-equal gstTestString
               #u32(1 3 6 1 3 1977 1 2)))

(define-method (test-get (self <test-reports>))
  (assert-equal "Guile-SNMP test string"
                (session #:host "127.0.0.1:10161" 
                  ((get (snmp-parse-oid "gstTestString.0"))))))
;
(define-method (test-get-multiple (self <test-reports>))
  (assert-equal (list "Guile-SNMP test string" 499)
                (session #:host "127.0.0.1:10161" 
                  (let ((vals (get (snmp-parse-oid "gstTestString.0") (snmp-parse-oid "gstTestInt32.0"))))
                    (list (vals) (vals (snmp-parse-oid "gstTestInt32.0")))))))
;
(define-method (test-getnext (self <test-reports>))
  (assert-equal "Guile-SNMP test string"
                (session #:host "127.0.0.1:10161" 
                  ((getnext (snmp-parse-oid "gstTestString"))))))

(define-method (test-walk-func (self <test-reports>))
  (assert-equal '(0 1)
                (session #:host "127.0.0.1:10161" 
                  (let* ((mywalk (walk-func (snmp-parse-oid "gstTabATable")))
                         (myval1 (value (mywalk)))
                         (myval2 (value (mywalk))))
                    (list myval1 myval2)))))

(define-method (test-walk (self <test-reports>))
  (assert-equal '("tableA row 0" "tableA row 1" "tableA row 2" "tableA row 3" "tableA row 4" "tableA row 5")
                (session #:host "127.0.0.1:10161" 
                  (map (lambda(x)(value x)) (walk (snmp-parse-oid "gstTabAData"))))))

(define-method (test-walk-on-fail (self <test-reports>))
  (assert-equal '("tableA row 0" "tableA row 1" "tableA row 2" "tableA row 3" "tableA row 4" "tableA row 5")
                (session #:host "127.0.0.1:10161" 
                  (let* ((result (list))
                         (val    (walk-on-fail (snmp-parse-oid "gstTabAData"))))
                    (if (not (equal? (val) #f))
                      (begin
                        (set! result (append result (list (val))))
                        (fail))
                      result)))))

(exit-with-summary (run-all-defined-test-cases))

