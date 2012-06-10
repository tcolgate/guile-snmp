(use-modules (asn1-parser)
	     (asn1-tokenize)
             (unit-test)
             (oop goops))

(define-class <test-asn1> (<test-case>))

(define-method (test-tokenize(self <test-asn1>))
	       (assert-equal (call-with-input-file "GUILE-SNMP-TEST-MIB" asn1-tokenize )
			     #f))

(define-method (test-parse (self <test-asn1>))
	       (assert-equal (call-with-input-file "GUILE-SNMP-TEST-MIB" read-asn1 )
			     #f))

(exit-with-summary (run-all-defined-test-cases))

