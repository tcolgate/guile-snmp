(use-modules (snmp net-snmp)
             (snmp display-hint)
             (unit-test)
	     (rnrs bytevectors)
             (oop goops))

(define-class <test-display-hint> (<test-case>))

(define-method (test-ascii-string (self <test-display-hint>))
	       (assert-equal "Hello World"
			     (apply-octet-str-display-hint 
			       (string->utf8 "Hello World") 
			       "255a")))

(define-method (test-short-ascii-string (self <test-display-hint>))
	       (assert-equal "Hello World"
			     (apply-octet-str-display-hint 
			       (string->utf8 "Hello World")  
			       "4a")))

(define-method (test-utf8-string (self <test-display-hint>))
	       (assert-equal "Hello World"
			     (apply-octet-str-display-hint 
			       (string->utf8 "Hello World") 
			       "255t")))

(define-method (test-sep-and-term (self <test-display-hint>))
	       (assert-equal "ff:80:40:20/10:8:4:2:1" 
			     (apply-octet-str-display-hint 
			       #vu8(4 255 128 64 32 16 8 4 2 1)  
			       "*1x:/1x:")))

(define-method (test-mixed-1 (self <test-display-hint>))
	       (assert-equal "[ff00:8040:20ff:101]:2572560000" 
			     (apply-octet-str-display-hint 
			       #vu8(255 0 128 64 32 255 1 1 1 1 1 0 0 0 0 0 0 0 0) 
			       "0a[2x:2x:2x:2x]0a:2d")))

(exit-with-summary (run-all-defined-test-cases))

