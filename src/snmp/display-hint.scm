;;-------------------------------------------------------------------
;; Copyright (C) 2009-2012 Tristan Colgate 
;;
;; display-hint.scm - This file provides routines for parsing and 
;;                    formatting RFC2579 MIB display hints
;;
;;-------------------------------------------------------------------
;;
(define-module (snmp display-hint)
	       #:use-module (ice-9 regex)
	       #:use-module (rnrs bytevectors))

(define dhint-integer-regex (make-regexp "([xoat]|(d(-[0-9]+){0,1}))"))

(define dhint-octect-str-regex (make-regexp "(\\*{0,1})([0-9]+)([xoatd])([^0-9\\*]{0,1})([^0-9\\*]{0,1})"))

(define (dhint->formatter dhint)
  (let* ((subhints (fold-matches dhint-octect-str-regex dhint '()
				 (lambda (match hints)
				   (let ((hintform (list (if (string=? (match:substring match 1) "*") #t #f)
							 (if (not (string=? (match:substring match 2) ""))
							   (string->number (match:substring match 2) 10) #f)
							 (if (not (string=? (match:substring match 3) ""))
							   (case (string-ref (match:substring match 3) 0)
							     ((#\d) 10)
							     ((#\o) 8)
							     ((#\x) 16)
							     ((#\a) 'ascii)
							     ((#\t) 'unicode))
							   #f)
							 (if (not (string=? (match:substring match 4) ""))
							   (string-ref (match:substring match 4) 0) #f)
							 (if (not (string=? (match:substring match 5) ""))
							   (string-ref (match:substring match 5) 0) #f) )))
				     (append hints (list hintform)))))))
    subhints))

(define (apply-display-hint bv formatter)
  ; The last formatter
  (let* ((finalhint (list-ref formatter (- (length formatter) 1)))
  	 (wip "")
	 (s 0)
	 (len 36) 
	 (consume (lambda (n)
		    (if (> (+ s n)  len) (throw 'empty (list 'blah))
		    (let* ((r (make-bytevector n))) 
	  	      (bytevector-copy! bv s r 0 n)
		      (set! s (+ s n))
		      r)))) 
		    
	 (left    (lambda () (- len s))) 
	 (apply-subhint (lambda (h)
			  (let* ((rep?    (list-ref h 0))
				 (i       (list-ref h 1))
				 (radix   (list-ref h 2))
				 (repsep? (list-ref h 3))
				 (reptrm? (list-ref h 4))
				 (reps    (if rep?
					    (bytevector-u8-ref (consume 1)) 
					    1)))
			    (let reploop ((r reps))
			      (if (> r 0)
				(begin
                                  (format #t "format ~a chars as ~a. ~a times: got ~a~%" 
					  i radix reps (consume i))
				  (reploop (- r 1))))))))) 

    (let dhintloop ((f formatter))
      (if (not (eq? f '()))
	(begin (apply-subhint (car f))
	  (dhintloop (cdr f))))) 

    (let finalloop ((l (left))) 
      (if(> (left) 0)
	(begin 
	  (apply-subhint finalhint)
	  (finalloop (left)))))))

(define testbv #vu8(2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(display testbv) (newline)
(apply-display-hint testbv  (dhint->formatter "0a[2x:2x:2x:2x:2x:2x:2x:2x]0a:2d"))
