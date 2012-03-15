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
	       #:use-module (rnrs bytevectors)
	       #:export (
		dhint-integer-regex
		dhint-octect-str-regex
		dhint->formatter
		apply-octet-str-display-hint))

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
							     ((#\a) #\a)
							     ((#\t) #\t))
							   #f)
							 (if (not (string=? (match:substring match 4) ""))
							   (match:substring match 4) #f)
							 (if (not (string=? (match:substring match 5) ""))
							   (match:substring match 5) #f) )))
				     (append hints (list hintform)))))))
    subhints))

(define (apply-octet-str-display-hint bv formatstr)
  ; The last formatter
  (let* ((formatter (dhint->formatter formatstr)) 
	 (finalhint (list-ref formatter (- (length formatter) 1)))
  	 (wip "")
	 (s 0)
	 (len (bytevector-length bv)) 
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
				 (dispsep (list-ref h 3))
				 (reptrm  (list-ref h 4))
				 (reps    (if rep?
					    (bytevector-u8-ref (consume 1) 0) 
					    1))
				 (use     (min i (left))))
			    (let reploop ((r reps))
			      (if (> r 0)
				(begin
				  (if (> i 0)
				    (let ((bytes (consume use)))
				      (set! wip 
					(string-append 
					  wip  
					  (case radix
					    ((#\a)
					     (utf8->string bytes)) 
					    ((#\t)
					     (utf8->string bytes)) 
					    (else
					      (let ((num (bytevector-uint-ref bytes 0 (endianness big) use)))
						(number->string num radix)))))))) 
				  (if (and (not (eq? #f dispsep)); If a display seperator is provided
					   (if(eq? #f reptrm)    ; AND we aren't about to display a repeat
					     #t                  ; seperator, AND there is still stuff left to display
					     (> r 1))
					   (not (eq? (left) 0))) 
				    (set! wip (string-append wip dispsep)))
				  (reploop (- r 1)))
				(if (and  
				      rep? 
				      (not (eq? #f reptrm)) 
				      (not (eq? (left) 0)))
				  (set! wip (string-append wip reptrm))))))
			  wip))) 

    (let dhintloop ((f formatter))
      (if (not (eq? f '()))
	(begin (apply-subhint (car f))
	  (dhintloop (cdr f))))) 

    (let finalloop ((l (left))) 
      (if(> (left) 0)
	(begin 
	  (apply-subhint finalhint)
	  (finalloop (left)))))
    wip))

