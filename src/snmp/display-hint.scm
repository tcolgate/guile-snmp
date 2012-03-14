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
  (let ((finalhint (list-ref formatter (- (length formatter) 1)))
	(wip "")
	(s 0)
	(e 10))

    (let dhintloop ((f formatter))
      (if (not (eq? f '()))
	(let* ((subhint (car f))
	       (rep?    (list-ref subhint 0))
	       (i       (list-ref subhint 1))
	       (radix   (list-ref subhint 2))
	       (repsep? (list-ref subhint 3))
	       (reptrm? (list-ref subhint 4))
	       (reps    (if rep?
			  ;(let ((c (bytevector-u8-ref))) (set! s (+ s 1)) c)
			  2
			  1)))
	  (let reploop ((r reps))
	    (if (> r 0)
	      (begin
		(format #t "pull ~a bytes and format as ~a~%" i radix)
		(reploop (- r 1)))))
	  (dhintloop (cdr f)))))))


