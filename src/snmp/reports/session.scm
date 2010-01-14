;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate 
;;
;; session.scm - This file contains various bits of common state information
;; needed by other modules
;;
;;-------------------------------------------------------------------

(define-module (snmp reports session)
  #:use-module (srfi srfi-39)
  #:use-module (oop goops)
  #:use-module (snmp net-snmp)
  #:export-syntax (session)
  #:export (
    current-session
    current-community 
    current-port 
    current-peername
    current-version 
    current-context
    new-snmp-session))

(define current-community (make-parameter "public"))
(define current-peername (make-parameter "localhost"))
(define current-version (make-parameter (SNMP-VERSION-2c)))
(define current-context (make-parameter #f))

(define (new-snmp-session)
  (let* ((bs (make <snmp-session>))
         (jn (snmp-sess-init bs)))
    (slot-set! bs 'version (current-version))
    (slot-set! bs 'peername (current-peername))
    (slot-set! bs 'community (current-community))
    (slot-set! bs 'community-len (string-length (current-community)))
    (snmp-sess-open bs)))

(define current-session (make-parameter (new-snmp-session)))

(define-syntax session
  (lambda(stx)
    (let ((args (cdr (syntax->datum stx)))
          (handler (lambda* (#:key (host #f)
                                   (community #f)
                                   (version #f)
                                   (context #f)
                             #:rest forms)
                     (let* ((remove-keys (lambda(ls)
                                           (let loop ((ls ls) (acc '()))
                                             (if (null? ls)
                                               (reverse acc)
                                               (let ((kw? (keyword? (car ls))))
                                                 (loop ((if kw? cddr cdr) ls)
                                                   (if kw? acc (cons (car ls) acc))))))))
                            (clean-forms (remove-keys forms)))
                       (datum->syntax stx
                        `(parameterize ((current-version   (if (eqv? ,version #f)
                                                             (current-version)
                                                             ,version))
                                        (current-peername  (if (eqv? ,host #f)
                                                             (current-peername)
                                                             ,host))
                                        (current-community (if (eqv? ,community #f)
                                                             (current-community)
                                                             ,community)))
                           (parameterize ((current-session (new-snmp-session)))
                             (begin
                               ,@clean-forms))))))))
          (apply handler args))))

