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
  #:export-syntax (
    session
    default-session)
  #:export (
    current-session
    current-community 
    current-port 
    current-host
    current-version 
    current-context
    current-retries
    current-timeout
    new-snmp-session))

(define current-community (make-parameter "public"))
(define current-host (make-parameter "localhost"))
(define current-version (make-parameter SNMP-VERSION-2c ))
(define current-context (make-parameter #f))
(define current-retries (make-parameter 1))
(define current-timeout (make-parameter 1000000))

(define (new-snmp-session)
  (let* ((bs (make <snmp-session>)))
    (slot-set! bs 'version (current-version))
    (slot-set! bs 'peername (current-host))
    (slot-set! bs 'community (current-community))
    (slot-set! bs 'community-len (string-length (current-community)))
    (slot-set! bs 'community (current-community))
    (if (not (equal? #f (current-context)))
      (begin 
        (slot-set! bs 'contextName (current-context))
        (slot-set! bs 'contextNameLen (string-length (current-context)))))
    (slot-set! bs 'retries (current-retries))
    (slot-set! bs 'timeout (current-timeout))
    (snmp-sess-open bs)))

(define current-session (make-parameter (new-snmp-session)))

(define-syntax default-session
  (lambda(stx)
    (let ((args (cdr (syntax->datum stx)))
          (handler (lambda* (#:key (host #f)
                                   (community #f)
                                   (version #f)
                                   (context #f)
                                   (timeout #f)
                                   (retries #f))
                     (datum->syntax stx
                      `(begin
                         (current-version   (if (eqv? ,version #f)
                                              (current-version)
                                              ,version))
                         (current-host  (if (eqv? ,host #f)
                                              (current-host)
                                              ,host))
                         (current-community (if (eqv? ,community #f)
                                              (current-community)
                                              ,community))
                         (current-context (if (eqv? ,context #f)
                                              (current-context)
                                              ,context))
                         (current-retries (if (eqv? ,retries #f)
                                              (current-retries)
                                              ,retries))
                         (current-timeout (if (eqv? ,timeout #f)
                                              (current-timeout)
                                              ,timeout))
                         (current-session (new-snmp-session)))))))
          (apply handler args))))

(define-syntax session
  (lambda(stx)
    (let ((args (cdr (syntax->datum stx)))
          (handler (lambda* (#:key (host #f)
                                   (community #f)
                                   (context #f)
                                   (timeout #f)
                                   (retries #f)
                                   (version #f)
                                   (usernane #f)
                                   (authMode #f)
                                   (authKey #f)
                                   (privMode #f)
                                   (privKey #f)
                                   (seclevel #f)
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
                                        (current-host  (if (eqv? ,host #f)
                                                             (current-host)
                                                             ,host))
                                        (current-community (if (eqv? ,community #f)
                                                             (current-community)
                                                             ,community))
                                        (current-context (if (eqv? ,context #f)
                                                             (current-context)
                                                             ,context))
                                        (current-retries (if (eqv? ,retries #f)
                                                             (current-retries)
                                                             ,retries))
                                        (current-timeout (if (eqv? ,timeout #f)
                                                             (current-timeout)
                                                             ,timeout)))
                           (parameterize ((current-session (new-snmp-session)))
			     (let ((result (begin
                                            ,@clean-forms)))
			       (snmp-sess-close (current-session))
			       result))))))))
          (apply handler args))))

