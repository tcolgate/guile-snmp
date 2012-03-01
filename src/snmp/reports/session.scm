;;-------------------------------------------------------------------
;; Copyright (C) 2009-2012 Tristan Colgate 
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
    current-sec-level
    current-sec-auth-proto
    current-sec-priv-proto
    current-sec-auth-key
    current-sec-priv-key
    current-sec-name
    AuthNone AuthMD5 AuthSHA1
    PrivNone PrivDES PrivAES
    new-snmp-session))

(define AuthNone (list->oid (list 1 3 6 1 6 3 10 1 1 1))) ; (snmp-parse-oid "usmNoAuthProtocol")) 
(define AuthMD5  (list->oid (list 1 3 6 1 6 3 10 1 1 2))) ; (snmp-parse-oid "usmHMACMD5AuthProtocol")) 
(define AuthSHA1 (list->oid (list 1 3 6 1 6 3 10 1 1 3))) ; (snmp-parse-oid "usmHMACSHA1AuthProtocol")) 
(define PrivNone (list->oid (list 1 3 6 1 6 3 10 1 2 1))) ; (snmp-parse-oid "usmNoPrivProtocol")) 
(define PrivDES  (list->oid (list 1 3 6 1 6 3 10 1 2 2))) ; (snmp-parse-oid "usmDESPrivProtocol")) 
(define PrivAES  (list->oid (list 1 3 6 1 6 3 10 1 2 4))) ; (snmp-parse-oid "usmAESPrivProtocol"))

(define current-community (make-parameter "public"))
(define current-host (make-parameter "localhost"))
(define current-version (make-parameter SNMP-VERSION-2c ))
(define current-context (make-parameter #f))
(define current-retries (make-parameter 1))
(define current-timeout (make-parameter 1000000))
(define current-sec-level (make-parameter SNMP-SEC-LEVEL-NOAUTH)) 
(define current-sec-auth-proto (make-parameter AuthNone)) 
(define current-sec-priv-proto (make-parameter PrivNone)) 
(define current-sec-name (make-parameter #f)) 
(define current-sec-auth-key (make-parameter #f)) 
(define current-sec-priv-key (make-parameter #f)) 

(define (new-snmp-session)
  "Create a new snmp session using the current session defauts options"
  (let* ((bs (make <snmp-session>)))
    (slot-set! bs 'version (current-version))
    (slot-set! bs 'peername (current-host))
    (slot-set! bs 'community (current-community))
    (if (not (equal? #f (current-context)))
      (slot-set! bs 'context (current-context))) 
    (slot-set! bs 'retries (current-retries))
    (slot-set! bs 'timeout (current-timeout))
    (slot-set! bs 'securityLevel (current-sec-level))
    (slot-set! bs 'securityAuthProto (current-sec-auth-proto))
    (slot-set! bs 'securityPrivProto (current-sec-priv-proto))
    (if (not (equal? #f (current-sec-name)))
      (slot-set! bs 'securityName (current-sec-name)))
    (if (not (equal? #f (current-sec-auth-key)))
      (slot-set! bs 'securityAuthKey (current-sec-auth-key)))
    (if (not (equal? #f (current-sec-priv-key)))
      (slot-set! bs 'securityPrivKey (current-sec-priv-key)))
    (snmp-sess-open bs)))

(define current-session (make-parameter (new-snmp-session)))

(define-syntax default-session
  (lambda(stx)
    "Adjust the global default session options, see the (help session) for the list of options"
    (let ((args (cdr (syntax->datum stx)))
          (handler (lambda* (#:key (host #f)
                             (community #f)
                             (version #f)
                             (context #f)
                             (timeout #f)
                             (retries #f)
                             (seclevel #f)
                             (secname #f)
                             (authproto #f)
                             (privproto #f)
                             (authkey #f)
                             (privkey #f))
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
                                              (current-sec-level (if (eqv? ,seclevel #f)
                                                                   (current-sec-level)
                                                                   ,seclevel))
                                              (current-sec-name (if (eqv? ,secname #f)
                                                                  (current-sec-name)
                                                                  ,secname))
                                              (current-sec-auth-proto (if (eqv? ,authproto #f)
                                                                        (current-sec-auth-proto)
                                                                        ,authproto)) 
                                              (current-sec-auth-key (if (eqv? ,authkey #f)
                                                                      (current-sec-auth-key)
                                                                      ,authkey))  
                                              (current-sec-priv-proto (if (eqv? ,privproto #f)
                                                                        (current-sec-priv-proto)
                                                                        ,privproto)) 
                                              (current-sec-priv-key (if (eqv? ,privkey #f)
                                                                      (current-sec-priv-key)
                                                                      ,privkey))  
                                              (current-session (new-snmp-session)))))))
      (apply handler args))))

(define-syntax session
  (lambda(stx)
"Create a new snmp session. The following options cn be give:

  (session
    #:host
    #:version
    #:community
    #:timeout
    #:retries
    #:seclevel
    #:secname
    #:authproto
    #:authkey
    #:privproto
    #:privkey
    ...)
    "
    (let ((args (cdr (syntax->datum stx)))
          (handler (lambda* (#:key (host #f)
                             (community #f)
                             (context #f)
                             (timeout #f)
                             (retries #f)
                             (version #f)
                             (seclevel #f)
                             (secname #f)
                             (authproto #f)
                             (authkey #f)
                             (privproto #f)
                             (privkey #f)
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
                                                                                ,timeout))
                                                             (current-sec-level (if (eqv? ,seclevel #f)
                                                                                  (current-sec-level)
                                                                                  ,seclevel))
                                                             (current-sec-name (if (eqv? ,secname #f)
                                                                                 (current-sec-name)
                                                                                 ,secname))
                                                             (current-sec-auth-proto (if (eqv? ,authproto #f)
                                                                                       (current-sec-auth-proto)
                                                                                       ,authproto)) 
                                                             (current-sec-auth-key (if (eqv? ,authkey #f)
                                                                                     (current-sec-auth-key)
                                                                                     ,authkey))  
                                                             (current-sec-priv-proto (if (eqv? ,privproto #f)
                                                                                       (current-sec-priv-proto)
                                                                                       ,privproto)) 
                                                             (current-sec-priv-key (if (eqv? ,privkey #f)
                                                                                     (current-sec-priv-key)
                                                                                     ,privkey)))
                                                            (parameterize ((current-session (new-snmp-session)))
                                                                          (let ((result (begin
                                                                                          ,@clean-forms)))
                                                                            (snmp-sess-close (current-session))
                                                                            result))))))))
      (apply handler args))))

