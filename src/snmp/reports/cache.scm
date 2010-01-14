;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate 
;;
;; cache.scm - This file defines classes and utilities to maintain
;; a cache of snmp queries
;;
;;-------------------------------------------------------------------

(define-module (snmp reports cache)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-39)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (snmp reports session)
  #:export (
    query-cache-enabled
    enable-query-cache 
    disable-query-cache 
    query-cache-lookup
    query-cache-insert
    clear-query-cache
    query-cache-statistics
    dump-query-cache))

; Cache functions

; Private cache interface
;
(define (cache-key req oid)
  (with-output-to-string
    (lambda ()
      (format #t "~a!~a!~a!~a!~a!~a"
        req
        (current-peername)
        (current-version)
        (current-community)
        (current-context)
        oid))))

(define query-cache-enabled (make-parameter #t))
(define report-query-cache  (list))

; Public cache interface
(define (enable-query-cache) (query-cache-enabled #t))
(define (disable-query-cache) (query-cache-enabled #f))
(define (clear-query-cache) (set! report-query-cache '()))

(define (query-cache-lookup querytype oid)
  (let ((rt (assoc (cache-key querytype oid) report-query-cache string=?)))
    (if (not rt)
      #f
      (let ((lh (slot-ref (cdr rt) 'oid))
            (rh (cdr rt)))
        (acons lh rh '())))))

(define (query-cache-insert querytype oid answer)
  (set! report-query-cache (acons
                             (cache-key querytype oid)
                             (cdr answer)
                             report-query-cache)))

(define (query-cache-statistics)
  (format (current-output-port) ""))

(define (dump-query-cache)
  (pretty-print report-query-cache))
