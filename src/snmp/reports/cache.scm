;;-------------------------------------------------------------------
;; Copyright (C) 2009-2012 Tristan Colgate 
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
(define (cache-key req oid nrs reps)
  (with-output-to-string
    (lambda ()
      (format #t "~a!~a!~a!~a!~a!~a!~a!~a"
        req
        (current-host)
        (current-version)
        (current-community)
        (current-context)
        oid
        nrs
        reps))))

(define query-cache-enabled (make-parameter #t))
(define report-query-cache  (make-hash-table 1024))

; Public cache interface
(define (enable-query-cache) (query-cache-enabled #t))
(define (disable-query-cache) (query-cache-enabled #f))
(define (clear-query-cache) (hash-clear! report-query-cache))

(define (query-cache-lookup querytype oid nrs reps)
  (let ((rt (hash-ref report-query-cache (cache-key querytype oid nrs reps))))
    (if (not rt)
      #f
      (acons (slot-ref rt 'oid) rt '()))))

(define (query-cache-insert querytype oid answer nrs reps)
  (hash-set! report-query-cache 
             (cache-key querytype oid nrs reps)
             (cdr answer)))

(define (query-cache-statistics)
  

  (format (current-output-port) ""))

(define (dump-query-cache)
  (hash-for-each (lambda(k v) (format #t "~a: ~a~%" k v)) report-query-cache))
