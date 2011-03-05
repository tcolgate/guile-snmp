;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate 
;;
;; oids.scm - This file defines classes and utilities to provide
;; oid manipulation and conversion fucntions.
;;
;;-------------------------------------------------------------------

(define-module (snmp oids)
  #:use-module (oop goops)
  #:use-module (snmp net-snmp)
  #:export (
    sub-objid
    %
    mac-as-oid
    ipstr-to-str
    str-to-ipstr
    get-oid-type
  ))

(enable-primitive-generic! +)

(define-method (+ (id1 <oid>) (id2 <oid>))
  (list->oid
    (append 
      (oid->list id1) 
      (oid->list id2))))

(define-method (+ (id1 <oid>) (id2 <uvec>))
  (list->oid
    (append 
      (oid->list id1) 
      (oidvector->list id2))))

(define-method (+ (id1 <uvec>) (id2 <oid>))
  (list->oid
    (append 
      (oidvector->list id1)
      (oid->list id2))))

(define-method (+ (id1 <uvec>) (id2 <uvec>))
  (list->oidvector 
    (append 
      (oidvector->list id1) 
      (oidvector->list id2))))

(define-method (+ (id1 <oid>) (id2 <integer>))
  (list->oid
    (append 
      (oid->list id1) 
      (list id2)))) 

(define-method (+ (id1 <integer>) (id2 <oid>))
  (list->oid
    (append 
      (list id1) 
      (oid->list id2))))

(define-method (+ (id1 <oid>))
  id1)

(define-method (+ . args)
  (+ (car args)
    (apply + (cdr args))))

(define-method (+)
   0)

(enable-primitive-generic! -)

(define-method (- (base <oid>) (id <oid>))
  (make <oid> #:value
    (- (slot-ref base '_vec) (slot-ref id '_vec))))

(define-method (- (base <oid>) (id <uvec>))
  (make <oid> #:value
    (- (slot-ref base '_vec) id)))

(define-method (- (base <uvec>) (id <oid>))
  (make <oid> #:value
    (- base (slot-ref id '_vec))))

(define-method (- (base <uvec>) (id <uvec>))
  (let* ((baselist (oidvector->list base))
         (idlist (oidvector->list  id))
         (prefixlen (length baselist))
         (idlen (length idlist))
         (prefix    (list-head idlist prefixlen)))
    (if (equal? baselist prefix)
      (list->oidvector(list-tail idlist prefixlen))
      id)))
(define-method (- (id1 <oid>))
  id1)
(define-method (- (id1 <uvec>))
  id1)
(define-method (-)
   0)

(define-generic %)

(define-method (% (s <integer>)(e <integer>) (id <oid>))
  (sub-objid s e id))

(define-method (% (s <integer>)(id <oid>))
  (oidvector-ref (slot-ref id '_vec) (- s 1)))

(define (get-oid-type oid)
   (slot-ref (get-tree oid (get-tree-head)) 'type))

(define (sub-objid s e id)
  (list->oid
      (list-tail (list-head (oid->list id)  e ) (- s 1))))

(define (mac-as-oid mac)
  (let* ((len (string-length mac))
         (oid (make-oidvector len))
         (i 0))
    (string-for-each
      (lambda(char)
        (oidvector-set! oid i (char->integer char))
        (set! i (+ i 1)))
      mac)
    (make <oid> #:value oid)))

(define (ipstr-to-str hexipaddr)
  (let* ((len (string-length hexipaddr))
         (iplist (list)))
    (string-for-each
      (lambda(char)
        (set! iplist (append iplist  (list (number->string (char->integer char))))))
      hexipaddr)
    (string-join iplist ".")))

(define (str-to-ipstr stripaddr)
  (list->string 
    (map integer->char 
         (map string->number 
              (string-split stripaddr #\.)))))

