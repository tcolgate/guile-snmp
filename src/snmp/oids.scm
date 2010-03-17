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
(define-method (+ (id1 <uvec>) (id2 <uvec>))
  (list->u32vector 
    (append 
      (u32vector->list id1) 
      (u32vector->list id2)))) 
(define-method (+ (id1 <uvec>) (id2 <integer>))
  (list->u32vector 
    (append 
      (u32vector->list id1) 
      (list id2)))) 
(define-method (+ (id1 <integer>) (id2 <uvec>))
  (list->u32vector 
    (append 
      (list id1) 
      (u32vector->list id2)))) 
(define-method (+ (id1 <uvec>))
  id1)
(define-method (+ . args)
  (+ (car args)
    (apply + (cdr args))))
(define-method (+)
   0)

(enable-primitive-generic! -)
(define-method (- (base <uvec>) (id <uvec>))
  (let* ((baselist (u32vector->list base))
         (idlist (u32vector->list  id))
         (prefixlen (length baselist))
         (idlen (length idlist))
         (prefix    (list-head idlist prefixlen)))
    (if (equal? baselist prefix)
      (list->u32vector(list-tail idlist prefixlen))
      id)))
(define-method (- (id1 <uvec>))
  id1)
(define-method (-)
   0)

(define-generic %)

(define-method (% (s <integer>)(e <integer>) (id <uvec>))
  (sub-objid s e id))

(define-method (% (s <integer>)(id <uvec>))
  (u32vector-ref id (- s 1)))

(define (get-oid-type oid)
   (slot-ref (get-tree oid (get-tree-head)) 'type))

(define (sub-objid s e id)
  (list->u32vector 
      (list-tail (list-head (u32vector->list id)  e ) (- s 1))))

(define (mac-as-oid mac)
  (let* ((len (string-length mac))
         (oid (make-u32vector len))
         (i 0))
    (string-for-each
      (lambda(char)
        (u32vector-set! oid i (char->integer char))
        (set! i (+ i 1)))
      mac)
    oid))

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

