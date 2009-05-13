#!/usr/bin/guile -s
!#
;;-------------------------------------------------------------------
;; Copyright (C) 2009 Tristan Colgate 
;;
;; goal.scm - This is intended to be a complex example of the ideal
;; grammar intended for reporting.
;;
;;-------------------------------------------------------------------

(use-modules (snmp reports))
(use-modules (ice-9 format))
(use-modules (ice-9 common-list))

(init-reports)

(define l3list '("192.168.3.59" "192.168.3.58" ))
(define name (cadr (command-line)))
(define address  (cddr (command-line)))
(define hostip (list->u32vector (map string->number (string-split (car address) #\. ))))


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

(let ((router (one-of l3list)))
  (session #:host router #:community "c0ntaCt546"
    (let* ((arp (walk ipNetToMediaPhysAddress))
           (ip (% 2 5 (iid arp)))
           (mac (arp)))
           
      (if (not (equal? hostip ip))
        (fail)
        (let* ((vlan (walk vtpVlanState))
               (vlanid (u32vector-ref (% 2 (iid vlan)) 0))
               (vlancomm (string-append "c0ntaCt546@" (number->string vlanid))))
          (session #:host router #:community vlancomm
            (let* ((mapint      (get (+ dot1dTpFdbPor (mac-as-oid mac))))
                   (validmapint (if (unspecified? (mapint))
                                  (fail)
                                  (mapint)))
                   (intif       (get (+ dot1dBasePortIfIndex validmapint)))
                   (intifDescr  (value (get (+ ifDescr (intif)))))
                   (poports     (walk pagpGroupIfIndex)))
               (if (not (equal? (poports) (intif)))
                 (fail)
                 (let* ((neighbour (walk (+ cdpCacheAddress (iid poports))))
                        (neighbourstr (ipstr-to-str (neighbour))))
                   (if (find-if (lambda(item)(equal? item neighbourstr)) l3list)
                     (fail) ; The neighbour is in the l3 list so we will check it anyway
                     (session #:host neighbourstr #:community vlancomm
                       (let* ((fdbport  (get (+ dot1dTpFdbPort (mac-as-oid mac))))
                              (fdbif    (get (+ dot1dBasePortIfIndex (fdbport))))
                              (port    (get (+ ifDescr (fdbif)))))
                         (format #t "Host: ~a  ~% Via Router: ~A ~% neighbour: ~a ~% port: ~a ~%"  name router neighbourstr (port))))))))))))))

