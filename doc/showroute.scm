#!/usr/bin/env snmp-shell -s
!#

;;-------------------------------------------------------------------
;; Copyright (C) 2009-2012 Tristan Colgate 
;;
;; showroute -  an example of using snmp-shell
;;
;;-------------------------------------------------------------------

; ./showroute srcip srcgw dstip dstgw
;
; Queries the routing table of a list of layer 3 nodes
; not build a network map, then list the hops taken from
; the given src address via the given initial gateways
;
; e.g.
; ./showroute 192.168.0.10 192.168.0.1 10.10.10.15 10.10.10.1
;
; The srcgw and dstgw must be an interface on one of the devices
; listed in the nodes list. The src and dst network need not be.

;(debug-reports #t)

(use-modules (srfi srfi-1))
(use-modules (ice-9 regex))
(use-modules (ipv4-router))

(primitive-load "./nodes-l3.scm")

(define (print-path src srcgw dst dstgw netmap)
  (let loop ((rtinfo (misc  (find-ipv4-route netmap srcgw)))) 
    (let* ((name (car rtinfo)) 
           (rtr  (cdr rtinfo))
           (next (find-ipv4-route rtr dst))        
           (nextgw (find-ipv4-route netmap (gw next)))) 

      (if nextgw
        (let ((gwname (car (misc nextgw)))
              (gwinfo (cdr (misc nextgw))))
          (if (equal? name gwname) 
            (format #t "end~%") 
            (begin
              (format #t "~a routes ~a via ~a using ~a~%" name dst gwname next)
              (loop (misc nextgw))))) 
        (format #t "~a routes ~a via unknown gateway ~a~%" name dst (gw next))))))

(define (print-route srcstr srcgwstr dststr dstgwstr)
  ; pervert the ipv4 routing table to give us a hash of 
  ; interfaces to the routing table that supports them
  (let ((src (make <ipv4-address> #:ip srcstr))
        (srcgw (make <ipv4-address> #:ip srcgwstr))        
        (dst (make <ipv4-address> #:ip dststr))       
        (dstgw (make <ipv4-address> #:ip dstgwstr))
        (netmap (session 
                  #:community "public"
                  (fold
                    (lambda(item table)
                      (let ((name (car item))
                            (node (cdr item)))
                        (catch
                          #t
                          (lambda()
                            (session 
                              #:host (slot-ref node 'ip)
                              #:community (slot-ref node 'community)
                              #:version (slot-ref node 'version)
                              (let ((name     ((get sysName.0)))
                                    (ints     (walk ipAdEntAddr))
                                    (rdests   (walk ipRouteDest)) 
                                    (rmasks   (walk ipRouteMask)) 
                                    (rhops    (walk ipRouteNextHop)))
                                (let ((routes (fold
                                                (lambda (rdest rmask rhop rtable)
                                                  (add-ipv4-route 
                                                    rtable 
                                                    (make <ipv4-route>
                                                          #:net (make <ipv4-network>
                                                                      #:prefix (make <ipv4-address> #:ip (rdest))
                                                                      #:mask (rmask))
                                                          #:gw (make <ipv4-address> #:ip (rhop))
                                                          #:misc name )))
                                                (make <ipv4-table>)
                                                rdests
                                                rmasks
                                                rhops))) 
                                  (fold
                                    (lambda (int table2)
                                      (add-ipv4-route 
                                        table2 
                                        (make <ipv4-route>
                                              #:net (make <ipv4-network>
                                                          #:prefix (make <ipv4-address> #:ip (int))
                                                          #:prefix-len 32)
                                              #:misc (cons name routes))))
                                    table 
                                    ints)))))
                          (lambda (key . args)
                            (format #t "failednode ~a ~%" (slot-ref node 'ip))
                            #t))))
                    (make <ipv4-table>)
                    nodes))))

    (print-path src srcgw dst dstgw netmap)
    (print-path dst dstgw src srcgw netmap)))

(define (main args)
    (apply print-route args))

(main (script-arguments))

