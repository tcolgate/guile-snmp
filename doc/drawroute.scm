#!/usr/bin/env snmp-shell -s
!#

; draw a graph of the current spanning tree configuration for 
; a given vlan This example also uses getopt-long ot process
; command line arguments.

;(debug-reports #t)

(use-modules (srfi srfi-1))
(use-modules (ice-9 regex))
(use-modules (ipv4-router))
(use-mibs IP-FORWARD-MIB)

(primitive-load "./nodes-l3.scm")

(define (print-path color src srcgw dst dstgw netmap)
  (let loop ((rtinfo (misc  (find-ipv4-route netmap srcgw))) 
             (prev  src)) 
    (let* ((name (car rtinfo)) 
           (rtr  (cdr rtinfo))
           (next (find-ipv4-route rtr dst))) 
      (if next
        (let ((nextgw (find-ipv4-route netmap (gw next))))
          (if nextgw
            (let ((gwname (car (misc nextgw)))
                  (gwinfo (cdr (misc nextgw))))
              (if (equal? name gwname) 
                (begin 
                  (format #t "  \"~a\" -> \"~a\" [color=~a] ;~%" prev name color)  
                  (format #t "  \"~a\" -> \"~a\" [color=~a] ;~%" name dst color))
                (begin
                  (format #t "  \"~a\" -> \"~a\" [color=~a] ;~%" prev name color)
                  (loop (misc nextgw) name)))) 
            (begin 
              (format #t "  \"~a\" -> \"~a\" [color=~a] ;~%" prev name color)
              (format #t "  \"~a\" -> \"~a\" [color=~a] ;~%" name (gw next) color)   
              (format #t "  \"~a\" -> \"~a\" [color=~a] ;~%" (gw next) dst color))))
        (format #t "  \"~a\" -> unknown -> \"~a\" [color=~a] ;~%" dst name color)))))

(define (print-paths srcstr srcgwstr dststr dstgwstr netmap)
  (let ((src (make <ipv4-address> #:ip srcstr))
        (srcgw (make <ipv4-address> #:ip srcgwstr))        
        (dst (make <ipv4-address> #:ip dststr))       
        (dstgw (make <ipv4-address> #:ip dstgwstr))) 

    (format #t "digraph path {~%")
    (format #t "  rankdir=BT ;")
    (format #t "  \"~a\" [shape=box] ;~%" src)
    (format #t "  \"~a\" [shape=box] ;~%" dst)
    (format #t "")
    
    (print-path "blue" src srcgw dst dstgw netmap) 
    (print-path "green" dst dstgw src srcgw netmap) 

    (format #t "}~%")))

(define (build-netmap)
  ; pervert the ipv4 routing table to give us a hash of 
  ; interfaces to the routing table that supports them
  (session 
    #:community "K9gYhRWkXhX3Afx"
    (fold
      (lambda(item maptable)
        (let ((name (car item))
              (node (cdr item)))
          (catch
            #t
            (lambda()
              (session 
                #:host (slot-ref node 'ip)
                #:community (slot-ref node 'community)
                #:version (slot-ref node 'version)
                (let* ((name       ((get sysName.0)))
                       (ints       (walk ipAdEntAddr))
                       (rdests     (walk ipRouteDest)) 
                       (rmasks     (walk ipRouteMask)) 
                       (rhops      (walk ipRouteNextHop))
                       (rcidrdests (catch #t
                                          (lambda () (walk ipCidrRouteDest))
                                          (lambda (key . args) '()))) 
                       (rcidrmasks (catch #t
                                          (lambda () (walk ipCidrRouteMask))
                                          (lambda (key . args) '())))
                       (rcidrhops  (catch #t
                                          (lambda () (walk ipCidrRouteNextHop))
                                          (lambda (key . args) '())))                                     
                       (routes   (append
                                   (map
                                     (lambda (rdest rmask rhop)
                                       (make <ipv4-route>
                                             #:net (make <ipv4-network>
                                                         #:prefix (make <ipv4-address> #:ip (rdest))
                                                         #:mask (rmask))
                                             #:gw (make <ipv4-address> #:ip (rhop))
                                             #:misc name ))
                                     rdests
                                     rmasks
                                     rhops) 
                                   (map
                                     (lambda (rcidrdest rcidrmask rcidrhop)
                                       (make <ipv4-route>
                                             #:net (make <ipv4-network>
                                                         #:prefix (make <ipv4-address> #:ip (rcidrdest))
                                                         #:mask(rcidrmask))
                                             #:gw (make <ipv4-address> #:ip (rcidrhop))
                                             #:misc name ))
                                     rcidrdests
                                     rcidrmasks
                                     rcidrhops)))
                       (routestable (fold
                                      (lambda (route rtable)
                                        (add-ipv4-route 
                                          rtable 
                                          route))
                                      (make <ipv4-table>)
                                      routes)))
                  (fold
                    (lambda (int table)
                      (add-ipv4-route 
                        table 
                        (make <ipv4-route>
                              #:net (make <ipv4-network>
                                          #:prefix (make <ipv4-address> #:ip (int))
                                          #:prefix-len 32
                                          #:gw "1.1.1.1")
                              #:misc (cons name routestable))))
                    maptable 
                    ints ))))
            (lambda (key . args)
              (format #t "failednode ~a ~a ~a~%" (slot-ref node 'ip) key args)
              #t))))
      (make <ipv4-table>)
      nodes))
  )

(define (main args)
  (let ((netmap (build-netmap)))
    (apply print-paths (append (list "192.168.0.10" "192.168.0.1" "192.168.40.10" "192.168.40.1") (list netmap))) 
    ))

(main (script-arguments))

