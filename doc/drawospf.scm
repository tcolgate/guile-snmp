#!/usr/bin/env snmp-shell -s
!#

; draw a graph of the current spanning tree configuration for
; a given vlan This example also uses getopt-long ot process
; command line arguments.

;(debug-reports #t)

(use-modules (ice-9 regex))
(use-modules (srfi srfi-11))

(use-mibs IP-MIB OSPF-MIB)

(primitive-load "./nodes-l3.scm")
;
;some 3750s take an age to response to BRIDGE-MIB
(default-session #:timeout 10000000)

(define (drawospf port)
  (format port "digraph OSPF {~%")
  (format port "rankdir=BT~%")
  (session
    #:community "K9gYhRWkXhX3Afx"
    (map
      (lambda(item)
        (let ((name (car item))
              (node (cdr item)))
          ;          (format (current-error-port) "Node: ~A~%" name)
          (catch
            #t
            (lambda()
              (session
                #:host (slot-ref node 'ip)
                #:community (slot-ref node 'community)
                #:version (slot-ref node 'version)
                (let ((rtrname    ((get sysName.0)))
                      (rtrid      ((get ospfRouterId.0)))
                      (rtrospfon  ((get ospfAdminStat.0)))
                      (rtrospfrbw ((get ospfReferenceBandwidth.0))))
                  (if (and (not (equal? 'noSuchObject rtrid))
                           (equal? 1 rtrospfon))
                   (begin
                      (format #t "\"rtr~a\" [ label = \"~a\\n~a\" ];~%" rtrid rtrname rtrid)
                      (let ((ospfneighfunc (walk-func ospfNbrIpAddr)))
                        (let neighloop ((neigh (ospfneighfunc)))
                          (let ((nrtrid (get (+ ospfNbrRtrId (iid neigh)))))
                            (let* ((ifs      (walk ifIndex)))
                              (let-values
                                (((cost name speed)  (let loop ((iflist ifs))
                                                       (let* ((testif (car iflist))
                                                              (testarp (get (+ (+ ipNetToMediaIfIndex (testif)) (% 1 4 (iid neigh))))))
                                                         (if(not (equal?
                                                                   'noSuchInstance
                                                                   (testarp)))
                                                           (let loop2 ((iplist (walk ipAdEntIfIndex)))
                                                             (let* ((testip (car iplist)))
                                                               (if (equal? (testip) (testif))
                                                                 (let ((cost (get (+ (+ ospfIfMetricValue (iid testip)) .0.0))))
                                                                   (if (not (equal?  'noSuchInstance (cost)))
                                                                     (values cost
                                                                             (get (+ ifName (testif)))
                                                                             (get (+ ifHighSpeed (testif))))
                                                                     (loop2 (cdr iplist))))
                                                                 (loop2 (cdr iplist)))))
                                                           (loop (cdr iflist)))))))
                                (format #t "\"rtr~a\" -> \"rtr~a\" [label = \"\\n~a\\n~aMb/s\\n~a\"];~%" rtrid (nrtrid) (name) (speed) (cost) ))
                              (neighloop (ospfneighfunc)))))))))))

            (lambda(key . args)
              #t))))
      nodes))

  (format port "}~%"))

(drawospf (current-output-port))

