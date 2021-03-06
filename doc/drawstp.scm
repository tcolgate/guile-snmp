#!/usr/bin/env snmp-shell -s
!#

; draw a graph of the current spanning tree configuration for
; a given vlan This example also uses getopt-long ot process
; command line arguments.

(disable-query-cache)

(use-modules (ice-9 regex))

(use-mibs BRIDGE-MIB)

(primitive-load "./nodes.scm")

(define (mac bytes)
  (with-output-to-string
    (lambda()
      (format #t "~{~2,'0x~^:~}" (map char->integer (string->list bytes)) ))))

(define (prio int32)
  (let ((inthigh (bit-extract int32 0 7))
        (intlow  (bit-extract int32 8 16)))
    (with-output-to-string
      (lambda()
        (format #t "~2,'0x:~2,'0x" intlow inthigh )))))

(define (bridgeid m p)
  (with-output-to-string
    (lambda()
      (format #t "~a:~a" p m))))

(default-session #:timeout 10000000) ;some 3750s take an age to response to BRIDGE-MIB

(define (drawvlan vlan port)
  (format port "digraph STP {~%")
  (format port "rankdir=BT~%")
  (session
    #:community "public"
    (map
      (lambda(item)
        (let ((name (car item))
              (node (cdr item)))
          (catch
            #t
            (lambda()
              (session #:host (slot-ref node 'ip)
                       #:community (if (equal? vlan "1")
                                     (slot-ref node 'community)
                                     (string-append (slot-ref node 'community) "@" vlan))
                       #:version (slot-ref node 'version)
                       (let ((brname    ((get sysName.0)))
                             (brmacval  ((get dot1dBaseBridgeAddress.0)))
                             (brprioval ((get dot1dStpPriority.0))))
                         (if (and (not (equal? 'noSuchObject brmacval))
                                  (not (equal? 'noSuchInstance brprioval)))
                           (let*((brprio   (prio brprioval))
                                 (brmac    (mac brmacval))
                                 (brid     (bridgeid brmac brprio))
                                 (brdr     (mac ((get dot1dStpDesignatedRoot.0)))))

                             (if (equal? brid brdr)
                               (format #t "\"~a\"[label = \"~a\", penwidth = 3];~%" brid name)
                               (let* ((brdrprtid   ((get dot1dStpRootPort.0)))
                                      (drprtifindx (if (eq? brdrprtid 0) (throw 'error "gah") ((get (+ dot1dBasePortIfIndex brdrprtid)))))
                                      (drprtname   ((get (+ ifName drprtifindx))))
                                      (drprtspeed  ((get (+ ifHighSpeed drprtifindx))))
                                      (drprtbr     (mac ((get (+ dot1dStpPortDesignatedBridge brdrprtid))))))
                                 (format #t "\"~a\"[label = \"~a\"];~%" brid name) (begin
                                   (format #t "\"~a\" -> \"~a\" [ label = \"~a (~aMb/s)\" ] ;~%"
                                           brid
                                           drprtbr
                                           drprtname
                                           drprtspeed)
                                 (let ((dprtstatefunc (walk-func dot1dStpPortState)))
                                   (let dprtloop ((dprtstate (dprtstatefunc)))
                                     (if (equal? 2 (dprtstate)) ; blocked port
                                       (catch #t
                                              (lambda()
                                                (let* ((blkprt (iid dprtstate))
                                                       (blkbrid (mac ((get (+ dot1dStpPortDesignatedBridge blkprt)))))
                                                       (blkprtifindex ((get (+ dot1dBasePortIfIndex blkprt)))))
                                                  (format #t
                                                          "\"~a\" -> \"~a\" [ label = \"~a\", style = dashed ] ;~%"
                                                          brid
                                                          blkbrid
                                                          ((get (+ ifName blkprtifindex)))
                                                          )) )
                                              (lambda(key . args)
                                                #t)))
                                     (dprtloop (dprtstatefunc))))))))))))
            (lambda (key . args)
              #t))))
      nodes))
  (format port "}~%"))

(define (main args)
  (if (eq? args '())
    (drawvlan "1" #t)
    (drawvlan (car args) #t)))

(main (script-arguments))

