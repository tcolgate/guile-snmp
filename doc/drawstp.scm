#!/home/tcolgate/local/bin/snmp-shell -s
!#

; draw a graph of the current spanning tree configuration for 
; a given vlan

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

(default-session #:community "K9gYhRWkXhX3Afx")

(format #t "digraph g {~%")
(format #t "rankdir=BT~%")
;
(map 
  (lambda(item)
    (let ((name (car item))
          (node (cdr item)))
      (session #:host (slot-ref node 'ip) 
               #:community (slot-ref node 'community)
               #:version (slot-ref node 'version)
        (let* ((brname   ((get sysName.0)))
               (brmac    (mac ((get dot1dBaseBridgeAddress.0))))
               (brprio   (prio ((get dot1dStpPriority.0))))
               (brid     (bridgeid brmac brprio))
               (brdr     (mac ((get dot1dStpDesignatedRoot.0)))))
        
          (format #t "#Name:            ~a~%" brname)
          (format #t "#Bridge Mac:      ~a~%" brmac)
          (format #t "#Bridge Prio:     ~a(~4,'0x)~%" brprio ((get dot1dStpPriority.0)))
          (format #t "#Bridge Id:       ~a~%" brid)
          (format #t "#Designated Root: ~a~%" brdr)
          (format #t "br~a[label = \"~a\"];~%" brid name)
          (if (not (equal? brid brdr))
            (let* ((brdrprtid   ((get dot1dStpRootPort.0)))
                   (drprtifindx ((get (+ dot1dBasePortIfIndex brdrprtid))))
                   (drprtname   ((get (+ ifName drprtifindx))))
                   (drprtbr     (mac ((get (+ dot1dStpPortDesignatedBridge brdrprtid))))))
               ;(format #t "DR Port:         ~a~%" drprtname)
               ;(format #t "Next Bridge:     ~a~%" drprtbr)))))))
               (format #t "\"br~a\" -> \"br~a\" [ label = \"~a\" ] ;~%" brid drprtbr drprtname )))))))
  nodes)

(format #t "}~%")

