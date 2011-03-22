#!/usr/local/bin/snmp-shell -s
!#

; draw a graph of the current spanning tree configuration for 
; a given vlan This example also uses getopt-long ot process
; command line arguments.

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

(define (drawvlan port)
  (session #:community "K9gYhRWkXhX3Afx")

    (format port "digraph g {~%")
    (format port "rankdir=BT~%")

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
          
              (format port "br~a[label = \"~a\"];~%" brid name)
              (if (not (equal? brid brdr))
                (let* ((brdrprtid   ((get dot1dStpRootPort.0)))
                       (drprtifindx ((get (+ dot1dBasePortIfIndex brdrprtid))))
                       (drprtname   ((get (+ ifName drprtifindx))))
                       (drprtbr     (mac ((get (+ dot1dStpPortDesignatedBridge brdrprtid))))))
                   (format port "\"br~a\" -> \"br~a\" [ label = \"~a\" ] ;~%" brid drprtbr drprtname )))))))
    nodes)

    (format port "}~%"))

(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (help    (single-char #\h) (value #f))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f)))
    (if (or version-wanted help-wanted)
        (begin
          (if version-wanted
              (format #t "drawStp version 0.1~%"))
          (if help-wanted
              (format #t "\
getopt-long-example [options]
  -v, --version    Display version
  -h, --help       Display this help
")))
        (begin
          (drawvlan #t)))))

(main (script-arguments))
