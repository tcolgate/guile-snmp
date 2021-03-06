#!/usr/bin/env snmp-shell -s
!#
;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate
;;
;; find-host -  an example of using snmp-shell
;;
;;-------------------------------------------------------------------

;;; Commentary:
;;
;;  Useage: find-host [OPTIONS] host1 host2 ...
;;   This command locates the Layer 2 connection for a given hostname
;;  using a hard coded list of Layer 3 devices as the intial point of
;;  the query.
;;
;;; Code:

; You can set the MIBS used in your MIBS environment variable as per usual,
; or explicity specify additional MIBs in here...
(use-mibs ...)

; This is a list of all the layer3 routers to query
(define l3list (list
                 "192.168.0.254" "192.168.0.254"))

(define (find-host name)
  "find-host: Track a host to the layer2 switch and port it is connected to"
  (let* ((router (one-of l3list))
         (hostipstr (inet-ntoa (car(vector-ref (gethostbyname name) 4))))
         (hostipoid (list->oidvector (map string->number (string-split hostipstr  #\. )))))
    (session #:host (router)
      (let* ((arp (walk-on-fail ipNetToMediaPhysAddress))
             (ip (% 2 5 (iid arp)))
             (mac (arp)))
        (if (not (equal? hostipoid ip))
          (fail)
          (let* ((vlan (walk-on-fail vtpVlanState))
                 (vlanid (% 2 (iid vlan)))
                 (vlancomm (string-append (currnet-community) (string-append "@" (number->string vlanid)))))
            (session #:community vlancomm
              (let* ((mapint      (get (+ dot1dTpFdbPor (mac-as-oid mac))))
                     (validmapint (if (eq? (mapint) 'noSuchObject)
                                    (fail)
                                    (mapint)))
                     (intif       (get (+ dot1dBasePortIfIndex validmapint)))
                     (intifDescr  (value (get (+ ifDescr (intif)))))
                     (poports     (walk-on-fail pagpGroupIfIndex)))
                 (if (not (equal? (poports) (intif)))
                   (fail)
                   (let* ((neighbour (walk-on-fail (+ cdpCacheAddress (iid poports))))
                          (neighbourstr (ipstr-to-str (neighbour))))
                     (if (find-if (lambda(item)(equal? item neighbourstr)) l3list)
                       (fail) ; The neighbour is in the l3 list so we will check it anyway
                       (session #:host neighbourstr
                         (let* ((peername (get sysName.0))
                                (fdbport  (get (+ dot1dTpFdbPort (mac-as-oid mac))))
                                (fdbif    (get (+ dot1dBasePortIfIndex (fdbport))))
                                (port     (get (+ ifDescr (fdbif))))
                                (podesc   (get (+ ifAlias (fdbif)))))
                           (format #t "Host: ~a  ~% Via Router: ~A ~% VLAN: ~A ~% Neighbour: ~A ~% Name: ~A ~% Port: ~A ~% Desc: ~A ~%"  name (router) vlanid  neighbourstr (peername)(port)(podesc)))))))))))))))

;main
(catch 'noMoreAlternate
  (lambda()(find-host (car (script-arguments))))
  (lambda(ex . args)(format #t "Host not found on any layer 3 device~%")))

; vim: ft=scheme:lisp:autoindent
