;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate
;;
;; port-usage-reprot: this is a monolithic version of the switch
;; report using threading and other features of the guile-snmp
;;
;;-------------------------------------------------------------------

(set! %load-path (cons "/usr/local/bin" %load-path))
(use-modules (ice-9 threads))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 popen))
(use-modules (ice-9 format))
(use-modules (srfi srfi-37))
(use-modules (ice-9 session))
(use-modules (ice-9 regex))
(use-modules (ice-9 common-list))

(use-modules (snmp reports))
(init-reports)

; This is needed as "format", even with a ~! isn't atomic, so the
; output was all getting jumbled up
(current-output-port (open-output-file "allports-report.txt"))
(define outputmutex (make-mutex))
(define (output . args)
  (lock-mutex outputmutex)
  (apply format args)
  (unlock-mutex outputmutex))

; This is used to convert a switch name into the list of possible l3
; routers, we have 12 so querying them all was out of the question
;                                       "xxx-d   1   -rt   1   02"
(define site-and-region-re (make-regexp "...-.([0-2])-..([0-6]).."))
(define (possible-rtrs hostname)
  (let ((match (regexp-exec site-and-region-re hostname)))
    (list
      (regexp-substitute #f match "xxx-d" 1 "-rt" 2 "00")
      (regexp-substitute #f match "xxx-d" 1 "-rt" 2 "01"))))

(define (find-ip-from-mac comm mac)
  "This routine determines the ip for a given mac, if possible"
  (let routerloop ((routerlist (possible-rtrs (current-host))))
    (let* ((router  (car routerlist))
	   (therest (cdr routerlist))
           (res  (session #:host router #:community comm
	             (catch #t
		       (lambda()
                         (let ((fl3mac (walk-func ipNetToMediaPhysAddress)))
		           (let l3macloop ((l3mac (fl3mac)))
                             (let ((l3macip (% 2 5 (iid l3mac))))
                               (if (equal? (mac-as-oid (l3mac)) mac)
                                 (array->list l3macip)
			         (l3macloop (fl3mac)))))))
		       (lambda(ex . args) #f)))))
          (if (equal? res #f)
  	  (if (equal? therest '())
  	    #f
  	    (routerloop therest))
  	  res))))

(define (find-mac-on-port switch portid)
  "This finds the mac address seen on a given switch and portid"
  (session
    (let* ((intvlan (get (+ vmVlan portid)))
           (vlancomm (if (eq? (intvlan) 'noSuchInstance)
		         (current-community)
	                 (string-append (current-community) "@" (number->string (intvlan))))))
      (session #:community vlancomm
	(catch #t
          (lambda()
            (let ((fbportindex (walk-func dot1dBasePortIfIndex)))
	      (let bportindexloop ((bportindex (fbportindex)))
                (let ((bportid (iid bportindex)))
                  (if (equal? (bportindex) (% 1 portid))
		    (catch #t
                      (lambda()
  	  	        (let ((fbportmac (walk-func dot1dTpFdbPort)))
		          (let bportmacloop ((bportmac (fbportmac)))
                            (let* ((mac  (iid bportmac))
                                   (lmac (array->list mac)))
                              (if (equal? (bportmac) (% 1 bportid))
				(let ((lip  (find-ip-from-mac "mycommunity" mac)))
		  	          (with-output-to-string
			            (lambda()
				      (if (equal? lip #f)
                                        (output (current-output-port) "~2,'0x~{:~2,'0x~}~!" (car lmac) (cdr lmac))
                                        (output (current-output-port) "~A~{.~d~}(~2,'0x~{:~2,'0x~})~!" (car lip) (cdr lip) (car lmac) (cdr lmac))))))
		                (bportmacloop (fbportmac)))))))
		      (lambda(ex . args) "unknown"))
	            (bportindexloop (fbportindex)))))))
	  (lambda(ex . args) "unknown"))))))

(define (switchreport switch)
  " Report the port useage information for a given switch"
  (session #:host switch
    (let* ((switchname    ((get sysName.0)))
           (switchloc     ((get sysLocation.0)))
           (fporttype     (walk-func ifType)))
      (let portloop ((port (fporttype)))
	(let ((porttype (value port))
	      (portid (iid port)))
          (if (equal? porttype 6)
            (let* ((portname  ((get (+ ifName   portid))))
    	           (portdesc  ((get (+ ifAlias  portid))))
    	           (portastat ((get (+ ifAdminStatus  portid))))
    	           (portostat ((get (+ ifOperStatus   portid))))
    	           (cdpneighq (getnext  (+ CISCO-CDP-MIB::cdpCacheAddress portid)))
  	           (cdpneigh  (if (equal? (+ CISCO-CDP-MIB::cdpCacheAddress
  	                                     portid)
  	  		                (tag cdpneighq))
  	                        (ipstr-to-str (cdpneighq))
  	  		      #f))
	           (aggstat   ((get (+ IEEE8023-LAG-MIB::dot3adAggPortSelectedAggID portid))))
                   (cdpname   (if (not (eq? cdpneigh #f))
	                        (getnext (+ CISCO-CDP-MIB::cdpCacheDeviceId portid))
	  		      #f))
                   (cdpport   (if (not (eq? cdpneigh #f))
	                        (getnext (+ CISCO-CDP-MIB::cdpCacheDevicePort portid))
	  		      #f))
	           (neigh     (if (not (eq? cdpneigh #f))
	                        (string-append cdpneigh "(" (cdpname) ")")
	  		        (if (and (eq? 1 portastat)(eq? 1 portostat))
	  		  	  (if (eq? aggstat 0)
	  		            (find-mac-on-port switch portid)
				    "port channel")
				  "unknown")))
	           (neighport (if (not (eq? cdpneigh #f))
	                        (cdpport)
			        "unkown")))
              (output (current-output-port) "~a\t~a\t~a\t\"~a\"\t~a\t~a\t~a\t~a~%~!"
	        switch
	        switchloc
                portname
                portdesc
                (if (eq? portastat 1) "Up" "AdminDown")
                (if (eq? portostat 1) "Up" "Down")
	        neigh
	        neighport)))
              (portloop (fporttype)))))))


(define alldevs (list
;  "switch1" "switch2" ....))

(default-session #:community "mycommunity")

(enable-query-cache)

(output (current-output-port) "Switch\tLocation\tPort\tDescription\tAdmin Status\tOp Status\tRemote Host\tRemote Port~%~!")

(n-par-for-each 4
  (lambda(x)
    (output (current-error-port) "Starting ~A~%~!" x)
    (catch #t
      (lambda() (switchreport x))
      (lambda(ex . args)
	(if (not (eq? ex 'walkend))
	 (output (current-error-port) "During ~a: ~a ~a ~%~!" x ex args))))
    (output (current-error-port) "Done: ~A~%~!" x))
  alldevs)

; vim: ft=scheme:lisp:autoindent
