;;-------------------------------------------------------------------
;; Copyright (C) 2009-2012 Tristan Colgate
;;
;; net-snmp.scm - This file defined the basic library wrapper
;;
;;-------------------------------------------------------------------

(define-module (snmp net-snmp))
 
(define-macro (re-export name)
  `(begin
     (define ,name ,(string->symbol
		       (string-append
		         "primitive:"
		         (symbol->string name))))
     (export! ,name)))

(eval-when (eval load compile)

  (use-modules (oop goops))
  (use-modules (srfi srfi-39))
  (use-modules (ice-9 pretty-print))

  ; The module will hook these up with architecture specific
  ; srfi-4 routines
  (define empty-oidvec #f)
  (define oidvector #f)
  (define make-oidvector #f)
  (define oidvector? #f)
  (define oidvector-length #f)
  (define list->oidvector #f)
  (define oidvector->list #f)
  (define oidvector-ref #f)
  (define oidvector-set! #f)

  (define-class <oid> ()
		(_vec #:init-form empty-oidvec
		      #:init-keyword #:value))

  (define oid-translate (make-parameter #t))

  (define-method (display (this <oid>) port)
		 (if (oid-translate)
		   (if (> (length (oid->list this)) 0) 
		     (let* ((node     (get-tree this (get-tree-head)))
			    (basename (slot-ref node 'label))
			    (diff     (- (oid-from-tree-node node) this)))
		       (format port "~a~{.~d~}" basename (oid->list diff))) 
		     (format port "(empty-oid)" ))
		   (format port "~{.~d~}" (oid->list this))))

  (define-method (write (this <oid>) port)
		 (if (oid-translate)
		   (if (> (length (oid->list this)) 0) 
		     (let* ((node     (get-tree this (get-tree-head)))
			    (basename (slot-ref node 'label))
			    (diff     (- (oid-from-tree-node node) this)))
		       (format port "#<oid: ~a~{.~d~}>#" basename (oid->list  diff))) 
		     (format port "#<oid: (empty-oid>#")) 
		   (format port "#<oid: ~{.~d~}>#" (oid->list this))))

  (define-method (equal? (a <oid>) (b <oid>))
		 (equal? (slot-ref a '_vec) (slot-ref  b '_vec)))

  (define-method (equal? (a <oid>) b)
		 (equal? (slot-ref a '_vec) b))

  (define-method (equal? (a <oid>) b)
		 (equal? (slot-ref a '_vec) b))

  (define-method (equal? a (b <oid>))
		 (equal? a (slot-ref  b '_vec)))

  (define-method (oid->list (this <oid>))
		 (oidvector->list (slot-ref  this '_vec)))

  (define-method (list->oid this)
		 (make <oid> #:value (list->oidvector this)))

  (export 
    <oid> 
    oid-translate
    oidvector 
    make-oidvector 
    oidvector? 
    oidvector-length
    list->oidvector 
    oidvector->list
    oidvector-ref
    oidvector-set!
    list->oid 
    oid->list 
    empty-oidvec)

  ; TODO: This needs to track the library version really
  (load-extension "libguile_snmp_net-snmp" "scm_init_snmp_net_snmp_module"))

(use-modules (oop goops))
(use-modules ((snmp net-snmp-primitive) :renamer (symbol-prefix-proc 'primitive:)))

;(define guile-snmp-async-response primitive:guile-snmp-async-response)

(define-class <snmp-constant> ()
  (value #:init-keyword #:value) 
  (symbol #:init-keyword #:symbol)) 

(define-method (display (this <snmp-constant>) port)
  (format port "~a" (slot-ref this 'symbol)))
(define-method (write (this <snmp-constant>) port)
  (format port "~a" (slot-ref this 'symbol)))

(define constant-classes (make-hash-table 20))
(export constant-classes)

(define-syntax define-constant-class
  (syntax-rules ()
    ((_ name)
     (begin
       (define-class name (<snmp-constant>))
       (hashq-set! constant-classes name (make-hash-table 32))  
       (export name)))))

(define-syntax define-constant
  (syntax-rules ()
    ((_ type name)
     (begin
       (define name (make type 
                          #:value (local-ref (list  
                          (string->symbol (string-append "primitive:_wrap_" 
                                          (symbol->string (quote name))))))
			  #:symbol (quote name)))
       (hashq-set! (hashq-ref constant-classes type) 
		   (local-ref (list  (string->symbol (string-append "primitive:_wrap_" 
                                          (symbol->string (quote name)))))) 
		   name) 
       (export name)))))

(define (constant-name-from-value class val)
  (hashq-ref (hashq-ref constant-classes (module-ref (current-module ) class)) val))
(primitive:set-constant-name-from-value-hook! constant-name-from-value)

(define-constant-class <snmp-version>)
(define-constant <snmp-version> SNMP-VERSION-1) 
(define-constant <snmp-version> SNMP-VERSION-2c) 
(define-constant <snmp-version> SNMP-VERSION-2u) 
(define-constant <snmp-version> SNMP-VERSION-3) 
(define-constant <snmp-version> SNMP-VERSION-sec) 
(define-constant <snmp-version> SNMP-VERSION-2p) 
(define-constant <snmp-version> SNMP-VERSION-2star) 

(define-constant-class <snmp-msg>)
(define-constant <snmp-msg> SNMP-MSG-GET)
(define-constant <snmp-msg> SNMP-MSG-GETNEXT)
(define-constant <snmp-msg> SNMP-MSG-RESPONSE)
(define-constant <snmp-msg> SNMP-MSG-SET)
(define-constant <snmp-msg> SNMP-MSG-TRAP)
(define-constant <snmp-msg> SNMP-MSG-GETBULK)
(define-constant <snmp-msg> SNMP-MSG-INFORM)
(define-constant <snmp-msg> SNMP-MSG-TRAP2)
(define-constant <snmp-msg> SNMP-MSG-REPORT)

(define-constant-class <asn-type>)
(define-constant <asn-type> ASN-BOOLEAN)
(define-constant <asn-type> ASN-INTEGER)
(define-constant <asn-type> ASN-BIT-STR)
(define-constant <asn-type> ASN-NULL)
(define-constant <asn-type> ASN-OBJECT-ID)
(define-constant <asn-type> ASN-SEQUENCE)
(define-constant <asn-type> ASN-SET)
(define-constant <asn-type> ASN-OCTET-STR)
(define-constant <asn-type> ASN-IPADDRESS)
(define-constant <asn-type> ASN-COUNTER)
(define-constant <asn-type> ASN-GAUGE)
(define-constant <asn-type> ASN-UNSIGNED)
(define-constant <asn-type> ASN-TIMETICKS)
(define-constant <asn-type> ASN-OPAQUE)
(define-constant <asn-type> ASN-NSAP)
(define-constant <asn-type> ASN-COUNTER64)
(define-constant <asn-type> ASN-UINTEGER)
(define-constant <asn-type> ASN-FLOAT)
(define-constant <asn-type> ASN-DOUBLE)
(define-constant <asn-type> ASN-INTEGER64)
(define-constant <asn-type> ASN-UNSIGNED64)
(define-constant <asn-type> SNMP-NOSUCHOBJECT)
(define-constant <asn-type> SNMP-NOSUCHINSTANCE)
(define-constant <asn-type> SNMP-ENDOFMIBVIEW)

(define-constant-class <snmp-status>)
(define-constant <snmp-status> STAT-SUCCESS)
(define-constant <snmp-status> STAT-ERROR)
(define-constant <snmp-status> STAT-TIMEOUT)

(define-constant-class <snmp-err-status>)
(define-constant <snmp-err-status> SNMP-ERR-NOERROR)
(define-constant <snmp-err-status> SNMP-ERR-TOOBIG)
(define-constant <snmp-err-status> SNMP-ERR-NOSUCHNAME)
(define-constant <snmp-err-status> SNMP-ERR-BADVALUE)
(define-constant <snmp-err-status> SNMP-ERR-READONLY)
(define-constant <snmp-err-status> SNMP-ERR-GENERR)
(define-constant <snmp-err-status> SNMP-ERR-NOACCESS)
(define-constant <snmp-err-status> SNMP-ERR-WRONGTYPE)
(define-constant <snmp-err-status> SNMP-ERR-WRONGLENGTH)
(define-constant <snmp-err-status> SNMP-ERR-WRONGENCODING)
(define-constant <snmp-err-status> SNMP-ERR-WRONGVALUE)
(define-constant <snmp-err-status> SNMP-ERR-NOCREATION)
(define-constant <snmp-err-status> SNMP-ERR-INCONSISTENTVALUE)
(define-constant <snmp-err-status> SNMP-ERR-RESOURCEUNAVAILABLE)
(define-constant <snmp-err-status> SNMP-ERR-COMMITFAILED)
(define-constant <snmp-err-status> SNMP-ERR-UNDOFAILED)
(define-constant <snmp-err-status> SNMP-ERR-AUTHORIZATIONERROR)
(define-constant <snmp-err-status> SNMP-ERR-NOTWRITABLE)
(define-constant <snmp-err-status> SNMP-ERR-INCONSISTENTNAME)

(define-constant-class <mib-type>)
(define-constant <mib-type> MIB-TYPE-OTHER)
(define-constant <mib-type> MIB-TYPE-OBJID)
(define-constant <mib-type> MIB-TYPE-OCTETSTR)
(define-constant <mib-type> MIB-TYPE-INTEGER)
(define-constant <mib-type> MIB-TYPE-NETADDR)
(define-constant <mib-type> MIB-TYPE-IPADDR)
(define-constant <mib-type> MIB-TYPE-COUNTER)
(define-constant <mib-type> MIB-TYPE-GAUGE)
(define-constant <mib-type> MIB-TYPE-TIMETICKS)
(define-constant <mib-type> MIB-TYPE-OPAQUE)
(define-constant <mib-type> MIB-TYPE-NULL)
(define-constant <mib-type> MIB-TYPE-COUNTER64)
(define-constant <mib-type> MIB-TYPE-BITSTRING)
(define-constant <mib-type> MIB-TYPE-NSAPADDRESS)
(define-constant <mib-type> MIB-TYPE-UINTEGER)
(define-constant <mib-type> MIB-TYPE-UNSIGNED32)
(define-constant <mib-type> MIB-TYPE-INTEGER32)
(define-constant <mib-type> MIB-TYPE-SIMPLE-LAST)
(define-constant <mib-type> MIB-TYPE-TRAPTYPE)
(define-constant <mib-type> MIB-TYPE-NOTIFTYPE)
(define-constant <mib-type> MIB-TYPE-OBJGROUP)
(define-constant <mib-type> MIB-TYPE-NOTIFGROUP)
(define-constant <mib-type> MIB-TYPE-MODID)
(define-constant <mib-type> MIB-TYPE-AGENTCAP)
(define-constant <mib-type> MIB-TYPE-MODCOMP)
(define-constant <mib-type> MIB-TYPE-OBJIDENTITY)

(define-constant-class <mib-access>)
(define-constant <mib-access> MIB-ACCESS-READONLY)
(define-constant <mib-access> MIB-ACCESS-READWRITE)
(define-constant <mib-access> MIB-ACCESS-WRITEHONLY)
(define-constant <mib-access> MIB-ACCESS-NOACCESS)
(define-constant <mib-access> MIB-ACCESS-NOTIFY)
(define-constant <mib-access> MIB-ACCESS-CREATE)

(define-constant-class <mib-status>)
(define-constant <mib-status> MIB-STATUS-MANDATORY)
(define-constant <mib-status> MIB-STATUS-OPTIONAL)
(define-constant <mib-status> MIB-STATUS-OBSOLETE)
(define-constant <mib-status> MIB-STATUS-DEPRECATED)
(define-constant <mib-status> MIB-STATUS-CURRENT)

(define-constant-class <snmp-sec-level>)
(define-constant <snmp-sec-level> SNMP-SEC-LEVEL-NOAUTH)
(define-constant <snmp-sec-level> SNMP-SEC-LEVEL-AUTHNOPRIV)
(define-constant <snmp-sec-level> SNMP-SEC-LEVEL-AUTHPRIV)

(define-constant-class <callback-op>)
(define-constant <callback-op> OP-RECEIVED-MESSAGE)
(define-constant <callback-op> OP-TIMED-OUT)
(define-constant <callback-op> OP-SEND-FAILED)
(define-constant <callback-op> OP-CONNECT)
(define-constant <callback-op> OP-DISCONNECT)

(define-syntax define-class-wrapped-struct
  (lambda(stx)
    (let* ((input (syntax->datum stx))
           (type  (cadr input))
           (slots  (cddr input))
           (class (string->symbol (string-append "<"  (symbol->string type) ">")))
           (primname (string->symbol (string-append "primitive:"  (symbol->string class))))
           (initfunc (string->symbol (string-append "primitive:initialize-"  (symbol->string type))))
	   (slotdefs (let* ((p "primitive:")) 
		       (map
			 (lambda (slot)
			   (let* ((pref  (string-append p 
							(symbol->string type) 
							"-"
							(symbol->string slot)))
				  (sget  (string->symbol (string-append pref "-get"))) 
				  (sset  (string->symbol (string-append pref "-set")))
				  (sacc  (string->symbol (string-append p (symbol->string slot)))))
			     `(,slot #:allocation #:virtual
				     #:slot-ref  ,sget
				     #:slot-set! ,sset
				     #:accessor ,sacc)))
			 slots)))
	   (slotexps (map
			(lambda (slot)
			     `(re-export ,slot))
			 slots)))
      
        (datum->syntax stx 
		     `(begin
			(define-class ,class()
				      ptr
				      ,@slotdefs
				      #:name (quote ,class))
			(define-method (initialize (obj ,class) initargs)
			   (,initfunc obj initargs)
                           (next-method))
			(export ,class)
			,@slotexps)))))

(define-class-wrapped-struct tree label description type access status display-hint 
			     units enums indexes varbinds parent peers children) 

; These provide a nicer interface to the tree structure when accessed as oids.
; When used with trees these return types consistent with the raw net-snmp structure
(define-method (description (o <oid>)) (description (get-tree o (get-tree-head)))) 
(define-method (type (o <oid>)) (mib-to-asn-type (type (get-tree o (get-tree-head))))) 
(define-method (access (o <oid>)) (access (get-tree o (get-tree-head)))) 
(define-method (status (o <oid>)) (status (get-tree o (get-tree-head)))) 
(define-method (display-hint (o <oid>)) (display-hint (get-tree o (get-tree-head)))) 
(define-method (units (o <oid>)) (units (get-tree o (get-tree-head)))) 
(define-method (enums (o <oid>)) (enums (get-tree o (get-tree-head)))) 
(define-method (varbinds (o <oid>)) (map snmp-parse-oid (varbinds (get-tree o (get-tree-head))))) 
(define-method (parent (o <oid>)) (oid-from-tree-node (parent (get-tree o (get-tree-head))))) 
(define-method (peers (o <oid>)) (map oid-from-tree-node  (peers (get-tree o (get-tree-head))))) 
(define-method (children (o <oid>)) (map oid-from-tree-node (children (get-tree o (get-tree-head))))) 
(define-method (indexes (o <oid>)) (map snmp-parse-oid (indexes (get-tree o (get-tree-head))))) 
(define indicies indexes) 
(export indicies)

(define-method (display (this <tree>) port)
	       (format port "<tree: ~a>" (oid-from-tree-node this)))
(define-method (write (this <tree>) port)
	       (format port "#<tree: ~a>" (oid-from-tree-node this)))

(define-method (describe (obj <oid>)) 
               (format #t "~%OID ~a (~{~d~^.~}):~%~%" obj (oid->list obj))                           

               (format #t "~@[Status: ~a~%~]" (status obj)) 
               (format #t "~@[Access: ~a~%~%~]" (access obj)) 

               (format #t "Description: ~a~%~%" (description obj))

               (format #t "~@[Type: ~a~%~]" (if (eq? '() (varbinds obj)) 
                                              (type obj)
                                              'NOTIFICATION-TYPE)) 
               (format #t "~@[Display-Hint: ~a~%~]" (display-hint obj)) 
               (format #t "~@[Units: ~a~%~]" (units obj)) 
               (format #t "~@[Indicies:~%~{  ~a~%~}~]" 
                       (if (eq? '() (indexes obj))
                         #f 
                         (indexes obj))) 
               (format #t "~@[Rows:~%~{  ~a~%~}~]" 
                       (if (eq? '() (indexes obj))
                         #f 
                         (children obj))) 
               (format #t "~@[Enumerations:~%~:{  ~a(~a)~%~}~]" 
                       (if (eq? '() (enums obj))
                         #f 
                         (map 
                           (lambda (e)
                             (list (cdr e) (car e)))
                           (enums obj)))) 
               (format #t "~@[Trap Varbinds:~%~{  ~a~%~}~]" 
                       (if (eq? '() (varbinds obj))
                         #f 
                         (varbinds obj)))
               (format #t "~@[Children:~%~{  ~a~%~}~]" 
                       (if (or (not (eq? '() (indexes obj)))
                               (eq? '() (children obj))) 
                         #f 
                         (children obj)))
               (newline))
(export describe)

(define-class-wrapped-struct snmp-session community peername localname local-port 
			     version context timeout retries callback securityName 
			     securityLevel securityAuthProto securityAuthKey 
			     securityPrivProto securityPrivKey)

(define-class-wrapped-struct snmp-single-session)
(define-class-wrapped-struct pdu errstat variables non-repeaters max-repetitions)
(define-class-wrapped-struct pdu-variable name type value)
(define-class-wrapped-struct snmp-fdinfo)
(define-class-wrapped-struct netsnmp-transport)

(re-export init-mib)
(re-export init-snmp)

(re-export read-module)
(re-export snmp-set-save-descriptions)
(re-export snmp-parse-oid)

(re-export get-tree)
(re-export get-tree-head)

(re-export oid-from-tree-node)

(re-export snmp-sess-init)
(re-export snmp-open)
(re-export snmp-synch-response)
(re-export snmp-send)
(re-export snmp-async-send)
(re-export snmp-read)
(re-export snmp-close)
(re-export snmp-select-info)
(re-export snmp-timeout)

(re-export snmp-sess-open)
(re-export snmp-sess-session)
(re-export snmp-sess-synch-response)
(re-export snmp-sess-send)
(re-export snmp-sess-async-send)
(re-export snmp-sess-read)
(re-export snmp-sess-close)
(re-export snmp-sess-error)
(re-export snmp-sess-select-info)
(re-export snmp-sess-timeout)

(re-export snmp-select)

(re-export snmp-pdu-create)
(re-export snmp-add-null-var)
(re-export snmp-add-var)
(re-export snmp-free-pdu)

(re-export netsnmp-tdomain-transport)
(re-export snmp-add)
(re-export snmp-sess-add)

(re-export netsnmp-oid-is-subtree)
(re-export mib-to-asn-type)

(re-export init-agent)
(re-export init-master-agent)
(re-export snmp-set-agent-agentx)
(re-export init-vacm-vars)
(re-export init-usm-users)
(re-export netsnmp-daemonize)
(re-export agent-check-and-process)
(re-export snmp-shutdown)
