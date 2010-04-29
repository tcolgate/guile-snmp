(define-module (ipv4-router)
  #:use-module (oop goops)
  #:use-module (srfi srfi-11)
  #:export 
    (<ipv4-address> ip 
     <ipv4-network> network prefix prefix-len mask
     prefixlen->intmask
     ip-in-network?
     <ipv4-route> route net gw
     <ipv4-table>
     add-ipv4-route
     add-ipv4-route!
     remove-ipv4-route
     remove-ipv4-route!
     find-ipv4-route
     value value-length
     trie-node
     trie-root
     trie-node->dot))

; A class to represent an ip address
(define (ipv4str->ipv4int ipstr)
  (if (string? ipstr)
    (car (vector-ref (gethostbyname ipstr) 4 ))
    (error "ipstr expects a string")))

(define-class <ipv4-address> ()
  (int-ip #:accessor int-ip 
          #:init-value 0
          #:init-keyword #:int-ip)
  (ip #:init-keyword #:ip
      #:allocation #:virtual
      #:slot-ref (lambda(this)(int-ip this))
      #:slot-set! (lambda(this val)(set! (int-ip this) val))
      #:accessor ip ))

(define-method ((setter int-ip) (this <ipv4-address>) (ip-string <string>))
  (slot-set! this 'int-ip (ipv4str->ipv4int ip-string)))

(define-method ((setter int-ip) (this <ipv4-address>) (ip-integer <integer>))
  (slot-set! this 'int-ip  ip-integer))

(define-method ((setter int-ip) (this <ipv4-address>) default)
  (format (current-error-port) "Cannot convert to IP: ~a~%" default)
  (throw 'error))

(define-method (display (this <ipv4-address>) port)
  (display (inet-ntoa (int-ip this))))

(define-method (write (this <ipv4-address>) port)
  (format port "#:ip(~s)" (inet-ntoa (int-ip this))))

; Class for representing a network
(define-class <ipv4-network> ()
  (prefix #:accessor prefix 
          #:init-value (make <ipv4-address> )
          #:init-keyword #:prefix)
  (prefix-len  #:accessor prefix-len 
               #:init-value 0
               #:init-keyword #:prefix-len)
  (network #:allocation #:virtual 
           #:accessor network
           #:slot-ref (lambda(this)(prefix this))
           #:slot-set! (lambda(this val)(set! (network this) val))
           #:init-keyword #:network)
  (mask #:allocation #:virtual 
        #:accessor mask
        #:slot-ref (lambda(this)(inet-ntoa (prefixlen->intmask (prefix-len this))))
        #:slot-set! (lambda(this val)(set! (mask this) val))
        #:init-keyword #:mask))

; setters
(define-method ((setter prefix) (this <ipv4-network>) (value <ipv4-address>))
  (slot-set! this 'prefix value))
(define-method ((setter prefix) (this <ipv4-network>) value)
  (slot-set! this 'prefix (make <ipv4-address> #:ip value)))

(define-method ((setter prefix-len) (this <ipv4-network>) (value <integer>))
  (slot-set! this 'prefix-len value))

(define-method ((setter mask) (this <ipv4-network>) (value <string>))
  (set! (mask this) (inet-aton value)))

(define-method ((setter mask) (this <ipv4-network>) (value <integer>))
  (let* ((intlen  (integer-length value))
         (len (- 32  (integer-length (logxor value (- (integer-expt 2 32) 1))))))
    (if (and (or (eq? intlen 0) (eq? intlen 32))
             (eq? len (logcount value)))
     (set! (prefix-len this) len)
     (error "invalid (or discontiguous) netmask"))))

(define (prefixlen->intmask preflen)
  (ash (- (integer-expt 2 preflen) 1) (- 32 preflen )))
 
(define-method ((setter network) (this <ipv4-network>) (networkspec <string>))
  (if (string-index networkspec #\/)
    (let ((spec (string-split networkspec #\/)))
      (set! (network this) (list (make <ipv4-address> #:ip (car spec)) (string->number (cadr spec)))))
    (if (string-index networkspec #\ )
      (let ((spec (string-split networkspec #\ )))
        (set! (network this) (list (make <ipv4-address> #:ip (car spec)) (cadr spec))))
      (error "Bad network specification"))))

(define-method ((setter network) (this <ipv4-network>) (networkspec <pair>))
  (let ((net (car networkspec))
        (maskspec (cadr networkspec)))
    (set! (prefix this) net)
    (cond 
       ((equal? (class-of maskspec) <string>)
        (set! (mask this) maskspec))
       ((equal? (class-of maskspec) <integer>)
        (set! (prefix-len this) maskspec))
       (else (throw 'error)))))

(define-method (display (this <ipv4-network>) port)
  (format port "~a/~a" (prefix this) (prefix-len this)))
(define-method (write (this <ipv4-network>) port)
  (format port "#:net(~s/~s)" (prefix this) (prefix-len this)))

(define (ip-in-network? testip testnet)
  (eqv? (logand (ip testip) (prefixlen->intmask (prefix-len testnet)))
          (ip (prefix testnet))))

; Class for representing a route
(define-class <ipv4-route> ()
  (_net #:init-value (make <ipv4-network> ))
  (_gw #:init-value (make <ipv4-network> ))
  (net #:allocation #:virtual
       #:accessor net 
       #:slot-ref (lambda(this)(slot-ref this '_net))
       #:slot-set! (lambda(this val)(set! (net this) val))
       #:init-keyword #:net)
  (gw #:allocation #:virtual
      #:accessor gw 
      #:slot-ref (lambda(this)(slot-ref this '_gw))
      #:slot-set! (lambda(this val)(set! (gw this) val))
      #:init-keyword #:gw))

; setters
(define-method ((setter net) (this <ipv4-route>) (val <ipv4-network>))
  (slot-set! this '_net val))
(define-method ((setter net) (this <ipv4-route>) val)
  (slot-set! this '_net (make <ipv4-network> #:network val)))

(define-method ((setter gw) (this <ipv4-route>) (val <ipv4-address>))
  (slot-set! this '_gw val))
(define-method ((setter gw) (this <ipv4-route>) val)
  (slot-set! this '_gw (make <ipv4-address> #:ip val)))

(define-method (display (this <ipv4-route>) port)
  (format port "~a via ~a" (net this) (gw this)))
(define-method (write (this <ipv4-route>) port)
  (format port "#:route(~s via ~s)" (net this) (gw this)))

; Class for implementing a compressed radix tree
;
(define-generic value-binstr)
(define-class <trie-node> ()
  (value         #:accessor value         
                 #:init-value  0 
                 #:init-keyword #:val)
  (value-binstr  #:allocation #:virtual   
                 #:slot-ref value-binstr 
                 #:slot-set! value-binstr
                 #:init-keyword #:valbin)
  (value-length  #:accessor value-length  
                 #:init-value 0 
                 #:init-keyword #:len)
  (left          #:accessor left          
                 #:init-value nullnode 
                 #:init-keyword #:l)
  (right         #:accessor right         
                 #:init-value nullnode
                 #:init-keyword #:r)
  (userdata      #:accessor userdata      
                 #:init-value #f 
                 #:init-keyword #:udata))
(define nullnode (make <trie-node>))
(define rootnode (make <trie-node> #:l nullnode #:r nullnode))

; setters
(define-method ((setter value) (this <trie-node>) (val <integer>))
  (slot-set! this 'value val))
(define-method ((setter value-length) (this <trie-node>) (len <integer>))
  (slot-set! this 'value-length len))
(define-method ((setter left) (this <trie-node>) (l <trie-node>))
  (slot-set! this 'left l))
(define-method ((setter right) (this <trie-node>) (r <trie-node>))
  (slot-set! this 'right r))
(define-method ((setter userdata) (this <trie-node>) ud)
  (slot-set! this 'userdata ud))

; virtual getters
(define-method (value-binstr (this <trie-node>))
  (with-output-to-string
    (lambda()
      (format #t 
        (string-append
          (string-append "~" 
            (string-append (number->string (value-length this))))
          ",'0b")
        (value this)))))

(define (shared-prefix v1 l1 v2 l2)
  "Takes two binary string (number and length) and returns, using multiple values
   the common prefix and its length"
  (cond 
     ((= l1 l2)
      (let again ((rotv1  v1)
                  (rotv2  v2)
                  (l      l1))
        (if (eq? rotv1 rotv2)
            (values rotv1 l)
            (again (ash rotv1 -1) (ash rotv2 -1) (- l 1)))))
     ((> l1 l2)
      (shared-prefix (ash v1 (- l2 l1)) l2 v2 l2))
     ((< l1 l2)
      (shared-prefix v1 l1 (ash v2 (- l1 l2)) l1))))
  
(define-generic add-trie-node)
(define-method (add-trie-node (root <trie-node>) 
                              (val <integer>) 
                              (len <integer>) 
                              udata)
  (if (eq? root nullnode)
    ; We are being asked to add a new leaf
    (make <trie-node> #:val  val #:len len #:l nullnode #:r nullnode #:udata udata)
    ; Trying to add to an existing node
    (let-values (((common-pref common-len) (shared-prefix (value root)(value-length root)
                                            val len)))
      (if (eq? common-len len)
        ; We are an exact match for this prefix
        (if (equal? udata (userdata root))
           ; We are an exact match for this node, just return
           root
           ; Userdata is different
           (make <trie-node> #:val (value root) 
                             #:len (value-length root) 
                             #:l (left root) 
                             #:r (right root)
                             #:udata udata))
        (if (= common-len (value-length root))
          ; We are adding to a sub tree
          (if (not (logbit? (- (- len common-len) 1) val))
            ;add to left for a zero
            (let* ((nextval (logand val (- (integer-expt 2 (- len common-len )) 1)))
                   (nextlen (- len common-len))
                   (nextnode (add-trie-node (left root) nextval nextlen udata)))
              (if (eq? nextnode (left root))
                  root
                  (make <trie-node> #:val (value root) 
                                    #:len (value-length root) 
                                    #:l nextnode 
                                    #:r (right root)
                                    #:udata (userdata root))))
            ;add to right for a one 
            (let* ((nextval (logand val (- (integer-expt 2 (- len common-len )) 1)))
                   (nextlen (- len common-len))
                   (nextnode (add-trie-node (right root) nextval nextlen udata)))
              (if (eq? nextnode (right root))
                  root
                  (make <trie-node> #:val (value root) 
                                    #:len (value-length root) 
                                    #:l (left root) 
                                    #:r nextnode
                                    #:udata (userdata root)))))
          ;(< common-len (value-length root))
          ; We need to split our current node)
          (let* ((common-remains (make <trie-node> 
                                    #:val
                                    (logand (value root) 
                                            (- (integer-expt 2 
                                                             (- (value-length root) 
                                                                common-len)) 
                                               1))
                                    #:len
                                    (- (value-length root) common-len)
                                    #:l
                                    (left root)
                                    #:r
                                    (right root)
                                    #:udata
                                    (userdata root)))
                 (common-newdata (make <trie-node>
                                    #:val
                                    (logand val
                                            (- (integer-expt 2 
                                                             (- len 
                                                                common-len)) 
                                               1))
                                    #:len
                                    (- len common-len)
                                    #:l
                                    nullnode
                                    #:r
                                    nullnode
                                    #:udata
                                    udata))
                 (nextleft       (if (not (logbit? (+ (value-length common-remains) 1)
                                                   (value common-remains)))
                                    common-remains
                                    common-newdata))
                 (nextright      (if (eq? nextleft common-remains)
                                    common-newdata
                                    common-remains)))
            (make <trie-node> #:val common-pref #:len common-len #:l nextleft #:r nextright #:udata #f)))))))

(define-generic delete-trie-node)
(define-method (delete-trie-node (rootnode <trie-node>) (prefix <integer>)))

(define-generic longest-prefix-match)
(define-method (longest-prefix-match (root <trie-node>) (val <integer>) (len <integer>))
  (let  again ((currnode root)
               (currval val)
               (currlen len)
               (prevmatch 0)
               (prevmatchlen 0)
               (prevnode #f)
               (bestmatch 0) 
               (bestmatchlen 0) 
               (bestmatchnode #f)) 
   (if (eq? currnode #f)
     (values bestmatch bestmatchlen bestmatchnode)
     (let-values (((common-pref common-len) (shared-prefix (value currnode)(value-length currnode)
                                             currval currlen)))
       (cond 
         ; We have gone as far as we can, prevmatch holds the longest full match
         ((< common-len (value-length currnode))
          (values bestmatch bestmatchlen bestmatchnode))
         ; This node is an exact match))) 
         ((eq? common-len currlen) 
          (values
             (logand (ash prevmatch currlen) currval)
             (+ currlen prevmatchlen)
             currnode))
         (else 
           (letrec ((nextval (logand currval (- (integer-expt 2 (- currlen (value-length currnode))) 1))) 
                    (nextlen (- currlen (value-length currnode))))
             (again  
                 (if (logbit? (- nextlen 1) nextval)
                   (right currnode)
                   (left currnode))
                 nextval
                 nextlen
                 (logior (ash prevmatch (value-length currnode)) (value currnode))
                 (+ (value-length currnode) prevmatchlen)
                 currnode
                 (if (eq? (userdata currnode) #f)
                    bestmatch
                    (logior (ash prevmatch (value-length currnode)) (value currnode)))
                 (if (eq? (userdata currnode) #f)
                    bestmatchlen
                    (+ (value-length currnode) prevmatchlen))
                 (if (eq? (userdata currnode) #f)
                    bestmatchnode
                    currnode)))))))))
  

; Display methods
(define-method (display (this <trie-node>) port)
  (if (eq? this nullnode)
    (format port "null")
    (format port "( ~b ~a ~a ~a ~a)"
                 (value this)
                 (value-length this)
                 (left this)
                 (right this)
                 (userdata this))))
(define-method (write (this <trie-node>) port)
  (if (eq? this nullnode)
    (format port "null")
    (format port "#:trie-node(val: ~b len: ~a l: ~a r: ~a ud: ~a)" 
                 (value this)
                 (value-length this)
                 (left this)
                 (right this)
                 (userdata this))))

(define (walk-trie rootnode func)
  (if (not (eq? #f rootnode))
    (begin
      (func rootnode)
      (walk-trie (left rootnode) func)
      (walk-trie (right rootnode) func))))

(define (trie-node->dot port start)
  (format port "digraph G {~%")
  (walk-trie start (lambda(node)
                     (if (not (eq? node nullnode))
                       (if (not (eq? (value-length node) 0))
                         (format #t "~a [label=~s];~%" (object-address node)(with-output-to-string (lambda()(format #t "~a via ~a" (value-binstr node) (userdata node)))))
                         (format #t "~a [label=\"root\"];~%" (object-address node))))))
  (walk-trie start (lambda(node)
                     (if (not (eq? node nullnode))
                       (begin
                         (if (not (eq? (left node) nullnode))
                           (format #t "~a->~a;~%" (object-address node)
                                                   (object-address (left node))))
                         (if (not (eq? (right node) nullnode))
                           (format #t "~a->~a;~%" (object-address node)
                                                   (object-address (right node))))))))
  (format port "}~%"))

; Wrapper around compressed radix code to support ipv4 routing table.
(define-class <ipv4-table> ()
  (trie-root #:init-value rootnode 
             #:accessor trie-root))
`
(define-generic add-ipv4-route)
(define-method (add-ipv4-route (table <ipv4-table>)(route <ipv4-route>))
  (let* ((newtable    (make <ipv4-table>))
         (addr    (net route))
         (pref    (ip (prefix addr)))
         (preflen (prefix-len addr)))
    (set! (trie-root newtable) (add-trie-node (trie-root table) (ash pref (- preflen 32)) preflen (ip(gw route))))
    newtable))

(define-generic add-ipv4-route!)
(define-method (add-ipv4-route! (table <ipv4-table>)(route <ipv4-route>))
  (let*((addr    (net route))
        (pref    (ip (prefix addr)))
        (preflen (prefix-len addr))) 
    (set! (trie-root table) (add-trie-node (trie-root table) (ash pref (- preflen 32)) preflen (ip(gw route))))))

(define-generic remove-ipv4-route)
(define-method (remove-ipv4-route (table <ipv4-table>)(route <ipv4-route>))
  (let ((newtable    (make <ipv4-table>)))
    (set! (trie-root newtable) (remove-trie-node (trie-root table) (net route) (ip (gw route))))
    newtable))

(define-generic remove-ipv4-route!)
(define-method (remove-ipv4-route! (table <ipv4-table>)(route <ipv4-route>))
  (set! (trie-root table) (remove-trie-node (trie-root table) (net route) (ip (gw route)))))

(define-generic find-ipv4-route)
(define-method (find-ipv4-route (table <ipv4-table>)(route <ipv4-address>))
  (let-values (((foundpref foundlen foundnode) 
                                  (longest-prefix-match
                                     (trie-root table)
                                     (ip route)
                                     32)))
    (let ((foundnet (make <ipv4-network>))
          (foundroute (make <ipv4-route>)))
      (prefix foundnet (ash foundpref (- 32 foundlen)))
      (prefix-len foundnet foundlen)
      (net foundroute foundnet)
      (gw foundroute (userdata foundnode))
      foundroute)))







