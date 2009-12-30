(define-module (ipv4-router)
  #:use-module (oop goops)
  #:use-module (srfi srfi-11)
  #:export 
    (ip new-ip
     network new-network prefix prefix-len mask
     prefixlen->intmask
     ip-in-network?
     route new-route net gw
     add-ipv4-route
     remove-ipv4-route
     find-ipv4-route
     new-ipv4-table value value-length
     trie-node->dot))

; A class to represent an ip address
(define-class <ipv4-address> ()
  (ip #:accessor ip #:init-value 0))

(define-method (ip (this <ipv4-address>) (ip-int <integer>))
  (slot-set! this 'ip ip-int))

(define (ipv4str->ipv4int ipstr)
  (if (string? ipstr)
    (car (vector-ref (gethostbyname ipstr) 4 ))
    (error "ipstr expects a string")))

(define-method (ip (this <ipv4-address>) (ip-str <string>))
  (ip this (ipv4str->ipv4int ip-str)))

(define-method (ip (this <ipv4-address>) default)
  (error "Can't convert to ip"))

(define-generic new-ip)
(define-method (new-ip)
  (let* ((new-ip (make <ipv4-address>)))
    (ip new-ip 0)
    new-ip))

(define-method (new-ip ip-address)
  (let* ((new-ip (make <ipv4-address>)))
    (ip new-ip ip-address)
    new-ip))

(define-method (display (this <ipv4-address>) port)
  (display (inet-ntoa (ip this))))
(define-method (write (this <ipv4-address>) port)
  (format port "#:ip(~s)" (inet-ntoa (ip this))))
  
; Class for representing a network
(define-generic network)
(define-generic mask)
(define-class <ipv4-network> ()
  (network #:allocation #:virtual #:slot-set! network #:slot-ref network)
  (mask #:allocation #:virtual #:slot-set! mask #:slot-ref mask)
  (prefix #:accessor prefix #:init-value (new-ip 0))
  (prefix-len  #:accessor prefix-len #:init-value 0))

; setters
(define-method (prefix (this <ipv4-network>) (value <ipv4-address>))
  (slot-set! this 'prefix value))
(define-method (prefix (this <ipv4-network>) value)
  (slot-set! this 'prefix (new-ip value)))

(define-method (prefix-len (this <ipv4-network>) (value <integer>))
  (slot-set! this 'prefix-len value))

(define-method (mask (this <ipv4-network>) (value <integer>))
  (let* ((intlen  (integer-length value))
         (len (- 32  (integer-length (logxor value (- (integer-expt 2 32) 1))))))
    (if (and (or (eq? intlen 0) (eq? intlen 32))
             (eq? len (logcount value)))
     (prefix-len this len)
     (error "invalid (or discontiguous) netmask"))))
(define-method (mask (this <ipv4-network>) (value <string>))
  (mask this (inet-aton value)))

(define (prefixlen->intmask preflen)
  (ash (- (integer-expt 2 preflen) 1) (- 32 preflen )))
 
(define-method (mask (this <ipv4-network>))
    (inet-ntoa ((prefixlen->intmask (prefix-len this)))))

(define-method (network (this <ipv4-network>) (networkspec <string>))
  (if (string-index networkspec #\/)
    (let ((spec (string-split networkspec #\/)))
      (network this (new-ip (car spec)) (string->number (cadr spec))))
    (if (string-index networkspec #\ )
      (let ((spec (string-split networkspec #\ )))
        (network this (new-ip (car spec)) (cadr spec)))
      (error "Bad network specification"))))

(define-method (network (this <ipv4-network>) net (maskspec <string>))
  (prefix this net)
  (mask this maskspec))
(define-method (network (this <ipv4-network>) net (preflen <integer>))
  (prefix this net)
  (prefix-len this preflen))

;constructors
(define-generic new-network)
(define-method (new-network)
  (make <ipv4-network>))
(define-method (new-network netspec)
  (let ((new-net (make <ipv4-network>)))
    (network new-net netspec)
    new-net))
(define-method (new-network netspec maskspec)
  (let ((new-net (make <ipv4-network>)))
    (network new-net netspec maskspec)
    new-net))

(define-method (display (this <ipv4-network>) port)
  (format port "~a/~a" (prefix this) (prefix-len this)))
(define-method (write (this <ipv4-network>) port)
  (format port "#:net(~s/~s)" (prefix this) (prefix-len this)))

(define (ip-in-network? testip testnet)
  (eqv? (logand (ip testip) (prefixlen->intmask (prefix-len testnet)))
          (ip (prefix testnet))))

; Class for representing a route
(define-class <ipv4-route> ()
  (net #:accessor net #:init-val (new-network "0.0.0.0/0"))
  (gw #:accessor gw #:init-val (new-ip "0.0.0.0")))

; setters
(define-method (net (this <ipv4-route>) (val <ipv4-network>))
  (slot-set! this 'net val))
(define-method (net (this <ipv4-route>) val)
  (slot-set! this 'net (new-network val)))
(define-method (gw (this <ipv4-route>) (val <ipv4-address>))
  (slot-set! this 'gw val))
(define-method (gw (this <ipv4-route>) val)
  (slot-set! this 'gw (new-ip val)))

(define-generic new-route)
(define-method (new-route)
  (make <ipv4-route>))
(define-method (new-route netspec gateway)
  (let ((newroute (make <ipv4-route>)))
    (slot-set! newroute 'net (new-network netspec))
    (slot-set! newroute 'gw (new-ip gateway))
    newroute))
(define-method (new-route netspec mask gateway)
  (let ((newroute (make <ipv4-route>)))
    (slot-set! newroute 'net (new-network netspec mask))
    (slot-set! newroute 'gw (new-ip gateway))
    newroute))

(define-method (display (this <ipv4-route>) port)
  (format port "~a via ~a" (net this) (gw this)))
(define-method (write (this <ipv4-route>) port)
  (format port "#:route(~s via ~s)" (net this) (gw this)))

; Class for implementing a compressed radix tree
;
(define-generic value-binstr)
(define-class <trie-node> ()
  (value         #:accessor value         #:init-value  0)
  (value-binstr  #:allocation #:virtual   #:slot-ref value-binstr #:slot-set! value-binstr)
  (value-length  #:accessor value-length  #:init-value  0)
  (left          #:accessor left          #:init-value #f)
  (right         #:accessor right         #:init-value #f)
  (userdata      #:accessor userdata      #:init-value #f))
(define nullnode (make <trie-node>))
(define rootnode (make <trie-node>))
(set! (left rootnode) nullnode)
(set! (right rootnode) nullnode)

; setters
(define-method (value (this <trie-node>) (val <integer>))
  (slot-set! this 'value val))
(define-method (value-length (this <trie-node>) (len <integer>))
  (slot-set! this 'value-length len))
(define-method (left (this <trie-node>) (l <trie-node>))
  (slot-set! this 'left l))
(define-method (right (this <trie-node>) (r <trie-node>))
  (slot-set! this 'right r))
(define-method (userdata (this <trie-node>) ud)
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

; Constructor
(define-generic new-trie-node)
(define-method (new-trie-node (val <integer>) 
                              (len <integer>) 
                              (l <trie-node>) 
                              (r <trie-node>) 
                              udata)
  (let ((newnode (make <trie-node>)))
    (value newnode val)
    (value-length newnode len)
    (left newnode l)
    (right newnode r)
    (userdata newnode udata)
    newnode))

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
    (new-trie-node val len nullnode nullnode udata)
    ; Trying to add to an existing node
    (let-values (((common-pref common-len) (shared-prefix (value root)(value-length root)
                                            val len)))
      (if (eq? common-len len)
        ; We are an exact match for this prefix
        (if (equal? udata (userdata root))
           ; We are an exact match for this node, just return
           root
           ; Userdata is different
           (new-trie-node (value root)
                          (value-length root)
                          (left root)
                          (right root)
                          udata))
        (if (= common-len (value-length root))
          ; We are adding to a sub tree
          (if (not (logbit? (- (- len common-len) 1) val))
            ;add to left for a zero
            (let* ((nextval (logand val (- (integer-expt 2 (- len common-len )) 1)))
                   (nextlen (- len common-len))
                   (nextnode (add-trie-node (left root) nextval nextlen udata)))
              (if (eq? nextnode (left root))
                  root
                  (new-trie-node (value root)
                                 (value-length root)
                                 nextnode
                                 (right root)
                                 (userdata root))))
            ;add to right for a one 
            (let* ((nextval (logand val (- (integer-expt 2 (- len common-len )) 1)))
                   (nextlen (- len common-len))
                   (nextnode (add-trie-node (right root) nextval nextlen udata)))
              (if (eq? nextnode (right root))
                  root
                  (new-trie-node (value root)
                                 (value-length root)
                                 (left root)
                                 nextnode
                                 (userdata root)))))
          ;(< common-len (value-length root))
          ; We need to split our current node)
          (let* ((common-remains (new-trie-node
                                    (logand (value root) 
                                            (- (integer-expt 2 
                                                             (- (value-length root) 
                                                                common-len)) 
                                               1))
                                    (- (value-length root) common-len)
                                    (left root)
                                    (right root)
                                    (userdata root)))
                 (common-newdata (new-trie-node
                                    (logand val
                                            (- (integer-expt 2 
                                                             (- len 
                                                                common-len)) 
                                               1))
                                    (- len common-len)
                                    nullnode
                                    nullnode
                                    udata))
                 (nextleft       (if (not (logbit? (+ (value-length common-remains) 1)
                                                   (value common-remains)))
                                    common-remains
                                    common-newdata))
                 (nextright      (if (eq? nextleft common-remains)
                                    common-newdata
                                    common-remains)))
            (new-trie-node common-pref common-len nextleft nextright #f))
          )))))

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
(define new-ipv4-table (lambda () rootnode))
`
(define-generic add-ipv4-route)
(define-method (add-ipv4-route (table <trie-node>)(route <ipv4-route>))
  (let* ((addr    (net route))
         (pref    (ip (prefix addr)))
         (preflen (prefix-len addr)))
    (add-trie-node table (ash pref (- preflen 32)) preflen (ip(gw route)))))

(define-generic remove-ipv4-route)
(define-method (remove-ipv4-route (table <trie-node>)(route <ipv4-route>))
  (remove-trie-node table (net route) (ip (gw route))))

(define-generic find-ipv4-route)
(define-method (find-ipv4-route (table <trie-node>)(route <ipv4-address>))
  (let-values (((foundpref foundlen foundnode) 
                                  (longest-prefix-match
                                     table
                                     (ip route)
                                     32)))
    (let ((foundnet (new-network))
          (foundroute (new-route)))
      (prefix foundnet (ash foundpref (- 32 foundlen)))
      (prefix-len foundnet foundlen)
      (net foundroute foundnet)
      (gw foundroute (userdata foundnode))
      foundroute)))







