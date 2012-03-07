(define-module (test-reporst)
 #:use-module (oop goops)
 #:duplicates (merge-generics)
 #:use-module (srfi srfi-1) 
 #:use-module (snmp net-snmp) 
 #:use-module (snmp reports) 
 #:use-module (snmp reports session) 
 #:use-module (unit-test)
 #:use-module (ice-9 regex)) 

(putenv "MIBDIRS=+.")
(putenv "MIBS=+GUILE-SNMP-TEST-MIB")
(init-reports)
(disable-query-cache)

(define-class <test-reports> (<test-case>))
  
(define-method (test-autoresolve (self <test-reports>))
  (assert-equal gstTestString
               (list->oid (list 1 3 6 1 3 1977 1 2))))

(define-method (test-get (self <test-reports>))
  (assert-equal "Guile-SNMP test string"
                (session #:host "127.0.0.1:10161" 
                  ((get (snmp-parse-oid "gstTestString.0"))))))

(define-method (test-v3-get (self <test-reports>))
  (assert-equal "Guile-SNMP test string"
                (session #:host "127.0.0.1:10161" 
                         #:version SNMP-VERSION-3
                         #:secname "gsttest"
                         #:seclevel SNMP-SEC-LEVEL-AUTHPRIV
                         #:authproto AuthMD5
                         #:privproto PrivDES
                         #:authkey "Top_Secret_v3_user_password"
                         #:privkey "Top_Secret_v3_user_password"
                  ((get (snmp-parse-oid "gstTestString.0"))))))

(define-method (test-v3-get-error-failauth (self <test-reports>))
  (assert-equal #t
                (session #:host "127.0.0.1:10161" 
                         #:version SNMP-VERSION-3
                         #:secname "gsttest2"
                         #:seclevel SNMP-SEC-LEVEL-AUTHPRIV
                         #:authproto AuthMD5
                         #:privproto PrivDES
                         #:authkey "XXXTop_Secret_v3_user_password"
                         #:privkey "Top_Secret_v3_user_password"
                  (let ((errstring "No error"))
                    (catch 'snmperror
                      (lambda()((get (snmp-parse-oid "gstTestString.0"))))
                      (lambda(x . args)(set! errstring (car args))))
                    (if  (eq? #f (string-match "Authentication failure.*" errstring))
                      errstring
                      #t)))))

(define-method (test-v3-get-error-failpriv (self <test-reports>))
  (assert-equal #t
                (session #:host "127.0.0.1:10161" 
                         #:version SNMP-VERSION-3
                         #:secname "gsttest3"
                         #:seclevel SNMP-SEC-LEVEL-AUTHPRIV
                         #:authproto AuthMD5
                         #:privproto PrivDES
                         #:authkey "Top_Secret_v3_user_password"
                         #:privkey "XXXTop_Secret_v3_user_password"
                  (let ((errstring "No error"))
                    (catch 'snmperror
                      (lambda()((get (snmp-parse-oid "gstTestString.0"))))
                      (lambda(x . args)(set! errstring (car args))))
                    (if  (eq? #f (string-match "Timeout" errstring))
                      errstring
                      #t)))))

(define-method (test-get-error-noSuchName (self <test-reports>))
  (assert-equal 'noSuchObject
                (session #:host "127.0.0.1:10161" 
                  ((get (snmp-parse-oid "ifName.1"))))))

(define-method (test-get-error-noInstance (self <test-reports>))
  (assert-equal 'noSuchInstance
                (session #:host "127.0.0.1:10161" 
                  ((get (+ (snmp-parse-oid "gstTabAData") 100))))))

(define-method (test-get-error-Timeout (self <test-reports>))
  (assert-equal #t
                (session #:host "127.0.0.1:10163" 
                  (let ((errstring "No error"))
                    (catch 'snmperror
                      (lambda()((get (snmp-parse-oid "gstEndMib"))))
                      (lambda(x . args)(set! errstring  #t)))
                    errstring)))) 

(define-method (test-getnext-error-endOfMibView (self <test-reports>))
  (assert-equal 'endOfMibView
                (session #:host "127.0.0.1:10161" 
                  ((getnext (+ (snmp-parse-oid "gstEndMib") 1))))))

(define-method (test-get-multiple (self <test-reports>))
  (assert-equal (list "Guile-SNMP test string" 499)
                (session #:host "127.0.0.1:10161" 
                  (let ((vals (get (snmp-parse-oid "gstTestString.0") (snmp-parse-oid "gstTestInt32.0"))))
                    (list (vals (snmp-parse-oid "gstTestString.0")) 
                          (vals (snmp-parse-oid "gstTestInt32.0")))))))

(define-method (test-getnext (self <test-reports>))
  (assert-equal "Guile-SNMP test string"
                (session #:host "127.0.0.1:10161" 
                  ((getnext (snmp-parse-oid "gstTestString"))))))

(define-method (test-getnext-multiple (self <test-reports>))
  (assert-equal '("Guile-SNMP test string" 499)
                (session #:host "127.0.0.1:10161" 
                         (let ((res (getnext (snmp-parse-oid "gstTestString") (snmp-parse-oid "gstTestInt32"))))
                           (map (lambda(x)(value res x)) (oidlist res))))))

(define-method (test-walk-func (self <test-reports>))
  (assert-equal '(1 2)
                (session #:host "127.0.0.1:10161" 
                  (let* ((mywalk (walk-func (snmp-parse-oid "gstTabATable")))
                         (myval1 (value (mywalk)))
                         (myval2 (value (mywalk))))
                    (list myval1 myval2)))))

(define-method (test-walk (self <test-reports>))
  (assert-equal '("tableA row 1" "tableA row 2" "tableA row 3" "tableA row 4" "tableA row 5")
                (session #:host "127.0.0.1:10161" 
                  (map (lambda(x)(value x)) (walk (snmp-parse-oid "gstTabAData"))))))

(define-method (test-walk-on-fail (self <test-reports>))
  (assert-equal '("tableA row 1" "tableA row 2" "tableA row 3" "tableA row 4" "tableA row 5")
                (session #:host "127.0.0.1:10161" 
                  (let* ((result (list))
                         (val    (walk-on-fail (snmp-parse-oid "gstTabAData"))))
                     (catch 'noMoreAlternatives
                       (lambda()
                         (set! result (append result (list (val))))
                         (fail))
                       (lambda(ex . args) result))))))

(define-method (test-getbulk (self <test-reports>))
  (assert-equal '("LargeTable row 1" "LargeTable row 2" "LargeTable row 3" "LargeTable row 4" "LargeTable row 5")
                (session #:host "127.0.0.1:10161" 
                         (let ((res (getbulk () 5 ((snmp-parse-oid "gstLargeTableData")))))
                           (map (lambda(x)(value res x)) (oidlist res))))))

(define-method (test-bulkwalk-short (self <test-reports>))
  (assert-equal '("LargeTable row 1" "LargeTable row 2" "LargeTable row 3" "LargeTable row 4" "LargeTable row 5"
                  "LargeTable row 6" "LargeTable row 7" "LargeTable row 8" "LargeTable row 9" "LargeTable row 10"
                  "LargeTable row 11" "LargeTable row 12" "LargeTable row 13" "LargeTable row 14" "LargeTable row 15")
                (session #:host "127.0.0.1:10161" 
                  (map (lambda(x)(value x)) (bulk-walk (snmp-parse-oid "gstLargeTableData"))))))

(define-method (test-bulkwalk-long (self <test-reports>))
  (assert-equal '("LargeTable row 1" "LargeTable row 2" "LargeTable row 3" "LargeTable row 4" "LargeTable row 5"
                 "LargeTable row 6" "LargeTable row 7" "LargeTable row 8" "LargeTable row 9" "LargeTable row 10"
                  "LargeTable row 11" "LargeTable row 12" "LargeTable row 13" "LargeTable row 14" "LargeTable row 15")
                (session #:host "127.0.0.1:10161" 
                  (map (lambda(x)(value x)) (bulk-walk (snmp-parse-oid "gstLargeTableData"))))))

(define-method (test-set (self <test-reports>))
  (assert-equal #t
                (session #:host "127.0.0.1:10161" 
                  (set ((snmp-parse-oid "gstTestString.0") "Guile-SNMP test string")))))

(define-method (test-set2 (self <test-reports>))
  (assert-equal #t
                (session #:host "127.0.0.1:10161" 
                  (set ((snmp-parse-oid "gstTestString.0") (ASN-OCTET-STR "Guile-SNMP test string"))))))

(define-method (test-set3 (self <test-reports>))
  (assert-equal #t
                (session #:host "127.0.0.1:10161" 
                  (set ((snmp-parse-oid "gstTestString.0") "Guile-SNMP test string")
                       ((snmp-parse-oid "gstTestInt32.0") 499)))))

(exit-with-summary (run-all-defined-test-cases))

