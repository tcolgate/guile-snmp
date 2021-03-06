```scheme
#!/usr/bin/snmp-shell -s
!#
;;-------------------------------------------------------------------
;; Copyright (C) 2009-2012 Tristan Colgate
;;
;; cisco-config-dump -
;;
;;  This script uses an snmp set to trigger a Cisco device into
;; beginning a tftp transfer of it's current config
;;
;;-------------------------------------------------------------------

;;; Commentary:
;;
;;  Useage: cisco-config-dump host1
;;
;;; Code:

(use-mibs CISCO-CONFIG-COPY-MIB)

(default-session #:host (car (script-arguments)) #:community "private")

(let* ((tftpservernat (str-to-ipstr "192.168.0.10)") ; The IP you will need to target for TFTP
      (jobid (getpid))
      (tftproot "/tftpboot")
      (filename (string-append (number->string jobid) "-confg"))
      (fqfilename (string-append tftproot "/" filename)))
 ; touch file
 (close-output-port (open-output-file fqfilename))
 (chmod fqfilename #o0666)

 (catch 'snmperror

  (lambda()
    (set ((+ ccCopySourceFileType jobid) 3)
         ((+ ccCopyDestFileType jobid) 1)
         ((+ ccCopyServerAddress jobid) tftpservernat)
         ((+ ccCopyFileName jobid) filename)
         ((+ ccCopyEntryRowStatus jobid) 4))

   (let wait ((ready ((get (+ copyState jobid)))))
     (if (not (< ready 3))
       (wait (begin
               (thread-sleep! (seconds->time (+ (time->seconds (current-time)) 0.25)))
               ((get (+ copyStat jobid)))))))

   ; dump output here
   (with-input-from-file fqfilename
     (lambda()
       (let loop ((input (read-line (current-input-port))))
         (if (not (eof-object? input))
           (begin (display input)(newline) (loop (read-line (current-input-port))))))))

   (set ((+ ccCopyEntryRowStatus jobid) 6)))

   (lambda (ex . args) (format (current-error-port) "Execption: ~a ~a~%" ex args)))

 (delete-file fqfilename))
```
