#!/usr/bin/env snmp-shell -s
!#

;;-------------------------------------------------------------------
;; Copyright (C) 2009,2010 Tristan Colgate 
;;
;; test-script - a trivial script using the script interpreter
;;
;;-------------------------------------------------------------------

(display "hello")(newline)

(display (current-community))(newline)

(display (script-arguments))(newline)

(display (value (get sysLocation.0)))(newline)
