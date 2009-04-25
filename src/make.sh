#!/bin/sh
swig -guile -Linkage module -package snmp -module net-snmp -scm -proxy -emitsetters -I/usr/include/net-snmp/library  -I/usr/include/net-snmp net-snmp.i 
gcc -shared -lnetsnmp -lsnmp  -o net-snmp.so net-snmp_wrap.c

