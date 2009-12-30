lib_LTLIBRARIES=libguile_snmp_net-snmp.la 

GUILE_CFLAGS  = @GUILE_CFLAGS@
GUILE_LDFLAGS = @GUILE_LDFLAGS@

SWIG_FLAGS = -D__i386__ -guile -Linkage module -package snmp -scm -proxy -emit-setters -I/usr/include/net-snmp/library -I/usr/include/net-snmp

mib.h snmp_client.h snmp_api.h: 
	cp /usr/include/net-snmp/library/$@ . && cat $@-patch |  patch -p0

net-snmp_wrap.c: net-snmp.i constants.i ignores.i mib.h snmp_client.h snmp_api.h
	$(SWIG) $(SWIG_FLAGS) -module net-snmp -o $@ $<

libguile_snmp_net_snmp_la_CFLAGS=-I../lightning -I.
libguile_snmp_net_snmp_la_SOURCES=net-snmp_wrap.c
libguile_snmp_net_snmp_la_LDFLAGS=-module -lsnmp -lnetsnmp

EXTRA_DIST = snmp snmp/net-snmp.scm snmp/reports.scm snmp/oids.scm mib.h-patch snmp_client.h-patch snmp_api.h-patch 
guilesnmpdir =  /usr/local/share/guile/site/snmp
guilesnmp_DATA = snmp/net-snmp.scm snmp/reports.scm snmp/oids.scm

