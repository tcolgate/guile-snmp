lib_LTLIBRARIES=libguile_snmp_net-snmp.la 

GUILE_CFLAGS  = @GUILE_CFLAGS@
GUILE_LDFLAGS = @GUILE_LDFLAGS@

SWIG_FLAGS = -guile -Linkage module -package snmp -scm -proxy -emit-setters -I/usr/include/net-snmp/library -I/usr/include/net-snmp

mib.h snmp_client.h snmp_api.h: 
	cp /usr/include/net-snmp/library/$@ . && cat $@-patch |  patch -p0

net-snmp_wrap.c: net-snmp.i constants.i renames.i ignores.i mib.h snmp_client.h snmp_api.h
	$(SWIG) $(SWIG_FLAGS) -module net-snmp -o $@ $<

libguile_snmp_net_snmp_la_CFLAGS=-I../lightning -I.
libguile_snmp_net_snmp_la_SOURCES=net-snmp_wrap.c
libguile_snmp_net_snmp_la_LDFLAGS=-module -lsnmp -lnetsnmp

dist_bin_SCRIPTS=snmp-report
EXTRA_DIST = snmp snmp/net-snmp.scm snmp/reports.scm mib.h-patch snmp_client.h-patch snmp_api.h-patch 
guilesnmpdir =  @GUILE_SITE@/snmp
guilesnmp_DATA = snmp/net-snmp.scm snmp/reports.scm 

