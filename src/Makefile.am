lib_LTLIBRARIES=libguile_snmp_net-snmp.la 

SWIG_FLAGS = -D__i386__ -guile -Linkage module -package snmp -scm -proxy -emit-setters -I/usr/include/net-snmp/library -I/usr/include/net-snmp

mib.h snmp_client.h snmp_api.h: 
	cp /usr/include/net-snmp/library/$@ . && cat $@-patch |  patch -p0

net-snmp_wrap.c: net-snmp.i constants.i ignores.i renames.i mib.h snmp_client.h snmp_api.h
	$(SWIG) $(SWIG_FLAGS) -module net-snmp -o $@ $<

libguile_snmp_net_snmp_la_CFLAGS=-I../lightning -I.
libguile_snmp_net_snmp_la_SOURCES=net-snmp_wrap.c
libguile_snmp_net_snmp_la_LDFLAGS=-module -lsnmp -lnetsnmp

SCHEMESOURCES=snmp/net-snmp.scm\
              snmp/reports.scm\
              snmp/oids.scm

if HAVE_GUILE_COMPILE
GOBJECTS = $(SCHEMESOURCES:%.scm=%.go)
else
GOBJECTS =
endif

CLEANFILES = $(GOBJECTS)

SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/dev-environ $(GUILE_COMPILE) -o "$@" "$<"

EXTRA_DIST = snmp $(SCHEMESOURCES) mib.h-patch snmp_client.h-patch snmp_api.h-patch 

guilesnmpdir =  /usr/local/share/guile/site/snmp
guilesnmp_DATA = $(SCHEMESOURCES) $(GOBJECTS)

bin_SCRIPTS = snmp-report
