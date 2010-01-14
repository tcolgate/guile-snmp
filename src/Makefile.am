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

SCHEMESOURCES2=snmp/reports/cache.scm

if HAVE_GUILE_COMPILE
GOBJECTS = $(SCHEMESOURCES:%.scm=%.go)
GOBJECTS2 = $(SCHEMESOURCES2:%.scm=%.go)
else
GOBJECTS =
GOBJECTS2 =
endif

CLEANFILES = $(GOBJECTS) $(GOBJECTS2)

SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/dev-environ $(GUILE_COMPILE) -o "$@" "$<"

EXTRA_DIST = snmp $(SCHEMESOURCES) $(SCHEMESOURCES2) mib.h-patch snmp_client.h-patch snmp_api.h-patch 

guilesnmpdir =  /usr/local/share/guile/site/snmp
guilesnmp_DATA = $(SCHEMESOURCES) $(GOBJECTS)

guilereportsdir =  /usr/local/share/guile/site/snmp/reports
guilereports_DATA = $(SCHEMESOURCES2) $(GOBJECTS2)

bin_SCRIPTS = snmp-report
