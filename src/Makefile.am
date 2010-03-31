lib_LTLIBRARIES=libguile_snmp_net-snmp.la 

SWIGSOURCES=net-snmp.i constants.i ignores.i renames.i 

NETSNMPPATCHES=mib.h-patch snmp_client.h-patch snmp_api.h-patch

SWIG_FLAGS = -D__i386__ -guile -Linkage module -package snmp -scm -proxy -emit-setters -I/usr/include/net-snmp/library -I/usr/include/net-snmp

mib.h snmp_client.h snmp_api.h: mib.h-patch snmp_client.h-patch snmp_api.h-patch
	cp /usr/include/net-snmp/library/$@ . && cat $@-patch |  patch -p0

net-snmp_wrap.c: $(SWIGSOURCES) $(NETSNMPPATCHES) mib.h snmp_client.h snmp_api.h
	$(SWIG) $(SWIG_FLAGS) -module net-snmp -o $@ $<

libguile_snmp_net_snmp_la_CFLAGS=@GUILE_CFLAGS@ -I.
libguile_snmp_net_snmp_la_SOURCES=net-snmp_wrap.c
libguile_snmp_net_snmp_la_LDFLAGS=@GUILE_LDFLAGS@ -module -lsnmp -lnetsnmp

SCHEMESOURCES=snmp/net-snmp.scm\
              snmp/reports.scm\
              snmp/oids.scm

SCHEMESOURCES2=snmp/reports/cache.scm\
               snmp/reports/session.scm

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

EXTRA_DIST = snmp $(SWIGSOURCES) net-snmp_wrap.c $(SCHEMESOURCES) $(SCHEMESOURCES2) $(NETSNMPPATCHES)

guilesnmpdir =  @GUILE_SITE@/snmp
guilesnmp_DATA = $(SCHEMESOURCES) $(GOBJECTS)

guilereportsdir =  @GUILE_SITE@/snmp/reports
guilereports_DATA = $(SCHEMESOURCES2) $(GOBJECTS2)

bin_PROGRAMS = snmp-shell

snmp_shell_SOURCES=snmp-shell.c
snmp_shell_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)
snmp_shell_LDFLAGS = $(GUILE_LDFLAGS)


