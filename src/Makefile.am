lib_LTLIBRARIES=libguile_snmp_net-snmp.la 

BUILT_SOURCES=$(builddir)/net-snmp_wrap.c $(builddir)/snmp/net-snmp.scm 
SWIGSOURCES=net-snmp.i constants.i ignores.i renames.i 

NETSNMPPATCHES=mib.h-patch snmp_client.h-patch snmp_api.h-patch

realcpu = __@host_cpu@__
fakecpu = `echo $(realcpu) | sed -e 's!i686!i386!'`
SWIG_FLAGS = -D$(fakecpu) -DNET_SNMP_CONFIG_H -guile -Linkage module -package snmp -proxy -emit-setters -I@NETSNMP_PREFIX@/include -outdir $(builddir)

mib.h snmp_client.h snmp_api.h: mib.h-patch snmp_client.h-patch snmp_api.h-patch 
	cp @NETSNMP_PREFIX@/include/net-snmp/library/$@ . && cat ${top_srcdir}/src/$@-patch |  patch -p0

$(builddir)/net-snmp_wrap.c: $(SWIGSOURCES) $(NETSNMPPATCHES) mib.h snmp_client.h snmp_api.h 
	test -d $(builddir)/snmp || mkdir $(builddir)/snmp
	$(SWIG) $(SWIG_FLAGS) -module net-snmp -o $@ $<

libguile_snmp_net_snmp_la_CFLAGS=@GUILE_CFLAGS@ -I.
nodist_libguile_snmp_net_snmp_la_SOURCES=$(builddir)/net-snmp_wrap.c $(builddir)/snmp/net-snmp.scm
libguile_snmp_net_snmp_la_LDFLAGS=@GUILE_LDFLAGS@ -module @NETSNMP_LDFLAGS@ @NETSNMPAGENT_LDFLAGS@
#libguile_snmp_net_snmp_la_LDFLAGS=@GUILE_LDFLAGS@ -module -lsnmp -lnetsnmp 

SCHEMESOURCES=$(builddir)/snmp/net-snmp.scm\
             $(builddir)/snmp/reports.scm\
             $(builddir)/snmp/oids.scm

SCHEMESOURCES2=snmp/reports/cache.scm\
               snmp/reports/session.scm

if HAVE_GUILE_COMPILE
GOBJECTS = $(SCHEMESOURCES:%.scm=%.go)
GOBJECTS2 = $(SCHEMESOURCES2:%.scm=%.go)
else
GOBJECTS =
GOBJECTS2 =
endif

CLEANFILES = $(GOBJECTS) $(GOBJECTS2) $(BUILT_SOURCES) *.orig mib.h snmp_client.h snmp_api.h

SUFFIXES = .scm .go
.scm.go:
	$(top_srcdir)/build-environ $(GUILE_COMPILE) -o "$@" "$<"

EXTRA_DIST = snmp net-snmp_wrap.c $(SWIGSOURCES) $(SCHEMESOURCES) $(SCHEMESOURCES2) $(NETSNMPPATCHES) mib.h snmp_client.h snmp_api.h
guilesite = @GUILE_SITE@
guileprefix = @GUILE_PREFIX@
guilesitesuffix = `echo $(guilesite) | sed -e 's!^$(guileprefix)!!'`
guilesnmpdir =  $(exec_prefix)/$(guilesitesuffix)/snmp
guilesnmp_DATA = $(SCHEMESOURCES) $(GOBJECTS)

guilereportsdir =  $(guilesnmpdir)/reports
guilereports_DATA = $(SCHEMESOURCES2) $(GOBJECTS2)

bin_PROGRAMS = snmp-shell

snmp_shell_SOURCES=snmp-shell.c
snmp_shell_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)
snmp_shell_LDFLAGS = $(GUILE_LDFLAGS)


