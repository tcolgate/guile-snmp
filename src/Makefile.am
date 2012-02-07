lib_LTLIBRARIES=libguile_snmp_net-snmp.la 

libguile_snmp_net_snmp_la_CFLAGS=@GUILE_CFLAGS@ 
libguile_snmp_net_snmp_la_SOURCES=net-snmp_wrap.c net-snmp_wrap_oid.h net-snmp_wrap_constants.h net-snmp_wrap_structs.h net-snmp_wrap_funcs.h
libguile_snmp_net_snmp_la_LDFLAGS=@GUILE_LDFLAGS@ -module @NETSNMP_LDFLAGS@ @NETSNMPAGENT_LDFLAGS@

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
	$(top_srcdir)/build-environ $(GUILE_COMPILE) -o "$@" "$<"

EXTRA_DIST = snmp $(SCHEMESOURCES) $(SCHEMESOURCES2) 
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


