lib_LTLIBRARIES=libguile_snmp_net-snmp.la 

SCHEMESOURCES=snmp/net-snmp.scm\
              snmp/reports.scm\
              snmp/oids.scm

SCHEMESOURCES2=snmp/reports/cache.scm\
               snmp/reports/session.scm

CLEANFILES = $(GOBJECTS) $(GOBJECTS2)

SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/dev-environ $(GUILE_COMPILE) -o "$@" "$<"

EXTRA_DIST = snmp $(SCHEMESOURCES) $(SCHEMESOURCES2) 

guilesnmpdir =  @GUILE_SITE@/snmp
guilesnmp_DATA = $(SCHEMESOURCES) $(GOBJECTS)

guilereportsdir =  @GUILE_SITE@/snmp/reports
guilereports_DATA = $(SCHEMESOURCES2) $(GOBJECTS2)

bin_PROGRAMS = snmp-shell

snmp_shell_SOURCES=snmp-shell.c
snmp_shell_CFLAGS = $(GUILE_CFLAGS) $(AM_CFLAGS)
snmp_shell_LDFLAGS = $(GUILE_LDFLAGS)


