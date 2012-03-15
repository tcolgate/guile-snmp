TESTS=test-oids.scm test-net-snmp.scm test-net-snmp-v3.scm test-reports.scm test-display-hint.scm

TESTS_ENVIRONMENT=\
  $(top_srcdir)/test-environ guile --no-auto-compile -L $(top_builddir) -L $(top_srcdir)/src -s

%.check: %
	$(TESTS_ENVIRONMENT) $*

EXTRA_DIST=$(TESTS) GUILE-SNMP-TEST-MIB guileSnmpTestMIB-emulation.conf

