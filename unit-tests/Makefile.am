# Have to use serial tests as we run them through the agent so order
# matters, this may be avoidable
AUTOMAKE_OPTIONS=serial-tests

TESTS=test-oids.scm test-net-snmp.scm test-net-snmp-v3.scm test-reports.scm test-display-hint.scm test-asn1-parse.scm

TESTS_ENVIRONMENT=\
  $(top_srcdir)/test-environ guile --no-auto-compile -L $(top_builddir) -L $(top_srcdir)/src -s

%.check: %
	$(TESTS_ENVIRONMENT) $*

EXTRA_DIST=$(TESTS) GUILE-SNMP-TEST-MIB guileSnmpTestMIB-emulation.conf

