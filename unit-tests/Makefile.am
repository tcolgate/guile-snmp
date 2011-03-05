TESTS=test-oids.scm test-net-snmp.scm test-reports.scm

# This wont current set LD_LIBRARY_PATH
TESTS_ENVIRONMENT=\
  $(top_srcdir)/test-environ guile --no-auto-compile -L $(top_builddir) -L $(top_srcdir)/src -s

%.check: %
	$(TESTS_ENVIRONMENT) $*

EXTRA_DIST=$(TESTS) GUILE-SNMP-TEST-MIB

