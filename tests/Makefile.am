TESTS=test.scm test2.scm test-report.scm find-host find-host-on-port

TESTS_ENVIRONMENT=guile -L $(top_srcdir)/src -s

%.check: %
	$(TESTS_ENVIRONMENT) $(srcdir)/$*

EXTRA_DIST=$(TESTS)

