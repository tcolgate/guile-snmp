TESTS=test-oids.scm

# This wont current set LD_LIBRARY_PATH
TESTS_ENVIRONMENT=\
	GUILE_AUTO_COMPILE=0	\
	$(top_srcdir)/dev-environ guile -L $(top_srcdir)/src -s

%.check: %
	$(TESTS_ENVIRONMENT) $(srcdir)/$*

EXTRA_DIST=$(TESTS)

# MIBDIRS=+. snmpd -f -r -Le  -C -c ./guileSnmpTestMIB-emulation.conf 
