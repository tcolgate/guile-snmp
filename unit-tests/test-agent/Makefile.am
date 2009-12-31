check_PROGRAMS = guile-snmp-test-agent
guile_snmp_test_agent_SOURCES = guileSnmpTestMIB.c guileSnmpTestMIB.h gstTabATable.c gstTabATable.h gstTabBTable.c gstTabBTable.h


guileSnmpTestMIB.c guileSnmpTestMIB.h: GUILE-SNMP-TEST-MIB
	MIBDIRS=+. MIBS=+GUILE-SNMP-TEST-MIB mib2c -c mib2c.scalar.conf guileSnmpTestMIB

gstTabATable.c gstTabATable.h: GUILE-SNMP-TEST-MIB
	MIBDIRS=+. MIBS=+GUILE-SNMP-TEST-MIB mib2c -c mib2c.mfd.conf gstTabATable

gstTabBTable.c gstTabBTable.h: GUILE-SNMP-TEST-MIB
	MIBDIRS=+. MIBS=+GUILE-SNMP-TEST-MIB mib2c -c mib2c.mfd.conf gstTabBTable

# net-snmp-config --compile-subagent mysubagent nstAgentSubagentObject.c
