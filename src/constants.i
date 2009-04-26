/*-------------------------------------------------------------------
 * Copyright (C) 2009 Tristan Colgate 
 *
 * constants.i - Forces swig to provide constants for some
 * variables that are otherwise missed by the parser.
 *
 -------------------------------------------------------------------*/

%constant oid SNMP_MSG_GET = (oid) SNMP_MSG_GET;
%constant oid SNMP_MSG_GETNEXT = (oid) SNMP_MSG_GETNEXT;
%constant oid SNMP_MSG_RESPONSE = (oid) SNMP_MSG_RESPONSE;
%constant oid SNMP_MSG_SET = (oid) SNMP_MSG_SET;
%constant oid SNMP_MSG_TRAP = (oid) SNMP_MSG_TRAP;
%constant oid SNMP_MSG_GETBULK = (oid) SNMP_MSG_GETBULK;
%constant oid SNMP_MSG_INFORM = (oid) SNMP_MSG_INFORM;
%constant oid SNMP_MSG_TRAP2 = (oid) SNMP_MSG_TRAP2;
%constant oid SNMP_MSG_REPORT = (oid) SNMP_MSG_REPORT;
