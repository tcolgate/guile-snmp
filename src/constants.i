/* Force the definitions of some consts that swig is missing */
%constant oid SNMP_MSG_GET = (oid) SNMP_MSG_GET;
%constant oid SNMP_MSG_GETNEXT = (oid) SNMP_MSG_GETNEXT;
%constant oid SNMP_MSG_RESPONSE = (oid) SNMP_MSG_RESPONSE;
%constant oid SNMP_MSG_SET = (oid) SNMP_MSG_SET;
%constant oid SNMP_MSG_TRAP = (oid) SNMP_MSG_TRAP;
%constant oid SNMP_MSG_GETBULK = (oid) SNMP_MSG_GETBULK;
%constant oid SNMP_MSG_INFORM = (oid) SNMP_MSG_INFORM;
%constant oid SNMP_MSG_TRAP2 = (oid) SNMP_MSG_TRAP2;
%constant oid SNMP_MSG_REPORT = (oid) SNMP_MSG_REPORT;
%constant int SNMP_NOSUCHOBJECT = (ASN_CONTEXT | ASN_PRIMITIVE | 0x0); /* 80=128 */
%constant int SNMP_NOSUCHINSTANCE = (ASN_CONTEXT | ASN_PRIMITIVE | 0x1); /* 81=129 */
%constant int SNMP_ENDOFMIBVIEW = (ASN_CONTEXT | ASN_PRIMITIVE | 0x2); /* 82=130 */

%constant int STAT_SUCCESS = 0;
%constant int STAT_ERROR = 1;
%constant int STAT_TIMEOUT = 2;

%constant int ASN_IPADDRESS = (ASN_APPLICATION | 0);
%constant int ASN_COUNTER = (ASN_APPLICATION | 1);
%constant int ASN_GAUGE = (ASN_APPLICATION | 2);
%constant int ASN_UNSIGNED = (ASN_APPLICATION | 2);
%constant int ASN_TIMETICKS = (ASN_APPLICATION | 3);
%constant int ASN_OPAQUE = (ASN_APPLICATION | 4);
%constant int ASN_NSAP = (ASN_APPLICATION | 5); 
%constant int ASN_COUNTER64 = (ASN_APPLICATION | 6);
%constant int ASN_UINTEGER = (ASN_APPLICATION | 7); 
%constant int ASN_FLOAT = (ASN_APPLICATION | 8);
%constant int ASN_DOUBLE = (ASN_APPLICATION | 9);
%constant int ASN_INTEGER64 = (ASN_APPLICATION | 10);
%constant int ASN_UNSIGNED64 = (ASN_APPLICATION | 11);

