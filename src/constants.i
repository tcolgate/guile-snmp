//-------------------------------------------------------------------
// Copyright (C) 2009,2010 Tristan Colgate 
//
// constants.i - forces the defintiion of some consts that swig  
// misses, usually due to "creativity" in the net-snmp headers.
//
//-------------------------------------------------------------------

/* Force the definitions of some consts that swig is missing */
%constant int SNMP_MSG_GET = SNMP_MSG_GET;
%constant int SNMP_MSG_GETNEXT = SNMP_MSG_GETNEXT;
%constant int SNMP_MSG_RESPONSE = SNMP_MSG_RESPONSE;
%constant int SNMP_MSG_SET = SNMP_MSG_SET;
%constant int SNMP_MSG_TRAP = SNMP_MSG_TRAP;
%constant int SNMP_MSG_GETBULK = SNMP_MSG_GETBULK;
%constant int SNMP_MSG_INFORM = SNMP_MSG_INFORM;
%constant int SNMP_MSG_TRAP2 = SNMP_MSG_TRAP2;
%constant int SNMP_MSG_REPORT = SNMP_MSG_REPORT;
%constant int SNMP_NOSUCHOBJECT = (ASN_CONTEXT | ASN_PRIMITIVE | 0x0); /* 80=128 */
%constant int SNMP_NOSUCHINSTANCE = (ASN_CONTEXT | ASN_PRIMITIVE | 0x1); /* 81=129 */
%constant int SNMP_ENDOFMIBVIEW = (ASN_CONTEXT | ASN_PRIMITIVE | 0x2); /* 82=130 */

%constant int STAT_SUCCESS = 0;
%constant int STAT_ERROR = 1;
%constant int STAT_TIMEOUT = 2;

/*
%{
%}
*/
%constant int ASN_BOOLEAN = 1;
%constant int ASN_INTEGER = 2;
%constant int ASN_BIT_STR = 3;
%constant int ASN_NULL = 0x05;
%constant int ASN_OBJECT_ID = 0x06;
%constant int ASN_SEQUENCE = 0x016;
%constant int ASN_SET = 0x017;
%constant int ASN_OCTET_STR = 0x04;

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


