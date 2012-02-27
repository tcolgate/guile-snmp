/*
 * -------------------------------------------------------------------
 *  Copyright (C) 2009-2012 Tristan Colgate
 *
 *  net-snmp_wrap_constants.h
 * -------------------------------------------------------------------
 */

#define WRAP_CONSTANT(type, name) \
static type wrap_const_ ## name = name ;

WRAP_CONSTANT(oid , SNMP_MSG_GET)
WRAP_CONSTANT(oid , SNMP_MSG_GETNEXT)
WRAP_CONSTANT(oid , SNMP_MSG_RESPONSE)
WRAP_CONSTANT(oid , SNMP_MSG_SET)
WRAP_CONSTANT(oid , SNMP_MSG_TRAP)
WRAP_CONSTANT(oid , SNMP_MSG_GETBULK)
WRAP_CONSTANT(oid , SNMP_MSG_INFORM)
WRAP_CONSTANT(oid , SNMP_MSG_TRAP2)
WRAP_CONSTANT(oid , SNMP_MSG_REPORT)

WRAP_CONSTANT(int , STAT_SUCCESS)
WRAP_CONSTANT(int , STAT_ERROR)
WRAP_CONSTANT(int , STAT_TIMEOUT)

WRAP_CONSTANT(int , ASN_BOOLEAN)
WRAP_CONSTANT(int , ASN_INTEGER)
WRAP_CONSTANT(int , ASN_BIT_STR)
WRAP_CONSTANT(int , ASN_NULL)
WRAP_CONSTANT(int , ASN_OBJECT_ID)
WRAP_CONSTANT(int , ASN_SEQUENCE)
WRAP_CONSTANT(int , ASN_SET)
WRAP_CONSTANT(int , ASN_OCTET_STR)
WRAP_CONSTANT(int , ASN_IPADDRESS)
WRAP_CONSTANT(int , ASN_COUNTER)
WRAP_CONSTANT(int , ASN_GAUGE)
WRAP_CONSTANT(int , ASN_UNSIGNED)
WRAP_CONSTANT(int , ASN_TIMETICKS)
WRAP_CONSTANT(int , ASN_OPAQUE)
WRAP_CONSTANT(int , ASN_NSAP)
WRAP_CONSTANT(int , ASN_COUNTER64)
WRAP_CONSTANT(int , ASN_UINTEGER)
WRAP_CONSTANT(int , ASN_FLOAT)
WRAP_CONSTANT(int , ASN_DOUBLE)
WRAP_CONSTANT(int , ASN_INTEGER64)
WRAP_CONSTANT(int , ASN_UNSIGNED64)
WRAP_CONSTANT(int , SNMP_NOSUCHOBJECT)
WRAP_CONSTANT(int , SNMP_NOSUCHINSTANCE)
WRAP_CONSTANT(int , SNMP_ENDOFMIBVIEW)

WRAP_CONSTANT(int , SNMP_VERSION_1)
WRAP_CONSTANT(int , SNMP_VERSION_2c)
WRAP_CONSTANT(int , SNMP_VERSION_2u)
WRAP_CONSTANT(int , SNMP_VERSION_3)
WRAP_CONSTANT(int , SNMP_VERSION_sec)
WRAP_CONSTANT(int , SNMP_VERSION_2p)
WRAP_CONSTANT(int , SNMP_VERSION_2star)

WRAP_CONSTANT(int , SNMP_ERR_NOERROR)
WRAP_CONSTANT(int , SNMP_ERR_TOOBIG)
WRAP_CONSTANT(int , SNMP_ERR_NOSUCHNAME)
WRAP_CONSTANT(int , SNMP_ERR_BADVALUE)
WRAP_CONSTANT(int , SNMP_ERR_READONLY)
WRAP_CONSTANT(int , SNMP_ERR_GENERR)
WRAP_CONSTANT(int , SNMP_ERR_NOACCESS)
WRAP_CONSTANT(int , SNMP_ERR_WRONGTYPE)
WRAP_CONSTANT(int , SNMP_ERR_WRONGLENGTH)
WRAP_CONSTANT(int , SNMP_ERR_WRONGENCODING)
WRAP_CONSTANT(int , SNMP_ERR_WRONGVALUE)
WRAP_CONSTANT(int , SNMP_ERR_NOCREATION)
WRAP_CONSTANT(int , SNMP_ERR_INCONSISTENTVALUE)
WRAP_CONSTANT(int , SNMP_ERR_RESOURCEUNAVAILABLE)
WRAP_CONSTANT(int , SNMP_ERR_COMMITFAILED)
WRAP_CONSTANT(int , SNMP_ERR_UNDOFAILED)
WRAP_CONSTANT(int , SNMP_ERR_AUTHORIZATIONERROR)
WRAP_CONSTANT(int , SNMP_ERR_NOTWRITABLE)
WRAP_CONSTANT(int , SNMP_ERR_INCONSISTENTNAME)

WRAP_CONSTANT(unsigned int , TYPE_OTHER)
WRAP_CONSTANT(unsigned int , TYPE_OBJID)
WRAP_CONSTANT(unsigned int , TYPE_OCTETSTR)
WRAP_CONSTANT(unsigned int , TYPE_INTEGER)
WRAP_CONSTANT(unsigned int , TYPE_NETADDR)
WRAP_CONSTANT(unsigned int , TYPE_IPADDR)
WRAP_CONSTANT(unsigned int , TYPE_COUNTER)
WRAP_CONSTANT(unsigned int , TYPE_GAUGE)
WRAP_CONSTANT(unsigned int , TYPE_TIMETICKS)
WRAP_CONSTANT(unsigned int , TYPE_OPAQUE)
WRAP_CONSTANT(unsigned int , TYPE_NULL)
WRAP_CONSTANT(unsigned int , TYPE_COUNTER64)
WRAP_CONSTANT(unsigned int , TYPE_BITSTRING)
WRAP_CONSTANT(unsigned int , TYPE_NSAPADDRESS)
WRAP_CONSTANT(unsigned int , TYPE_UINTEGER)
WRAP_CONSTANT(unsigned int , TYPE_UNSIGNED32)
WRAP_CONSTANT(unsigned int , TYPE_INTEGER32)

WRAP_CONSTANT(unsigned int , TYPE_SIMPLE_LAST)

WRAP_CONSTANT(unsigned int , TYPE_TRAPTYPE)
WRAP_CONSTANT(unsigned int , TYPE_NOTIFTYPE)
WRAP_CONSTANT(unsigned int , TYPE_OBJGROUP)
WRAP_CONSTANT(unsigned int , TYPE_NOTIFGROUP)
WRAP_CONSTANT(unsigned int , TYPE_MODID)
WRAP_CONSTANT(unsigned int , TYPE_AGENTCAP)
WRAP_CONSTANT(unsigned int , TYPE_MODCOMP)
WRAP_CONSTANT(unsigned int , TYPE_OBJIDENTITY)

WRAP_CONSTANT(unsigned int , MIB_ACCESS_READONLY)
WRAP_CONSTANT(unsigned int , MIB_ACCESS_READWRITE)
WRAP_CONSTANT(unsigned int , MIB_ACCESS_WRITEONLY)
WRAP_CONSTANT(unsigned int , MIB_ACCESS_NOACCESS)
WRAP_CONSTANT(unsigned int , MIB_ACCESS_NOTIFY)
WRAP_CONSTANT(unsigned int , MIB_ACCESS_CREATE)

WRAP_CONSTANT(unsigned int , MIB_STATUS_MANDATORY)
WRAP_CONSTANT(unsigned int , MIB_STATUS_OPTIONAL)
WRAP_CONSTANT(unsigned int , MIB_STATUS_OBSOLETE)
WRAP_CONSTANT(unsigned int , MIB_STATUS_DEPRECATED)
WRAP_CONSTANT(unsigned int , MIB_STATUS_CURRENT)

// Provide hook for the main module to hook into
SCM constant_name_from_value_hook = SCM_BOOL_F;

SCM
scm_set_constant_name_from_value_hook_x(SCM func)
{
  constant_name_from_value_hook = func;
  return SCM_UNSPECIFIED;
};

SCM
scm_constant_name_from_int(const char* classname, int val)
{
  return scm_call_2(
           constant_name_from_value_hook,
           scm_from_utf8_symbol(classname),
           scm_from_signed_integer(val));
};

int
scm_int_from_constant(const char* classname, SCM obj)
{
  SCM reqclass = scm_variable_ref(scm_c_lookup(classname));
  SCM objclass = scm_call_1( scm_variable_ref(scm_c_lookup("class-of")), obj);

  if(reqclass != objclass){
    scm_throw(
      scm_string_to_symbol(
        scm_from_locale_string("type error")),
          scm_from_locale_string("Incorrect type"));
    return 0;
  };
  return scm_to_int(scm_slot_ref(obj,scm_from_utf8_symbol("value")));
};

void
init_snmp_wrap_constants(void)
{
  scm_c_define_gsubr("set-constant-name-from-value-hook!", 1, 0, 0, (void *) scm_set_constant_name_from_value_hook_x);
  scm_c_export("set-constant-name-from-value-hook!", NULL);

#define EXPORT_CONSTANT(cname, name, func) \
  scm_c_define("_wrap_" name , \
    func ( wrap_const_ ## cname ));\
  scm_c_export("_wrap_" name , NULL);\

  EXPORT_CONSTANT(SNMP_VERSION_1, "SNMP-VERSION-1" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_VERSION_2c, "SNMP-VERSION-2c" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_VERSION_2u, "SNMP-VERSION-2u" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_VERSION_3, "SNMP-VERSION-3" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_VERSION_sec, "SNMP-VERSION-2sec" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_VERSION_2p, "SNMP-VERSION-2p" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_VERSION_2star, "SNMP-VERSION-2star" , scm_from_signed_integer)

  EXPORT_CONSTANT(SNMP_MSG_GET, "SNMP-MSG-GET" , scm_from_ulong)
  EXPORT_CONSTANT(SNMP_MSG_GETNEXT, "SNMP-MSG-GETNEXT" , scm_from_ulong)
  EXPORT_CONSTANT(SNMP_MSG_RESPONSE, "SNMP-MSG-RESPONSE" , scm_from_ulong)
  EXPORT_CONSTANT(SNMP_MSG_SET, "SNMP-MSG-SET" , scm_from_ulong)
  EXPORT_CONSTANT(SNMP_MSG_TRAP, "SNMP-MSG-TRAP" , scm_from_ulong)
  EXPORT_CONSTANT(SNMP_MSG_GETBULK, "SNMP-MSG-GETBULK" , scm_from_ulong)
  EXPORT_CONSTANT(SNMP_MSG_INFORM, "SNMP-MSG-INFORM" , scm_from_ulong)
  EXPORT_CONSTANT(SNMP_MSG_TRAP2, "SNMP-MSG-TRAP2" , scm_from_ulong)
  EXPORT_CONSTANT(SNMP_MSG_REPORT, "SNMP-MSG-REPORT" , scm_from_ulong)

  EXPORT_CONSTANT(ASN_BOOLEAN , "ASN-BOOLEAN" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_INTEGER , "ASN-INTEGER" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_BIT_STR , "ASN-BIT-STR" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_NULL , "ASN-NULL" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_OBJECT_ID , "ASN-OBJECT-ID" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_SEQUENCE , "ASN-SEQUENCE" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_SET , "ASN-SET" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_OCTET_STR , "ASN-OCTET-STR" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_IPADDRESS , "ASN-IPADDRESS" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_COUNTER , "ASN-COUNTER" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_GAUGE , "ASN-GAUGE" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_UNSIGNED , "ASN-UNSIGNED" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_TIMETICKS , "ASN-TIMETICKS" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_OPAQUE , "ASN-OPAQUE" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_NSAP , "ASN-NSAP" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_COUNTER64 , "ASN-COUNTER64" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_UINTEGER , "ASN-UINTEGER" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_FLOAT , "ASN-FLOAT" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_DOUBLE , "ASN-DOUBLE" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_INTEGER64 , "ASN-INTEGER64" , scm_from_signed_integer)
  EXPORT_CONSTANT(ASN_UNSIGNED64 , "ASN-UNSIGNED64" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_NOSUCHOBJECT , "SNMP-NOSUCHOBJECT" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_NOSUCHINSTANCE , "SNMP-NOSUCHINSTANCE" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ENDOFMIBVIEW , "SNMP-ENDOFMIBVIEW" , scm_from_signed_integer)

  EXPORT_CONSTANT(STAT_SUCCESS , "STAT-SUCCESS" , scm_from_signed_integer)
  EXPORT_CONSTANT(STAT_ERROR , "STAT-ERROR" , scm_from_signed_integer)
  EXPORT_CONSTANT(STAT_TIMEOUT , "STAT-TIMEOUT" , scm_from_signed_integer)

  EXPORT_CONSTANT(SNMP_ERR_NOERROR , "SNMP-ERR-NOERROR" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_TOOBIG , "SNMP-ERR-TOOBIG" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_NOSUCHNAME , "SNMP-ERR-NOSUCHNAME" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_BADVALUE , "SNMP-ERR-BADVALUE" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_READONLY , "SNMP-ERR-READONLY" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_GENERR , "SNMP-ERR-GENERR" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_NOACCESS , "SNMP-ERR-NOACCESS" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_WRONGTYPE , "SNMP-ERR-WRONGTYPE" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_WRONGLENGTH , "SNMP-ERR-WRONGLENGTH" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_WRONGENCODING , "SNMP-ERR-WRONGENCODING" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_WRONGVALUE , "SNMP-ERR-WRONGVALUE" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_NOCREATION , "SNMP-ERR-NOCREATION" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_INCONSISTENTVALUE , "SNMP-ERR-INCONSISTENTVALUE" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_RESOURCEUNAVAILABLE , "SNMP-ERR-RESOURCEUNAVAILABLE" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_COMMITFAILED , "SNMP-ERR-COMMITFAILED" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_UNDOFAILED , "SNMP-ERR-UNDOFAILED" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_AUTHORIZATIONERROR , "SNMP-ERR-AUTHORIZATIONERROR" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_NOTWRITABLE , "SNMP-ERR-NOTWRITABLE" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ERR_INCONSISTENTNAME , "SNMP-ERR-INCONSISTENTNAME" , scm_from_signed_integer)

  EXPORT_CONSTANT(TYPE_OTHER , "MIB-TYPE-OTHER" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_OBJID , "MIB-TYPE-OBJID" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_OCTETSTR , "MIB-TYPE-OCTETSTR" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_INTEGER , "MIB-TYPE-INTEGER" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_NETADDR , "MIB-TYPE-NETADDR" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_IPADDR , "MIB-TYPE-IPADDR" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_COUNTER , "MIB-TYPE-COUNTER" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_GAUGE , "MIB-TYPE-GAUGE" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_TIMETICKS , "MIB-TYPE-TIMETICKS" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_OPAQUE , "MIB-TYPE-OPAQUE" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_NULL , "MIB-TYPE-NULL" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_COUNTER64 , "MIB-TYPE-COUNTER64" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_BITSTRING , "MIB-TYPE-BITSTRING" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_NSAPADDRESS , "MIB-TYPE-NSAPADDRESS" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_UINTEGER , "MIB-TYPE-UINTEGER" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_UNSIGNED32 , "MIB-TYPE-UNSIGNED32" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_INTEGER32 , "MIB-TYPE-INTEGER32" , scm_from_unsigned_integer)

  EXPORT_CONSTANT(TYPE_SIMPLE_LAST , "MIB-TYPE-SIMPLE-LAST" , scm_from_unsigned_integer)

  EXPORT_CONSTANT(TYPE_TRAPTYPE , "MIB-TYPE-TRAPTYPE" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_NOTIFTYPE , "MIB-TYPE-NOTIFTYPE" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_OBJGROUP , "MIB-TYPE-OBJGROUP" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_NOTIFGROUP , "MIB-TYPE-NOTIFGROUP" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_MODID , "MIB-TYPE-MODID" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_AGENTCAP , "MIB-TYPE-AGENTCAP" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_MODCOMP , "MIB-TYPE-MODCOMP" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(TYPE_OBJIDENTITY , "MIB-TYPE-OBJIDENTITY" , scm_from_unsigned_integer)

  EXPORT_CONSTANT(MIB_ACCESS_READONLY , "MIB-ACCESS-READONLY" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_ACCESS_READWRITE , "MIB-ACCESS-READWRITE" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_ACCESS_WRITEONLY , "MIB-ACCESS-WRITEHONLY" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_ACCESS_NOACCESS , "MIB-ACCESS-NOACCESS" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_ACCESS_NOTIFY , "MIB-ACCESS-NOTIFY" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_ACCESS_CREATE , "MIB-ACCESS-CREATE" , scm_from_unsigned_integer)

  EXPORT_CONSTANT(MIB_STATUS_MANDATORY , "MIB-STATUS-MANDATORY" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_STATUS_OPTIONAL , "MIB-STATUS-OPTIONAL" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_STATUS_OBSOLETE , "MIB-STATUS-OBSOLETE" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_STATUS_DEPRECATED , "MIB-STATUS-DEPRECATED" , scm_from_unsigned_integer)
  EXPORT_CONSTANT(MIB_STATUS_CURRENT , "MIB-STATUS-CURRENT" , scm_from_unsigned_integer)
}
