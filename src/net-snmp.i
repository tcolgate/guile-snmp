// SWIG templates for guile-netsnmp
//  -- based on original work by Yves Perrenoud <yves@xpand.org>

//%module net-snmp

// A few type definitions to clarify what to do

typedef unsigned long size_t;
typedef char u_char;
typedef unsigned long u_long;
typedef unsigned short u_short;

// Includes for both the C code and to generate the interface

%{
#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-config-i386.h>
#include <net-snmp/net-snmp-includes.h>
#include <net-snmp/library/transform_oids.h>
#include <net-snmp/library/snmp_impl.h>
#include <net-snmp/library/snmp_api.h>
#include <net-snmp/library/parse.h>
#include <net-snmp/library/mib.h>

%}

%include<ports.i>

#define NETSNMP_IMPORT extern

/* A typemap to pass read-only oids */

%typemap(in)(const oid* , size_t )(oid* temp_oid){
  scm_t_array_handle handle;
  size_t len,i;
  ssize_t inc;
  const scm_t_uint32* elt = scm_u32vector_elements($input, &handle, &len, &inc);
  temp_oid = (oid*)calloc(len,sizeof(oid));
  oid* oid_elt = temp_oid;
  for (i = 0; i < len; i++, elt += inc,oid_elt++)
     *oid_elt = (oid) *elt;

  scm_array_handle_release (&handle);

  $1 = temp_oid;
  $2 = len;
}

%typemap(freearg)(const oid* , size_t ){
  free (temp_oid$argnum);
}

%apply(const oid* , size_t ){
  (const oid* objid , size_t objidlen)
}

/* A pair of typemaps  to pass oids for write and return the 
  value as output */

%typemap(in,numinputs=0)(oid* , size_t*){
  // allocate a new oid( of maximum length)
  $2 = (size_t*)calloc(1,sizeof(size_t));
  *$2=MAX_OID_LEN;
  $1 = (oid*)calloc(*$2,sizeof(oid));
}

%typemap(argout)(oid* , size_t*){
  int i = 0;
  SCM newoid = SCM_UNSPECIFIED;

  if(result){
    newoid = scm_make_u32vector(scm_from_unsigned_integer(*$2),scm_from_unsigned_integer(0));
  
    for (i = 0; i < *$2; i++)
      scm_u32vector_set_x(newoid,scm_from_unsigned_integer(i),scm_from_unsigned_integer($1[i]));
  };

  gswig_result = newoid;
  free($1);
  free($2);
}

%apply(oid* , size_t* ){
  (oid* objid , size_t* objidlen)
}

/* A pair of typemaps to pass char* for write and return the 
  value as output */

%typemap(in,numinputs=0)(char* , size_t){
  // allocate a new string
  $2=128; // really bad, but use an arbitrarily long string
  $1 = (char*)calloc($2,sizeof(char));
}

%typemap(argout)(char* , size_t){
  SCM newstring = SCM_UNSPECIFIED;

  if(result){
    newstring = scm_from_locale_string($1);
  };

  gswig_result = newstring;
  free($1);
}

%apply(char* , size_t ){
  (char* buf, size_t buf_len)
}
/* A pair of typemaps  to pass pdus for write and return the 
  value as output */

%typemap(in,numinputs=0)(netsnmp_pdu **)(netsnmp_pdu *responsepdu){
  $1 = &responsepdu;
}

%typemap(argout)(netsnmp_pdu **){
  SCM scmout = SCM_UNSPECIFIED;

  if(!result){
    scmout = SWIG_NewPointerObj (*$1, SWIGTYPE_p_snmp_pdu, 0);
  };
  gswig_result = scmout;
}

%include "net-snmp-config.h"
%include "asn1.h"

%extend variable_list {
       const SCM value;
}

// Specific implementation of set/get functions
%{
SCM variable_list_value_get(struct variable_list *p) {
  SCM result = SCM_UNSPECIFIED;
  switch(p->type){
    case ASN_OCTET_STR: 
    case ASN_BIT_STR: 
      // These should probably be handled differently
      result = scm_from_locale_stringn((p->val).string,p->val_len);
      break;
    case ASN_INTEGER: 
    case ASN_GAUGE: 
      result = scm_int2num(*((p->val).integer));
      break;
    case ASN_COUNTER: 
    case ASN_TIMETICKS: 
      result = scm_uint2num(*((p->val).integer));
      break;
    case ASN_NULL: 
      //lambda pdu: None,
      result = SCM_EOL;
      break;
    case ASN_OBJECT_ID: 
      {
        // Guile wants to take ownership of the array
        // so we copy it first.
        oid* temp = (oid*)malloc(p->val_len);
        memcpy(temp,(p->val).objid,p->val_len);
        result = scm_take_u32vector(temp, (p->val_len)/sizeof(oid));
      }; 
      break;
    case ASN_IPADDRESS: 
      //Since snmp session takes a string we will rewturn
      // these as strings. would prefer a proper IP object
      {
        char* temp = (char*)malloc(16*(sizeof(char)));
        unsigned int a = ((unsigned char*)((p->val).bitstring))[0];
        unsigned int b = ((unsigned char*)((p->val).bitstring))[1];
        unsigned int c = ((unsigned char*)((p->val).bitstring))[2];
        unsigned int d = ((unsigned char*)((p->val).bitstring))[3];
        snprintf(temp,16,"%u.%u.%u.%u",a,b,c,d);
        result = scm_from_locale_string(temp);
        free(temp);
      }
      break;
    case ASN_COUNTER64: 
      {
        SCM high = scm_int2num(((p->val).counter64)->high);
        SCM low = scm_int2num(((p->val).counter64)->low);
        result = scm_sum(
                   scm_ash(high,scm_int2num(32)),
                   low);
      };
      break;
    case ASN_APP_FLOAT: 
      result = scm_float2num(*((p->val).floatVal));
      break;
    case ASN_APP_DOUBLE: 
      result = scm_double2num(*((p->val).doubleVal));
      break;
//    case ASN_BOOLEAN: 
//      // Do not think this is a valid pdu type  
//      break;
    default: 
      // use snprint_value to format the value as a string
      break;
  };
  return result;
};
%}

%inline %{
int
oid_from_varbind(netsnmp_variable_list* varbind, oid* objid, size_t* objidlen){
  memcpy( objid, varbind->name, (varbind->name_length * sizeof(oid)));
  *objidlen =  varbind->name_length;
  return 1 ;
};

%}

%include "constants.i"
%include "renames.i"
%include "ignores.i"


%include "snmp.h"
%include "snmp_api.h"
%include "snmp_client.h"
%include "parse.h"
%include "mib.h"
%include "default_store.h"

%goops %{ 
(load-extension "libguile_snmp_net-snmp.so" "scm_init_snmp_net_snmp_module")
%}


