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
#include <net-snmp/library/snmp_api.h>
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

%inline %{
struct myoid {
        oid *ids;
        size_t len;
};
struct myoid* nyoid_from_ptr(oid* data, ssize_t len){
struct myoid* newoid = (struct myoid*)calloc(1,sizeof(struct myoid));
  newoid->ids = data;
  newoid->len = len;
  return newoid;
};

%}

%include "constants.i"
%include "ignores.i"


%include "snmp.h"
%include "snmp_api.h"
%include "snmp_client.h"
%include "mib.h"
%include "default_store.h"

%goops %{ 
(load-extension "libguile_snmp_net-snmp.so" "scm_init_snmp_net_snmp_module")
%}


