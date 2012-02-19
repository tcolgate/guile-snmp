#include "config.h"

SCM scm_goops_make;
SCM scm_class_oid;
SCM scm_kw_value;
SCM scm_oid_vec_slot;

/* This is needed by snmp_agent.h */
int      lastAddrAge;

#if SIZEOF_OID == 8
#define SCM_T_OID scm_t_uint64
#define SCM_TAKE_OIDVECTOR  scm_take_u64vector
#define SCM_OIDVECTOR_ELEMENTS scm_u64vector_elements
#define SCM_OIDVECTOR_P scm_u64vector_p
#define SCM_LIST_TO_OIDVECTOR scm_take_u64vector
#elif SIZEOF_OID == 4
#define SCM_T_OID scm_t_uint32
#define SCM_TAKE_OIDVECTOR  scm_take_u32vector
#define SCM_OIDVECTOR_ELEMENTS scm_u32vector_elements
#define SCM_OIDVECTOR_P scm_u32vector_p
#else
#error "Can't handle size of oid"
#endif

#ifdef __cplusplus
extern "C" {
#endif

void
init_oid_class()
{
  scm_goops_make = scm_variable_ref(
    scm_c_module_lookup(
      scm_module_goops,
      "make"));
  scm_kw_value = scm_from_locale_keyword("value");
  scm_class_oid = scm_variable_ref(
    scm_c_module_lookup(
      scm_c_resolve_module("snmp net-snmp"),
      "<oid>"));
  scm_oid_vec_slot = scm_from_locale_symbol("_vec");
  
  scm_variable_set_x(
    scm_c_module_lookup(
      scm_c_resolve_module("snmp net-snmp"),
      "empty-oidvec"),
  #if SIZEOF_OID == 8
    scm_make_u64vector(scm_from_int(0),SCM_EOL)
  #else // SIZEOF_OID == 4
    scm_make_u32vector(scm_from_int(0),SCM_EOL)
  #endif
    );
  
  SCM netsnmp_module = scm_c_resolve_module("snmp net-snmp");
  SCM srfi4_module = scm_c_resolve_module("srfi srfi-4");
  
#if SIZEOF_OID == 8
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u64vector")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "make-oidvector"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "make-u64vector")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector?"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u64vector?")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector-length"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u64vector-length")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "list->oidvector"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "list->u64vector")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector->list"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u64vector->list")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector-ref"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u64vector-ref")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector-set!"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u64vector-set!")));
#else // SIZEOF_OID == 4
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u32vector")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "make-oidvector"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "make-u32vector")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector?"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u32vector?")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector-length"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u32vector-length")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "list->oidvector"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "list->u32vector")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector->list"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u32vector->list")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector-ref"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u32vector-ref")));
  scm_variable_set_x( scm_c_module_lookup( netsnmp_module, "oidvector-set!"),
  scm_variable_ref( scm_c_module_lookup( srfi4_module, "u32vector-set!")));
#endif
};

void
scm_to_oid(SCM oidscm, oid** result, size_t *len)
{
  scm_t_array_handle handle;
  size_t i;
  ssize_t inc;
  const SCM_T_OID* elt = SCM_OIDVECTOR_ELEMENTS(scm_slot_ref(oidscm,scm_oid_vec_slot), &handle, len, &inc);
  *result = (oid*) scm_realloc(*result,*len * sizeof(oid));
  oid* oid_elt = *result;
  for (i = 0; i < *len; i++, elt += inc,oid_elt++)
  {
    *oid_elt = (oid) *elt;
  };

  scm_array_handle_release (&handle);
  scm_remember_upto_here_1(oidscm);
  return;
}

SCM scm_from_oid(oid *oidarray, int oidlen)
{
  SCM result = scm_apply(scm_goops_make,scm_list_3(scm_class_oid,scm_kw_value,
            SCM_TAKE_OIDVECTOR((SCM_T_OID*) oidarray, oidlen)
          ),SCM_EOL);

  return result;
}

