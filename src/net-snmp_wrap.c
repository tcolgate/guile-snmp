#include <string.h>
#include <libguile.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include <unistd.h>

#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-includes.h>
#include <net-snmp/library/snmp.h>
#include <net-snmp/agent/net-snmp-agent-includes.h>
// These clash with the net-snmp definitons
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
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
  *result = (oid*) realloc(*result,*len * sizeof(oid));
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

typedef size_t (*free_wrap_smob_f)(SCM);
typedef SCM (*mark_wrap_smob_f)(SCM);
typedef int (*print_wrap_smob_f)(SCM,SCM,scm_print_state*);
typedef SCM (*equalp_wrap_smob_f)(SCM,SCM);
typedef struct wrap_smob_typedef_s {
  char *name;
  free_wrap_smob_f free_func;
  mark_wrap_smob_f mark_func;
  print_wrap_smob_f print_func;
  equalp_wrap_smob_f equalp_func;
  SCM ptrclass;
} wrap_smob_typedef_t;

static scm_t_bits snmp_wrap_smob_tag;
typedef enum snmp_wrap_smob_subtypes {
  smob_netsnmp_session = 0,
  smob_tree,
  smob_valuesi,
  smob_last
} snmp_wrap_smob_subtypes_e;

wrap_smob_typedef_t wrap_smob_types[] = {
  {"<snmp-session>", NULL, NULL, NULL, NULL, NULL},
  {"<tree>", NULL, NULL, NULL, NULL, NULL},
  {"<values>", NULL, NULL, NULL, NULL, NULL}
};

size_t
free_snmp_wrap_smob (SCM smob)
{
  scm_assert_smob_type (snmp_wrap_smob_tag, smob);
  if(wrap_smob_types[SCM_SMOB_FLAGS(smob)].free_func){
    return wrap_smob_types[SCM_SMOB_FLAGS(smob)].free_func(smob);
  };
  return 0;
}

SCM
mark_snmp_wrap_smob (SCM smob)
{
  scm_assert_smob_type (snmp_wrap_smob_tag, smob);
  if(wrap_smob_types[SCM_SMOB_FLAGS(smob)].mark_func){
    return wrap_smob_types[SCM_SMOB_FLAGS(smob)].mark_func(smob);
  };

  return SCM_UNSPECIFIED;
}

static int
print_snmp_wrap_smob (SCM smob, SCM port, scm_print_state *pstate)
{
  scm_assert_smob_type (snmp_wrap_smob_tag, smob);
  if(wrap_smob_types[SCM_SMOB_FLAGS(smob)].print_func){
    return wrap_smob_types[SCM_SMOB_FLAGS(smob)].print_func(smob, port, pstate);
  };

  void* data = (void *) SCM_SMOB_DATA (smob);
  int flags = (int) SCM_SMOB_FLAGS (smob);

  char str[128];
  snprintf(str,128,"#<smob: %p %i>",data,flags);
  scm_puts (str, port);

  /* non-zero means success */
  return 1;
}

SCM
equalp_snmp_wrap_smob (SCM smob1, SCM smob2)
{
  scm_assert_smob_type (snmp_wrap_smob_tag, smob1);
  scm_assert_smob_type (snmp_wrap_smob_tag, smob2);

  if(SCM_SMOB_FLAGS(smob1) != SCM_SMOB_FLAGS(smob2)) return SCM_BOOL_F;

  if(wrap_smob_types[SCM_SMOB_FLAGS(smob1)].equalp_func){
    return wrap_smob_types[SCM_SMOB_FLAGS(smob1)].equalp_func(smob1, smob2);
  };

  return SCM_SMOB_DATA(smob1) == SCM_SMOB_DATA(smob2) ? SCM_BOOL_T : SCM_BOOL_F;
}

void
init_snmp_wrap_smob_type (void)
{
  snmp_wrap_smob_tag = scm_make_smob_type ("snmp_wrap_smob", sizeof (void*));
  scm_set_smob_free (snmp_wrap_smob_tag, free_snmp_wrap_smob);
  scm_set_smob_mark (snmp_wrap_smob_tag, mark_snmp_wrap_smob);
  scm_set_smob_print (snmp_wrap_smob_tag, print_snmp_wrap_smob);
  scm_set_smob_equalp (snmp_wrap_smob_tag, equalp_snmp_wrap_smob);
}

void
init_snmp_wrap_classes(void)
{
  init_snmp_wrap_smob_type();
  snmp_wrap_smob_subtypes_e last = smob_last;

  int c;
  for(c = 0; c < (int) last; c++){
    SCM classname = scm_from_utf8_symbol(wrap_smob_types[c].name);

    /*
     *  (define <classname-ptr>
     *    (make-class (list) (list (list 'ptr)) #:name '<classname-ptr>))))
     *  (export <classname-ptr>)
     */
    SCM makeclass = scm_variable_ref(
		      scm_c_module_lookup (
			scm_c_resolve_module("oop goops"), "make-class"));
    SCM namekw = scm_from_utf8_keyword("name");
    SCM supers = SCM_EOL;
    SCM slots = scm_list_1(
                  scm_list_1(
                    scm_from_utf8_symbol("ptr")));

    SCM ptrclass = scm_call_4 (
		     makeclass,
		     supers,
		     slots,
                     namekw, 
                     classname );

    scm_module_define(scm_current_module(), classname, ptrclass);
    wrap_smob_types[c].ptrclass = ptrclass;
    scm_module_export(scm_current_module(), scm_list_1(classname));
  };
  
  return ;
};

static SCM
make_wrapped_pointer (snmp_wrap_smob_subtypes_e type, void* wrapstruct)
{
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, wrapstruct);
  SCM_SET_SMOB_FLAGS (smob, type);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  SCM inst = scm_make(scm_list_1(wrap_smob_types[type].ptrclass));
  scm_slot_set_x(inst,ptrsym,smob);

  return inst;

}

inline void
assert_smob_subtype(snmp_wrap_smob_subtypes_e type, SCM smob)
{
  if(SCM_SMOB_FLAGS(smob) != type) {
    return;
  };
}

void*
pointer_from_wrapped_smob(snmp_wrap_smob_subtypes_e type, SCM obj)
{
  SCM ptrsym = scm_from_utf8_symbol("ptr");
  SCM smob = scm_slot_ref(obj, ptrsym);
  assert_smob_subtype(type, smob);
  return (void*) SCM_SMOB_DATA (smob);
};

static SCM
read_only_setter(SCM s_0, SCM s_1)
{
  return SCM_UNSPECIFIED;
};

static SCM

make_snmp_wrap_tree_smob_from_ptr(struct tree *ptr)
{
  return make_wrapped_pointer(smob_tree ,(void*) ptr);
};

static SCM
_wrap_tree_label_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  return scm_from_utf8_string(node->label);
}

static SCM
_wrap_get_tree_head (void)
{
   return make_snmp_wrap_tree_smob_from_ptr(get_tree_head());
}

static SCM
_wrap_get_tree (SCM oidscm, SCM treehead)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, treehead);
  size_t len = MAX_OID_LEN;
  oid* temp_oid = (oid*)malloc(len * sizeof(oid));
  scm_to_oid(oidscm,&temp_oid,&len);

  struct tree* result = get_tree(temp_oid,len,node);
  scm_remember_upto_here_1(treehead);

  SCM scmresult = make_snmp_wrap_tree_smob_from_ptr(result);

  free(temp_oid);
  scm_remember_upto_here_1(oidscm);

  return scmresult;
}

SCM netsnmp_variable_list_value_get(struct variable_list *p) {
  SCM result = SCM_UNSPECIFIED;
  switch(p->type){
    case ASN_OCTET_STR: 
    case ASN_BIT_STR: 
      // These should probably be handled differently
      result = scm_from_locale_stringn((p->val).string,p->val_len);
      break;
    case ASN_INTEGER: 
    case ASN_GAUGE: 
      result = scm_from_int(*((p->val).integer));
      break;
    case ASN_COUNTER: 
    case ASN_TIMETICKS: 
      result = scm_from_uint(*((p->val).integer));
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
        result = scm_apply(scm_goops_make,scm_list_3(scm_class_oid,scm_kw_value,
            SCM_TAKE_OIDVECTOR((SCM_T_OID *)temp, (p->val_len)/sizeof(oid))
          ),SCM_EOL);
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
        SCM high = scm_from_int(((p->val).counter64)->high);
        SCM low = scm_from_int(((p->val).counter64)->low);
        result = scm_sum(
                   scm_ash(high,scm_from_int(32)),
                   low);
      };
      break;
    case ASN_APP_FLOAT: 
      result = scm_from_double((double)*((p->val).floatVal));
      break;
    case ASN_APP_DOUBLE: 
      result = scm_from_double(*((p->val).doubleVal));
      break;
//    case ASN_BOOLEAN: 
//      // Do not think this is a valid pdu type  
//      break;
    case SNMP_NOSUCHOBJECT: 
      result = scm_string_to_symbol(scm_from_locale_string("noSuchObject"));
      break;
    case SNMP_NOSUCHINSTANCE: 
      result = scm_string_to_symbol(scm_from_locale_string("noSuchInstance"));
      break;
    case SNMP_ENDOFMIBVIEW: 
      result = scm_string_to_symbol(scm_from_locale_string("endOfMibView"));
      break;
    default: 
      // use snprint_value to format the value as a string
      break;
  };
  return result;
};


int
oid_from_varbind(struct variable_list* varbind, oid* objid, size_t* objidlen){
  memcpy( objid, varbind->name, (varbind->name_length * sizeof(oid)));
  *objidlen =  varbind->name_length;
  return 1 ;
};


int
oid_from_tree_node(struct tree *tree_node, oid* objid, size_t* objidlen) {
  struct tree *currnode = tree_node;
  int currid = 0;
  *objidlen = 0;

  while(currnode != NULL)
  {
    *objidlen += 1;
    if(currnode->parent == NULL) break;
    currnode = currnode->parent;
  };
  currnode = tree_node;

  while(currnode != NULL)
  {
    objid[*objidlen - currid - 1] = currnode->subid;
    currid++;
    if(currnode->parent == NULL) break;
    currnode = currnode->parent;
  };
  return 1;
};


int guile_snmp_async_response(int op, struct snmp_session *sp, int reqid,
                     struct snmp_pdu *pdu, void *magic){
  return 1;
};

static SCM
make_snmp_wrap_netsnmp_session_smob(void)
{
  return make_wrapped_pointer(smob_netsnmp_session
		           ,(void*) scm_gc_malloc (sizeof(netsnmp_session), "netsnmp_session"));
};

SCM
snmp_session_callback_get(struct snmp_session *p) {
  return (SCM) p->callback_magic;
};

void
snmp_session_callback_set(struct snmp_session *p, SCM cb) {
  p->callback = guile_snmp_async_response;
  p->callback_magic = cb;
  return ;
};

_wrap_oid_from_varbind (SCM s_0)
{
#define FUNC_NAME "oid-from-varbind"
  struct variable_list *arg1 = (struct variable_list *) 0 ;
  oid *arg2 = (oid *) 0 ;
  size_t *arg3 = (size_t *) 0 ;
  SCM scmresult;
  int result;
  
  {
    // allocate a new oid( of maximum length)
    arg3 = (size_t*)calloc(1,sizeof(size_t));
    *arg3=MAX_OID_LEN;
    arg2 = (oid*)calloc(*arg3,sizeof(oid));
  }
  result = (int)oid_from_varbind(arg1,arg2,arg3);
  {
    scmresult = scm_from_signed_integer(result);
  }
  {
    scmresult = SCM_UNSPECIFIED;
    
    if(result){
      int i = 0;
      SCM newoid = SCM_TAKE_OIDVECTOR((SCM_T_OID*) arg2, *arg3);
      scmresult = scm_apply(scm_goops_make,scm_list_3(scm_class_oid,scm_kw_value,newoid),SCM_EOL);
    } else {
      free(arg2);
    };
    
    free(arg3);
  }
  
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_oid_from_tree_node (SCM s_0)
{
  struct tree *arg1 = (struct tree *) pointer_from_wrapped_smob(smob_tree,s_0) ;
  oid *arg2 = (oid *) 0 ;
  size_t *arg3 = (size_t *) 0 ;
  SCM scmresult;
  int result;
  
  {
    // allocate a new oid( of maximum length)
    arg3 = (size_t*)calloc(1,sizeof(size_t));
    *arg3=MAX_OID_LEN;
    arg2 = (oid*)calloc(*arg3,sizeof(oid));
  }
  result = (int)oid_from_tree_node(arg1,arg2,arg3);
  {
    scmresult = scm_from_signed_integer(result);
  }
  {
    scmresult = SCM_UNSPECIFIED;
    
    if(result){
      int i = 0;
      SCM newoid = SCM_TAKE_OIDVECTOR((SCM_T_OID*) arg2, *arg3);
      scmresult = scm_apply(scm_goops_make,scm_list_3(scm_class_oid,scm_kw_value,newoid),SCM_EOL);
    } else {
      free(arg2);
    };
    
    free(arg3);
  }

  return scmresult;
}


static SCM
_wrap_guile_snmp_async_response (SCM s_0, SCM s_1, SCM s_2, SCM s_3, SCM s_4)
{
#define FUNC_NAME "guile-snmp-async-response"
  int arg1 ;
  struct snmp_session *arg2 = (struct snmp_session *) 0 ;
  int arg3 ;
  struct snmp_pdu *arg4 = (struct snmp_pdu *) 0 ;
  void *arg5 = (void *) 0 ;
  SCM scmresult;
  int result;
  
  {
    arg1 = (int) scm_to_int(s_0);
  }
  {
    arg3 = (int) scm_to_int(s_2);
  }
  {
    arg5 = (void *)SWIG_MustGetPtr(s_4, NULL, 5, 0);
  }
  result = (int)guile_snmp_async_response(arg1,arg2,arg3,arg4,arg5);
  {
    scmresult = scm_from_signed_integer(result);
  }
  
  
  
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_version_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-version-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  long arg2 ;
  SCM scmresult;
  
  {
    arg2 = (long) scm_to_long(s_1);
  }
  if (arg1) (arg1)->version = arg2;
  scmresult = SCM_UNSPECIFIED;
  
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_version_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-version-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM scmresult;
  long result;
  
  result = (long) ((arg1)->version);
  {
    scmresult = scm_from_long(result);
  }
  
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_retries_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-retries-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  int arg2 ;
  SCM scmresult;
  
  {
    arg2 = (int) scm_to_int(s_1);
  }
  if (arg1) (arg1)->retries = arg2;
  scmresult = SCM_UNSPECIFIED;
  
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_retries_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-retries-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM scmresult;
  int result;
  
  result = (int) ((arg1)->retries);
  {
    scmresult = scm_from_signed_integer(result);
  }
  
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_timeout_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-timeout-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  long arg2 ;
  SCM scmresult;
  
  {
    arg2 = (long) scm_to_long(s_1);
  }
  if (arg1) (arg1)->timeout = arg2;
  scmresult = SCM_UNSPECIFIED;
  
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_timeout_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-timeout-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM scmresult;
  long result;
  
  result = (long) ((arg1)->timeout);
  {
    scmresult = scm_from_long(result);
  }
  
  
  return scmresult;
#undef FUNC_NAME
}

static SCM
_wrap_snmp_session_peername_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-peername-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  char *arg2 = (char *) 0 ;
  int must_free2 = 0 ;
  SCM scmresult;
  
  {
    arg2 = (char *)SWIG_scm2str(s_1);
    must_free2 = 1;
  }
  {
    if (arg1->peername) free((char *)arg1->peername);
    if (arg2) {
      arg1->peername = (char *) malloc(strlen((const char *)arg2)+1);
      strcpy((char *)arg1->peername, (const char *)arg2);
    } else {
      arg1->peername = 0;
    }
  }
  scmresult = SCM_UNSPECIFIED;
  
  if (must_free2 && arg2) SWIG_free(arg2);
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_peername_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-peername-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM scmresult;
  char *result = 0 ;
  
  result = (char *) ((arg1)->peername);
  {
    scmresult = scm_from_locale_string((const char *)result);
  }
  
  
  return scmresult;
#undef FUNC_NAME
}

static SCM
_wrap_snmp_session_community_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-community-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  u_char *arg2 = (u_char *) 0 ;
  int must_free2 = 0 ;
  SCM scmresult;
  
  {
    arg2 = (u_char *)SWIG_scm2str(s_1);
    must_free2 = 1;
  }
  {
    if (arg1->community) free((char *)arg1->community);
    if (arg2) {
      arg1->community = (u_char *) malloc(strlen((const char *)arg2)+1);
      strcpy((char *)arg1->community, (const char *)arg2);
    } else {
      arg1->community = 0;
    }
  }
  scmresult = SCM_UNSPECIFIED;
  
  if (must_free2 && arg2) SWIG_free(arg2);
  
  return scmresult;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_community_get (SCM s_0)
{
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM scmresult;
  u_char *result = 0 ;
  
  result = (u_char *) ((arg1)->community);
  {
    scmresult = scm_from_locale_string((const char *)result);
  }
  
  
  return scmresult;
}


static SCM
_wrap_init_snmp (SCM name)
{
  init_snmp(scm_to_utf8_string (name));
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_init_mib (void)
{
  init_mib();
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_parse_oid (SCM oidname)
{
	
   size_t *oidlen = (size_t*)scm_gc_malloc_pointerless(sizeof(size_t), "oid stroage");
   *oidlen=MAX_OID_LEN;
   oid *oidstore = (oid*)scm_gc_malloc_pointerless(*oidlen * sizeof(oid), "oid storage");

   oid *result = snmp_parse_oid(scm_to_locale_string(oidname), oidstore, oidlen);
  
   SCM scmresult = SCM_UNSPECIFIED; 
   if(result){
     scmresult = scm_from_oid(oidstore,*oidlen);
   }; 
     
   return scmresult;
}

SCM constant_classes;

#define WRAP_CONSTANT_CLASS(cname) \
static SCM constant_class_ ## cname ;

#define WRAP_CONSTANT(type, name) \
static type wrap_const_ ## name = name ;\
SCM wrap_const_inst_ ## name = SCM_BOOL_F;

WRAP_CONSTANT_CLASS(snmp_msg);
WRAP_CONSTANT(oid , SNMP_MSG_GET)
WRAP_CONSTANT(oid , SNMP_MSG_GETNEXT)
WRAP_CONSTANT(oid , SNMP_MSG_RESPONSE)
WRAP_CONSTANT(oid , SNMP_MSG_SET)
WRAP_CONSTANT(oid , SNMP_MSG_TRAP)
WRAP_CONSTANT(oid , SNMP_MSG_GETBULK)
WRAP_CONSTANT(oid , SNMP_MSG_INFORM)
WRAP_CONSTANT(oid , SNMP_MSG_TRAP2)
WRAP_CONSTANT(oid , SNMP_MSG_REPORT)

WRAP_CONSTANT_CLASS(snmp_status);
WRAP_CONSTANT(int , SNMP_NOSUCHOBJECT)
WRAP_CONSTANT(int , SNMP_NOSUCHINSTANCE)
WRAP_CONSTANT(int , SNMP_ENDOFMIBVIEW)
WRAP_CONSTANT(int , STAT_SUCCESS)
WRAP_CONSTANT(int , STAT_ERROR)
WRAP_CONSTANT(int , STAT_TIMEOUT)

WRAP_CONSTANT_CLASS(asn_type);
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

WRAP_CONSTANT_CLASS(snmp_version);
WRAP_CONSTANT(int , SNMP_VERSION_1)
WRAP_CONSTANT(int , SNMP_VERSION_2c)
WRAP_CONSTANT(int , SNMP_VERSION_2u)
WRAP_CONSTANT(int , SNMP_VERSION_3)
WRAP_CONSTANT(int , SNMP_VERSION_sec)
WRAP_CONSTANT(int , SNMP_VERSION_2p)
WRAP_CONSTANT(int , SNMP_VERSION_2star)

WRAP_CONSTANT_CLASS(snmp_err_status);
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


void
init_snmp_wrap_constants(void)
{
  SCM current_module_name = scm_call_1(scm_variable_ref(scm_c_lookup("module-name")),
                                  scm_current_module());

  SCM makeclass = scm_variable_ref(
                    scm_c_module_lookup (
                      scm_c_resolve_module("oop goops"), "make-class"));

  constant_classes = scm_from_utf8_symbol("constant-classes");
  scm_define(constant_classes, 
    scm_make_hash_table(scm_from_signed_integer(20)));
  scm_module_export(scm_current_module(), scm_list_1(constant_classes));
  SCM constant_classes_ref = scm_public_ref(current_module_name,constant_classes);  

  SCM superclassname = scm_from_utf8_symbol("<snmp-constant>");
  SCM namekw = scm_from_utf8_keyword("name");
  SCM initkw = scm_from_utf8_keyword("init-keyword");
  SCM valuekw = scm_from_utf8_keyword("value");
  SCM valuesym = scm_from_utf8_symbol("value");
  SCM supers = SCM_EOL;
  SCM slots = scm_list_1(
                  scm_list_3(
                    scm_from_utf8_symbol("value"),initkw, valuekw));
  scm_define(superclassname,
    scm_call_4 ( makeclass, supers, slots, namekw, superclassname ));
  scm_module_export(scm_current_module(), scm_list_1(superclassname));

  SCM classname = SCM_EOL;
  SCM constname,value,makeargs = SCM_EOL;

  supers = scm_list_1(scm_public_ref(current_module_name,superclassname));
  slots = SCM_EOL; 


#define EXPORT_CONSTANT_CLASS(cname , name) \
  constant_class_ ## cname = scm_from_utf8_symbol("<" name ">");\
  scm_define(constant_class_ ## cname ,scm_call_4 ( makeclass, supers, slots, namekw, constant_class_ ## cname )); \
  scm_module_export(scm_current_module(), scm_list_1(constant_class_ ## cname)); \
  scm_hash_set_x( constant_classes_ref \
    , constant_class_ ## cname , scm_make_hash_table(scm_from_signed_integer(32)));\


#define EXPORT_CONSTANT(class, cname, name, func) \
  wrap_const_inst_ ## cname = scm_from_utf8_symbol( name );\
  value = func ( wrap_const_ ## cname );\
  makeargs = scm_list_1(scm_public_ref(current_module_name, constant_class_ ## class));\
  scm_define( wrap_const_inst_ ## cname, scm_make(makeargs));\
  scm_module_export( scm_current_module() , scm_list_1(wrap_const_inst_ ## cname));\
  scm_slot_set_x(scm_public_ref(current_module_name,wrap_const_inst_ ## cname),valuesym,value);\
  scm_hash_set_x(\
    scm_hash_ref(constant_classes_ref , constant_class_ ## class , SCM_BOOL_F)\
  , value \
  , wrap_const_inst_ ## cname);\

  EXPORT_CONSTANT_CLASS(snmp_version , "snmp-version");
  EXPORT_CONSTANT(snmp_version, SNMP_VERSION_1, "SNMP-VERSION-1" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_version, SNMP_VERSION_2c, "SNMP-VERSION-2c" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_version, SNMP_VERSION_2u, "SNMP-VERSION-2u" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_version, SNMP_VERSION_3, "SNMP-VERSION-3" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_version, SNMP_VERSION_sec, "SNMP-VERSION-2sec" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_version, SNMP_VERSION_2p, "SNMP-VERSION-2p" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_version, SNMP_VERSION_2star, "SNMP-VERSION-2star" , scm_from_signed_integer)

  EXPORT_CONSTANT_CLASS(snmp_msg , "snmp-msg");
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_GET, "SNMP-MSG-GET" , scm_from_ulong)
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_GETNEXT, "SNMP-MSG-GETNEXT" , scm_from_ulong)
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_RESPONSE, "SNMP-MSG-RESPONSE" , scm_from_ulong)
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_SET, "SNMP-MSG-SET" , scm_from_ulong)
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_TRAP, "SNMP-MSG-TRAP" , scm_from_ulong)
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_GETBULK, "SNMP-MSG-GETBULK" , scm_from_ulong)
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_INFORM, "SNMP-MSG-INFORM" , scm_from_ulong)
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_TRAP2, "SNMP-MSG-TRAP2" , scm_from_ulong)
  EXPORT_CONSTANT(snmp_msg, SNMP_MSG_REPORT, "SNMP-MSG-REPORT" , scm_from_ulong)

  EXPORT_CONSTANT_CLASS(asn_type , "asn-type");
  EXPORT_CONSTANT(asn_type, ASN_BOOLEAN , "ASN-BOOLEAN" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_INTEGER , "ASN-INTEGER" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_BIT_STR , "ASN-BIT-STR" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_NULL , "ASN-NULL" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_OBJECT_ID , "ASN-OBJECT-ID" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_SEQUENCE , "ASN-SEQUENCE" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_SET , "ASN-SET" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_OCTET_STR , "ASN-OCTET-STR" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_IPADDRESS , "ASN-IPADDRESS" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_COUNTER , "ASN-COUNTER" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_GAUGE , "ASN-GAUGE" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_UNSIGNED , "ASN-UNSIGNED" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_TIMETICKS , "ASN-TIMETICKS" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_OPAQUE , "ASN-OPAQUE" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_NSAP , "ASN-NSAP" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_COUNTER64 , "ASN-COUNTER64" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_UINTEGER , "ASN-UINTEGER" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_FLOAT , "ASN-FLOAT" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_DOUBLE , "ASN-DOUBLE" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_INTEGER64 , "ASN-INTEGER64" , scm_from_signed_integer)
  EXPORT_CONSTANT(asn_type, ASN_UNSIGNED , "ASN-UNSIGNED64" , scm_from_signed_integer)

  EXPORT_CONSTANT_CLASS(snmp_status , "snmp-status");
  EXPORT_CONSTANT(snmp_status, SNMP_NOSUCHOBJECT , "SNMP-NOSUCHOBJECT" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_status, SNMP_NOSUCHINSTANCE , "SNMP-NOSUCHINSTANCE" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_status, SNMP_ENDOFMIBVIEW , "SNMP-ENDOFMIBVIEW" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_status, STAT_SUCCESS , "STAT-SUCCESS" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_status, STAT_ERROR , "STAT-ERROR" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_status, STAT_TIMEOUT , "STAT-TIMEOUT" , scm_from_signed_integer)

  EXPORT_CONSTANT_CLASS(snmp_err_status , "snmp-err-status");
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_NOERROR , "SNMP-ERR-NOERROR" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_TOOBIG , "SNMP-ERR-TOOBIG" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_NOSUCHNAME , "SNMP-ERR-NOSUCHNAME" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_BADVALUE , "SNMP-ERR-BADVALUE" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_READONLY , "SNMP-ERR-READONLY" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_GENERR , "SNMP-ERR-GENERR" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_NOACCESS , "SNMP-ERR-NOACCESS" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_WRONGTYPE , "SNMP-ERR-WRONGTYPE" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_WRONGLENGTH , "SNMP-ERR-WRONGLENGTH" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_WRONGENCODING , "SNMP-ERR-WRONGENCODING" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_WRONGVALUE , "SNMP-ERR-WRONGVALUE" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_NOCREATION , "SNMP-ERR-NOCREATION" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_INCONSISTENTVALUE , "SNMP-ERR-INCONSISTENTVALUE" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_RESOURCEUNAVAILABLE , "SNMP-ERR-RESOURCEUNAVAILABLE" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_COMMITFAILED , "SNMP-ERR-COMMITFAILED" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_UNDOFAILED , "SNMP-ERR-UNDOFAILED" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_AUTHORIZATIONERROR , "SNMP-ERR-AUTHORIZATIONERROR" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_NOTWRITABLE , "SNMP-ERR-NOTWRITABLE" , scm_from_signed_integer)
  EXPORT_CONSTANT(snmp_err_status, SNMP_ERR_INCONSISTENTNAME , "SNMP-ERR-INCONSISTENTNAME" , scm_from_signed_integer)
/*
 */
}

static void init_snmp_wrap(void *data)
{

  init_oid_class();
  init_snmp_wrap_classes();
  init_snmp_wrap_constants();

  scm_c_define_gsubr("oid-from-varbind", 1, 0, 0, (void *) _wrap_oid_from_varbind);
  scm_c_define_gsubr("guile-snmp-async-response", 5, 0, 0, (void *) _wrap_guile_snmp_async_response);

  scm_c_define_gsubr("init-mib", 0, 0, 0, (void *) _wrap_init_mib);
  scm_c_export("init-mib" , NULL);

  scm_c_define_gsubr("init-snmp", 1, 0, 0, (void *) _wrap_init_snmp);
  scm_c_export("init-snmp" , NULL);

  scm_c_define_gsubr("snmp-parse-oid", 1, 0, 0, (void *) _wrap_snmp_parse_oid);
  scm_c_export("snmp-parse-oid" , NULL);

  scm_c_define_gsubr("get-tree-head", 0, 0, 0, (void *) _wrap_get_tree_head);
  scm_c_export("get-tree-head" , NULL);

  scm_c_define_gsubr("get-tree", 2, 0, 0, (void *) _wrap_get_tree);
  scm_c_export("get-tree" , NULL);

  scm_c_define_gsubr("oid-from-tree-node", 1, 0, 0, (void *) _wrap_oid_from_tree_node);
  scm_c_export("oid-from-tree-node" , NULL);

  scm_c_define_gsubr("tree-label-get", 1, 0, 0, (void *) _wrap_tree_label_get);
  scm_c_define_gsubr("tree-label-set", 2, 0, 0, (void *) read_only_setter);
  scm_c_export("tree-label-get" , NULL);
  scm_c_export("tree-label-set" , NULL);

  scm_c_define("snmp-session-version", scm_make_procedure_with_setter(
    scm_c_define_gsubr("snmp-session-version-get", 1, 0, 0, (void *) _wrap_snmp_session_version_get),
    scm_c_define_gsubr("snmp-session-version-set", 2, 0, 0, (void *) _wrap_snmp_session_version_set)));

  scm_c_define("snmp-session-peername", scm_make_procedure_with_setter(
    scm_c_define_gsubr("snmp-session-peername-get", 1, 0, 0, (void *) _wrap_snmp_session_peername_get),
    scm_c_define_gsubr("snmp-session-peername-set", 2, 0, 0, (void *) _wrap_snmp_session_peername_set)));

  scm_c_define("snmp-session-community", scm_make_procedure_with_setter(
    scm_c_define_gsubr("snmp-session-community-get", 1, 0, 0, (void *) _wrap_snmp_session_community_get),
    scm_c_define_gsubr("snmp-session-community-set", 2, 0, 0, (void *) _wrap_snmp_session_community_set)));

  scm_c_define_gsubr ("make-snmp-wrap-netsnmp-session-smob", 0, 0, 0, make_snmp_wrap_netsnmp_session_smob);
  scm_c_export("make-snmp-wrap-netsnmp-session-smob" , NULL);
}

SCM
scm_init_snmp_net_snmp_module (void)
{
  scm_c_define_module("snmp net-snmp-primitive", init_snmp_wrap, NULL);
  return SCM_UNSPECIFIED;
}

#ifdef __cplusplus
}
#endif

