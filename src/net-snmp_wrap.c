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
#include <net-snmp/agent/net-snmp-agent-includes.h>
// These clash with the net-snmp definitons
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#include <config.h>


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

typedef void (*init_snmp_wrap_smob_class_f)(void);
typedef SCM (*make_snmp_wrap_smob_f)(SCM);
typedef SCM (*clear_snmp_wrap_smob_f)(SCM);
typedef int (*print_snmp_wrap_smob_f)(SCM,SCM,scm_print_state);
typedef struct snmp_wrap_smob_typedef_s {
  char *name;
  init_snmp_wrap_smob_class_f init_func;
  make_snmp_wrap_smob_f make_func;
  clear_snmp_wrap_smob_f clear_func;
  print_snmp_wrap_smob_f print_func;
} snmp_wrap_smob_typedef_t;

static scm_t_bits snmp_wrap_smob_tag;
typedef enum snmp_wrap_smob_subtypes {
  smob_netsnmp_session = 0,
  smob_values
} snmp_wrap_smob_subtypes_e;

snmp_wrap_smob_typedef_t snmp_wrap_smob_types[] = {
  {"snmp-session", NULL, NULL, NULL, NULL},
  {"values", NULL, NULL, NULL, NULL},
  {NULL, NULL, NULL, NULL, NULL}
};

void
init_snmp_wrap_classes(void)
{
  return ;
};

static SCM
make_snmp_wrap_smob (snmp_wrap_smob_subtypes_e type, void* wrapstruct)
{
  SCM smob;
  /* Step 1: Allocate the memory block.
   */
  // image = (struct image *) scm_gc_malloc (sizeof (struct image), "image");

  /* Step 2: Initialize it with straight code.
   */
  // image->width = width;
  // image->height = height;
  // image->pixels = NULL;
  // image->name = SCM_BOOL_F;
  // image->update_func = SCM_BOOL_F;

  /* Step 3: Create the smob.
   */
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, wrapstruct);
  SCM_SET_SMOB_FLAGS (smob, type);
  /* Step 4: Finish the initialization.
   */
  // image->name = name;
  // image->pixels = scm_gc_malloc_pointerless (width * height, "image pixels");

  return smob;
}

static SCM
make_snmp_wrap_netsnmp_session_smob(void)
{
  return make_snmp_wrap_smob(smob_netsnmp_session
		           ,(void*) scm_gc_malloc (sizeof(netsnmp_session), "netsnmp_session"));
};

SCM
clear_snmp_wrap_smob (SCM image_smob)
{
//  int area;
//  struct image *image;

//  scm_assert_smob_type (image_tag, image_smob);

 // image = (struct image *) SCM_SMOB_DATA (image_smob);
 // area = image->width * image->height;
 // memset (image->pixels, 0, area);

  /* Invoke the image's update function.
   */
//  if (scm_is_true (image->update_func))
//    scm_call_0 (image->update_func);

 // scm_remember_upto_here_1 (image_smob);

  return SCM_UNSPECIFIED;
}

static int
print_snmp_wrap_smob (SCM snmp_wrap_smob, SCM port, scm_print_state *pstate)
{
//  struct image *image = (struct image *) SCM_SMOB_DATA (image_smob);

//  scm_puts ("#<image ", port);
//  scm_display (image->name, port);
//  scm_puts (">", port);

  /* non-zero means success */
  return 1;
}

void
init_snmp_wrap_smob_type (void)
{
  snmp_wrap_smob_tag = scm_make_smob_type ("snmp_wrap_smob", sizeof (void*));
  // scm_set_smob_print (snmp_wrap_smob_tag, snmp_wrap_smob);

  scm_c_define_gsubr ("make-snmp-wrap-netsnmp-session-smob", 0, 0, 0, make_snmp_wrap_netsnmp_session_smob);
  scm_c_export("make-snmp-wrap-netsnmp-session-smob" , NULL);
//  scm_c_define_gsubr ("clear-snmp-wrap-smob", 1, 0, 0, clear_snmp_wrap_smob);
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


static oid wrap_const_SNMP_MSG_GET = (oid) SNMP_MSG_GET;
static oid wrap_const_SNMP_MSG_GETNEXT = (oid) SNMP_MSG_GETNEXT;
static oid wrap_const_SNMP_MSG_RESPONSE = (oid) SNMP_MSG_RESPONSE;
static oid wrap_const_SNMP_MSG_SET = (oid) SNMP_MSG_SET;
static oid wrap_const_SNMP_MSG_TRAP = (oid) SNMP_MSG_TRAP;
static oid wrap_const_SNMP_MSG_GETBULK = (oid) SNMP_MSG_GETBULK;
static oid wrap_const_SNMP_MSG_INFORM = (oid) SNMP_MSG_INFORM;
static oid wrap_const_SNMP_MSG_TRAP2 = (oid) SNMP_MSG_TRAP2;
static oid wrap_const_SNMP_MSG_REPORT = (oid) SNMP_MSG_REPORT;

static int wrap_const_SNMP_NOSUCHOBJECT = (ASN_CONTEXT|ASN_PRIMITIVE|0x0);
static int wrap_const_SNMP_NOSUCHINSTANCE = (ASN_CONTEXT|ASN_PRIMITIVE|0x1);
static int wrap_const_SNMP_ENDOFMIBVIEW = (ASN_CONTEXT|ASN_PRIMITIVE|0x2);
static int wrap_const_STAT_SUCCESS = 0;
static int wrap_const_STAT_ERROR = 1;
static int wrap_const_STAT_TIMEOUT = 2;

static int wrap_const_ASN_BOOLEAN = 1;
static int wrap_const_ASN_INTEGER = 2;
static int wrap_const_ASN_BIT_STR = 3;
static int wrap_const_ASN_NULL = 0x05;
static int wrap_const_ASN_OBJECT_ID = 0x06;
static int wrap_const_ASN_SEQUENCE = 0x016;
static int wrap_const_ASN_SET = 0x017;
static int wrap_const_ASN_OCTET_STR = 0x04;
static int wrap_const_ASN_IPADDRESS = (ASN_APPLICATION|0);
static int wrap_const_ASN_COUNTER = (ASN_APPLICATION|1);
static int wrap_const_ASN_GAUGE = (ASN_APPLICATION|2);
static int wrap_const_ASN_UNSIGNED = (ASN_APPLICATION|2);
static int wrap_const_ASN_TIMETICKS = (ASN_APPLICATION|3);
static int wrap_const_ASN_OPAQUE = (ASN_APPLICATION|4);
static int wrap_const_ASN_NSAP = (ASN_APPLICATION|5);
static int wrap_const_ASN_COUNTER64 = (ASN_APPLICATION|6);
static int wrap_const_ASN_UINTEGER = (ASN_APPLICATION|7);
static int wrap_const_ASN_FLOAT = (ASN_APPLICATION|8);
static int wrap_const_ASN_DOUBLE = (ASN_APPLICATION|9);
static int wrap_const_ASN_INTEGER64 = (ASN_APPLICATION|10);
static int wrap_const_ASN_UNSIGNED64 = (ASN_APPLICATION|11);

static int wrap_const_SNMP_VERSION_1 = 0;
static int wrap_const_SNMP_VERSION_2c = 1;
static int wrap_const_SNMP_VERSION_2u = 2;
static int wrap_const_SNMP_VERSION_3 = 3;
static int wrap_const_SNMP_VERSION_sec = 128;
static int wrap_const_SNMP_VERSION_2p = 129;
static int wrap_const_SNMP_VERSION_2star = 130;

_wrap_oid_from_varbind (SCM s_0)
{
#define FUNC_NAME "oid-from-varbind"
  struct variable_list *arg1 = (struct variable_list *) 0 ;
  oid *arg2 = (oid *) 0 ;
  size_t *arg3 = (size_t *) 0 ;
  SCM gswig_result;
  int result;
  
  {
    // allocate a new oid( of maximum length)
    arg3 = (size_t*)calloc(1,sizeof(size_t));
    *arg3=MAX_OID_LEN;
    arg2 = (oid*)calloc(*arg3,sizeof(oid));
  }
  result = (int)oid_from_varbind(arg1,arg2,arg3);
  {
    gswig_result = scm_from_signed_integer(result);
  }
  {
    gswig_result = SCM_UNSPECIFIED;
    
    if(result){
      int i = 0;
      SCM newoid = SCM_TAKE_OIDVECTOR((SCM_T_OID*) arg2, *arg3);
      gswig_result = scm_apply(scm_goops_make,scm_list_3(scm_class_oid,scm_kw_value,newoid),SCM_EOL);
    } else {
      free(arg2);
    };
    
    free(arg3);
  }
  
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_oid_from_tree_node (SCM s_0)
{
#define FUNC_NAME "oid-from-tree-node"
  struct tree *arg1 = (struct tree *) 0 ;
  oid *arg2 = (oid *) 0 ;
  size_t *arg3 = (size_t *) 0 ;
  SCM gswig_result;
  int result;
  
  {
    // allocate a new oid( of maximum length)
    arg3 = (size_t*)calloc(1,sizeof(size_t));
    *arg3=MAX_OID_LEN;
    arg2 = (oid*)calloc(*arg3,sizeof(oid));
  }
  result = (int)oid_from_tree_node(arg1,arg2,arg3);
  {
    gswig_result = scm_from_signed_integer(result);
  }
  {
    gswig_result = SCM_UNSPECIFIED;
    
    if(result){
      int i = 0;
      SCM newoid = SCM_TAKE_OIDVECTOR((SCM_T_OID*) arg2, *arg3);
      gswig_result = scm_apply(scm_goops_make,scm_list_3(scm_class_oid,scm_kw_value,newoid),SCM_EOL);
    } else {
      free(arg2);
    };
    
    free(arg3);
  }
  
  
  return gswig_result;
#undef FUNC_NAME
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
  SCM gswig_result;
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
    gswig_result = scm_from_signed_integer(result);
  }
  
  
  
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_version_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-version-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  long arg2 ;
  SCM gswig_result;
  
  {
    arg2 = (long) scm_to_long(s_1);
  }
  if (arg1) (arg1)->version = arg2;
  gswig_result = SCM_UNSPECIFIED;
  
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_version_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-version-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM gswig_result;
  long result;
  
  result = (long) ((arg1)->version);
  {
    gswig_result = scm_from_long(result);
  }
  
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_retries_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-retries-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  int arg2 ;
  SCM gswig_result;
  
  {
    arg2 = (int) scm_to_int(s_1);
  }
  if (arg1) (arg1)->retries = arg2;
  gswig_result = SCM_UNSPECIFIED;
  
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_retries_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-retries-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM gswig_result;
  int result;
  
  result = (int) ((arg1)->retries);
  {
    gswig_result = scm_from_signed_integer(result);
  }
  
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_timeout_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-timeout-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  long arg2 ;
  SCM gswig_result;
  
  {
    arg2 = (long) scm_to_long(s_1);
  }
  if (arg1) (arg1)->timeout = arg2;
  gswig_result = SCM_UNSPECIFIED;
  
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_timeout_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-timeout-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM gswig_result;
  long result;
  
  result = (long) ((arg1)->timeout);
  {
    gswig_result = scm_from_long(result);
  }
  
  
  return gswig_result;
#undef FUNC_NAME
}

static SCM
_wrap_snmp_session_peername_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-peername-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  char *arg2 = (char *) 0 ;
  int must_free2 = 0 ;
  SCM gswig_result;
  
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
  gswig_result = SCM_UNSPECIFIED;
  
  if (must_free2 && arg2) SWIG_free(arg2);
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_peername_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-peername-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM gswig_result;
  char *result = 0 ;
  
  result = (char *) ((arg1)->peername);
  {
    gswig_result = scm_from_locale_string((const char *)result);
  }
  
  
  return gswig_result;
#undef FUNC_NAME
}

static SCM
_wrap_snmp_session_community_set (SCM s_0, SCM s_1)
{
#define FUNC_NAME "snmp-session-community-set"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  u_char *arg2 = (u_char *) 0 ;
  int must_free2 = 0 ;
  SCM gswig_result;
  
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
  gswig_result = SCM_UNSPECIFIED;
  
  if (must_free2 && arg2) SWIG_free(arg2);
  
  return gswig_result;
#undef FUNC_NAME
}


static SCM
_wrap_snmp_session_community_get (SCM s_0)
{
#define FUNC_NAME "snmp-session-community-get"
  struct snmp_session *arg1 = (struct snmp_session *) 0 ;
  SCM gswig_result;
  u_char *result = 0 ;
  
  result = (u_char *) ((arg1)->community);
  {
    gswig_result = scm_from_locale_string((const char *)result);
  }
  
  
  return gswig_result;
#undef FUNC_NAME
}


static void init_snmp_wrap(void *data)
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
  
#define MAPSRFI4(x) (x) * 57.29578)
  
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

  scm_c_define_gsubr("oid-from-varbind", 1, 0, 0, (void *) _wrap_oid_from_varbind);
  scm_c_define_gsubr("oid-from-tree-node", 1, 0, 0, (void *) _wrap_oid_from_tree_node);
  scm_c_define_gsubr("guile-snmp-async-response", 5, 0, 0, (void *) _wrap_guile_snmp_async_response);

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
  EXPORT_CONSTANT(ASN_UNSIGNED , "ASN-UNSIGNED64" , scm_from_signed_integer)

  EXPORT_CONSTANT(SNMP_NOSUCHOBJECT , "SNMP-NOSUCHOBJECT" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_NOSUCHINSTANCE , "SNMP-NOSUCHINSTANCE" , scm_from_signed_integer)
  EXPORT_CONSTANT(SNMP_ENDOFMIBVIEW , "SNMP-ENDOFMIBVIEW" , scm_from_signed_integer)
  EXPORT_CONSTANT(STAT_SUCCESS , "STAT-SUCCESS" , scm_from_signed_integer)
  EXPORT_CONSTANT(STAT_ERROR , "STAT-ERROR" , scm_from_signed_integer)
  EXPORT_CONSTANT(STAT_TIMEOUT , "STAT-TIMEOUT" , scm_from_signed_integer)

  init_snmp_wrap_classes();

  {
    SCM setter = scm_c_define_gsubr("snmp-session-version-set", 2, 0, 0, (void *) _wrap_snmp_session_version_set);
    SCM getter = scm_c_define_gsubr("snmp-session-version-get", 1, 0, 0, (void *) _wrap_snmp_session_version_get);
    scm_c_define("snmp-session-version", scm_make_procedure_with_setter(getter, setter));
  }
  {
    SCM setter = scm_c_define_gsubr("snmp-session-peername-set", 2, 0, 0, (void *) _wrap_snmp_session_peername_set);
    SCM getter = scm_c_define_gsubr("snmp-session-peername-get", 1, 0, 0, (void *) _wrap_snmp_session_peername_get);
    scm_c_define("snmp-session-peername", scm_make_procedure_with_setter(getter, setter));
  }
  {
    SCM setter = scm_c_define_gsubr("snmp-session-community-set", 2, 0, 0, (void *) _wrap_snmp_session_community_set);
    SCM getter = scm_c_define_gsubr("snmp-session-community-get", 1, 0, 0, (void *) _wrap_snmp_session_community_get);
    scm_c_define("snmp-session-community", scm_make_procedure_with_setter(getter, setter));
  }
}

SCM
scm_init_snmp_net_snmp_module (void)
{
  init_snmp_wrap_smob_type();

  scm_c_define_module("snmp net-snmp-primitive",
  init_snmp_wrap, NULL);
  return SCM_UNSPECIFIED;
}

#ifdef __cplusplus
}
#endif

