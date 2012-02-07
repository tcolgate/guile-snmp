
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
_wrap_tree_type_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  return scm_constant_name_from_int("<asn-type>", node->type);
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

static SCM
make_snmp_wrap_netsnmp_session_smob(void)
{
  return make_wrapped_pointer(smob_netsnmp_session
		           ,(void*) scm_gc_malloc (sizeof(netsnmp_session), "netsnmp_session"));
};


int guile_snmp_async_response(int , struct snmp_session *, int , struct snmp_pdu *, void *);

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


static void init_snmp_wrap_structs(void)
{
  scm_c_define_gsubr("tree-label-get", 1, 0, 0, (void *) _wrap_tree_label_get);
  scm_c_define_gsubr("tree-label-set", 2, 0, 0, (void *) read_only_setter);
  scm_c_export("tree-label-get" , NULL);
  scm_c_export("tree-label-set" , NULL);

  scm_c_define_gsubr("tree-type-get", 1, 0, 0, (void *) _wrap_tree_type_get);
  scm_c_define_gsubr("tree-type-set", 2, 0, 0, (void *) read_only_setter);
  scm_c_export("tree-type-get" , NULL);
  scm_c_export("tree-type-set" , NULL);

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

