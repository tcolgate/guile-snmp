/*
 * -------------------------------------------------------------------
 *  Copyright (C) 2009-2012 Tristan Colgate
 *
 *  net-snmp_wrap_structs.h
 * -------------------------------------------------------------------
 */

typedef size_t (*free_wrap_smob_f)(SCM);
typedef int (*print_wrap_smob_f)(SCM,SCM,scm_print_state*);
typedef SCM (*equalp_wrap_smob_f)(SCM,SCM);
typedef struct wrap_smob_typedef_s {
  char *name;
  free_wrap_smob_f free_func;
  print_wrap_smob_f print_func;
  equalp_wrap_smob_f equalp_func;
  SCM ptrclass;
} wrap_smob_typedef_t;

static scm_t_bits snmp_wrap_smob_tag;
typedef enum snmp_wrap_smob_subtypes {
  smob_snmp_session = 0,
  smob_snmp_single_session,
  smob_tree,
  smob_mib_module,
  smob_pdu,
  smob_pdu_variable,
  smob_snmp_fdinfo,
  smob_netsnmp_transport,
  smob_netsnmp_mib_handler,
  smob_netsnmp_handler_registration,
  smob_netsnmp_handler_args,
  smob_netsnmp_delegated_cache,
  smob_netsnmp_agent_request_info,
  smob_netsnmp_request_info,
  smob_netsnmp_iterator_info,
  smob_netsnmp_table_registration_info,
  smob_netsnmp_table_request_info,
  smob_last
} snmp_wrap_smob_subtypes_e;


wrap_smob_typedef_t wrap_smob_types[] = {
  {"<snmp-session>", NULL, NULL, NULL, NULL},
  {"<snmp-single-session>", NULL, NULL, NULL, NULL},
  {"<tree>", NULL, NULL, NULL, NULL},
  {"<mib-module>", NULL, NULL, NULL, NULL},
  {"<pdu>", NULL, NULL, NULL, NULL},
  {"<pdu-variable>", NULL, NULL, NULL, NULL},
  {"<snmp-fdinfo>", NULL, NULL, NULL, NULL},
  {"<netsnmp-transport>", NULL, NULL, NULL, NULL},
  {"<netsnmp-mib-handler>", NULL, NULL, NULL, NULL},
  {"<netsnmp-mib-handler-registration>", NULL, NULL, NULL, NULL},
  {"<netsnmp-handler-args>", NULL, NULL, NULL, NULL},
  {"<netsnmp-delegated-cache>", NULL, NULL, NULL, NULL},
  {"<netsnmp-agent-request-info>", NULL, NULL, NULL, NULL},
  {"<netsnmp-request-info>", NULL, NULL, NULL, NULL},
  {"<netsnmp-iterator-info>", NULL, NULL, NULL, NULL},
  {"<netsnmp-table-registration-info>", NULL, NULL, NULL, NULL},
  {"<netsnmp-table-request-info>", NULL, NULL, NULL, NULL},
  {NULL, NULL, NULL, NULL, NULL}
};

size_t
free_snmp_wrap_smob (SCM smob)
{
  scm_assert_smob_type (snmp_wrap_smob_tag, smob);
  snmp_wrap_smob_subtypes_e subtype = SCM_SMOB_FLAGS(smob);
  if(wrap_smob_types[SCM_SMOB_FLAGS(smob)].free_func){
    return wrap_smob_types[SCM_SMOB_FLAGS(smob)].free_func(smob);
  };
  scm_remember_upto_here_1(smob);
  return 0;
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
  snprintf(str,128,"#<snmp-wrap-smob: %p %i>",data,flags);
  scm_puts (str, port);

  scm_remember_upto_here_1(smob);
  scm_remember_upto_here_1(port);
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
  scm_set_smob_mark (snmp_wrap_smob_tag, 0);
  scm_set_smob_print (snmp_wrap_smob_tag, print_snmp_wrap_smob);
  scm_set_smob_equalp (snmp_wrap_smob_tag, equalp_snmp_wrap_smob);
}

void
init_snmp_wrap_classes(void)
{
  init_snmp_wrap_smob_type();
  snmp_wrap_smob_subtypes_e last = smob_last;
};

static SCM
make_wrapped_pointer (snmp_wrap_smob_subtypes_e type, void* wrapstruct)
{
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, wrapstruct);
  SCM_SET_SMOB_FLAGS (smob, type);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  SCM inst = scm_apply(make_func, scm_list_1(scm_variable_ref(scm_c_lookup(wrap_smob_types[type].name))),SCM_EOL);
  scm_slot_set_x(inst,ptrsym,smob);

  scm_remember_upto_here_1(smob);

  return inst;
}

inline void
assert_smob_subtype(snmp_wrap_smob_subtypes_e type, SCM smob)
{
  if(SCM_SMOB_FLAGS(smob) != type) {
    char* message1 = "Wrong type, got ";
    char* message2 = " expected ";
    int len = strlen(message1) + strlen(message2) +
	      strlen(wrap_smob_types[type].name) +
	      strlen(wrap_smob_types[SCM_SMOB_FLAGS(smob)].name) + 1;
    char* temp = (char*)scm_calloc(16*(sizeof(char)));

    snprintf(temp,len,"%s%s%s%s"
                     ,message1
		     ,wrap_smob_types[type].name
		     ,message2
		     ,wrap_smob_types[SCM_SMOB_FLAGS(smob)].name);

    scm_throw(
      scm_from_utf8_symbol("snmperror"),
        scm_list_1(scm_from_utf8_string(temp)));
  };
}

#define ASSERT_NOT_NULL_PTR( SCM , PTR ) \
       if( PTR == NULL ) scm_throw( scm_from_utf8_symbol("error"),\
        scm_list_2(scm_from_utf8_string("NULL pointer access"), SCM ));

void*
pointer_from_wrapped_smob(snmp_wrap_smob_subtypes_e type, SCM obj)
{
  SCM ptrsym = scm_from_utf8_symbol("ptr");
  SCM smob = scm_slot_ref(obj, ptrsym);
  assert_smob_subtype(type, smob);
  scm_remember_upto_here_1(obj);
  return (void*) SCM_SMOB_DATA (smob);
};

static SCM
read_only_setter(SCM s_0, SCM s_1)
{
  return SCM_UNSPECIFIED;
};

/*
 * Wrap struct netsnmp_session
 */

static SCM
_wrap_initialize_snmp_session (SCM obj, SCM args)
{
  struct snmp_session *ptr = scm_gc_calloc(sizeof(struct snmp_session), "snmp session");
  snmp_sess_init(ptr);
  ptr->callback = NULL;
  ptr->callback_magic = SCM_BOOL_F;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, (void*)ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_snmp_session);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_version_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  scm_remember_upto_here_1(s_0);
  return scm_constant_name_from_int("<snmp-version>", session->version);
}

static SCM
_wrap_snmp_session_version_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  session->version = scm_int_from_constant("<snmp-version>",s_1);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_retries_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  scm_remember_upto_here_1(s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  return scm_from_int(session->retries);
}

static SCM
_wrap_snmp_session_retries_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);

  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->retries = scm_to_int(s_1);
  scm_remember_upto_here_1(s_0);

  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_timeout_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_long(session->timeout);
}

static SCM
_wrap_snmp_session_timeout_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->timeout = scm_to_long(s_1);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_peername_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_latin1_string(session->peername);
}

static SCM
_wrap_snmp_session_peername_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->peername = scm_to_latin1_string(s_1);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_localname_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_latin1_string(session->localname);
}

static SCM
_wrap_snmp_session_localname_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->localname = scm_to_latin1_string(s_1);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_local_port_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_ushort(session->local_port);
}

static SCM
_wrap_snmp_session_local_port_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->local_port = scm_to_ushort(s_1);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_community_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_utf8_string(session->community);
}

static SCM
_wrap_snmp_session_community_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->community = scm_to_utf8_string(s_1);
  session->community_len = strlen(session->community);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_context_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_utf8_string(session->contextName);
}

static SCM
_wrap_snmp_session_context_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->contextName = scm_to_utf8_string(s_1);
  session->contextNameLen = strlen(session->contextName);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

int guile_snmp_async_response(int op, struct snmp_session *sess, int reqid, struct snmp_pdu *pdu, void *magic)
{
    SCM res = scm_call_4(
      (SCM) magic,
      scm_constant_name_from_int("<callback-op>", op),
      make_wrapped_pointer(smob_snmp_session ,(void*) sess),
      scm_from_int(reqid),
      make_wrapped_pointer(smob_pdu ,(void*) pdu));
    return scm_to_int(res);
};

SCM
_wrap_snmp_session_callback_get(SCM s_0) {
  struct snmp_session *p = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  return (SCM) p->callback_magic;
};

SCM
_wrap_snmp_session_callback_set(SCM s_0, SCM s_1) {
  struct snmp_session *p = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  if(s_1 == SCM_BOOL_F){
    p->callback = NULL;
    return SCM_UNSPECIFIED;
  };
  p->callback = guile_snmp_async_response;
  p->callback_magic = s_1;

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
};

static SCM
_wrap_snmp_session_securityName_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_latin1_string(session->securityName);
}

static SCM
_wrap_snmp_session_securityName_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->securityName = scm_to_latin1_string(s_1);
  session->securityNameLen = strlen(session->securityName);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_securityLevel_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_constant_name_from_int("<snmp-sec-level>", session->securityLevel);
}

static SCM
_wrap_snmp_session_securityLevel_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->securityLevel = scm_int_from_constant("<snmp-sec-level>",s_1);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_securityAuthProto_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_oid(session->securityAuthProto,session->securityAuthProtoLen);
}

static SCM
_wrap_snmp_session_securityAuthProto_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )

  size_t oidlen = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(oidlen * sizeof(oid));
  scm_to_oid(s_1,&temp_oid,&oidlen);

  session->securityAuthProto = temp_oid;
  session->securityAuthProtoLen = oidlen;

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_securityAuthKey_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_latin1_string(session->securityAuthKey);
}

static SCM
_wrap_snmp_session_securityAuthKey_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )

  u_char* passphrase = scm_to_locale_string(s_1);
  session->securityAuthKeyLen = USM_AUTH_KU_LEN;

  if (generate_Ku(session->securityAuthProto,
                  session->securityAuthProtoLen,
                  (u_char *) passphrase, strlen(passphrase),
                  session->securityAuthKey,
                  &session->securityAuthKeyLen) != SNMPERR_SUCCESS) {
    scm_throw(
      scm_from_utf8_symbol("snmperror"),
        scm_list_1(scm_from_utf8_string("Failed to generate AuthProtoKey")));
  }

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_securityPrivProto_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_oid(session->securityAuthProto,session->securityAuthProtoLen);
}

static SCM
_wrap_snmp_session_securityPrivProto_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )

  size_t oidlen = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(oidlen * sizeof(oid));
  scm_to_oid(s_1,&temp_oid,&oidlen);

  session->securityPrivProto = temp_oid;
  session->securityPrivProtoLen = oidlen;

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_session_securityPrivKey_get (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  scm_remember_upto_here_1(s_0);
  return scm_from_latin1_string(session->securityPrivKey);
}

static SCM
_wrap_snmp_session_securityPrivKey_set (SCM s_0, SCM s_1)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , session )
  session->securityPrivKeyLen = USM_PRIV_KU_LEN;

  u_char* passphrase = scm_to_locale_string(s_1);
  if (generate_Ku(session->securityAuthProto,
                  session->securityAuthProtoLen,
                  (u_char *) passphrase, strlen(passphrase),
                  session->securityPrivKey,
                  &session->securityPrivKeyLen) != SNMPERR_SUCCESS) {
    scm_throw(
      scm_from_utf8_symbol("snmperror"),
        scm_list_1(scm_from_utf8_string("Failed to generate PrivProtoKey")));
  }

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return SCM_UNSPECIFIED;
}
/*
 * Wrap struct void* as snmp-single-session
 */

static SCM
_wrap_initialize_snmp_single_session (SCM obj, SCM args)
{
  void *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_snmp_single_session);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

/*
 * Wrap struct tree
 */

static SCM
_wrap_initialize_tree (SCM obj)
{
  return SCM_UNSPECIFIED;
}

static SCM
make_tree_smob_from_ptr(struct tree *ptr)
{
  return make_wrapped_pointer(smob_tree ,(void*) ptr);
};

static SCM
_wrap_tree_label_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  scm_remember_upto_here_1(tree);
  return scm_from_utf8_string(node->label);
}

static SCM
_wrap_tree_description_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  if(node->description){
    return scm_from_utf8_string(node->description);
  };
  scm_remember_upto_here_1(tree);
  return scm_from_utf8_string("No description set, MIB description may not be being loaded");
}

static SCM
_wrap_tree_type_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  scm_remember_upto_here_1(tree);
  return scm_constant_name_from_int("<mib-type>", node->type);
}

static SCM
_wrap_tree_access_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  scm_remember_upto_here_1(tree);
  return scm_constant_name_from_int("<mib-access>", node->access);
}

static SCM
_wrap_tree_status_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  scm_remember_upto_here_1(tree);
  return scm_constant_name_from_int("<mib-status>", node->status);
}

static SCM
_wrap_tree_units_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  scm_remember_upto_here_1(tree);
  return node->units != NULL ? scm_from_utf8_string(node->units) : SCM_BOOL_F;
}

static SCM
_wrap_tree_hint_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  scm_remember_upto_here_1(tree);
  return node->hint != NULL ? scm_from_utf8_string(node->hint) : SCM_BOOL_F;
}

static SCM
_wrap_tree_enums_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  SCM result = SCM_EOL;
  struct enum_list* p = node->enums;
  while(p != NULL){
    result = scm_append(scm_list_2(result,
			    scm_list_1(scm_cons(
					    scm_from_signed_integer(p->value),
					    scm_from_latin1_symbol(p->label)))));
    p = p->next;
  };
  scm_remember_upto_here_1(tree);
  return result;
}

static SCM
_wrap_tree_indexes_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  SCM result = SCM_EOL;
  struct index_list* p = node->indexes;
  while(p != NULL){
    result = scm_append(scm_list_2(result, scm_list_1(scm_from_latin1_string(p->ilabel))));
    p = p->next;
  };

  scm_remember_upto_here_1(tree);
  return result;
}

static SCM
_wrap_tree_varbinds_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  SCM result = SCM_EOL;
  struct varbind_list* p = node->varbinds;
  while(p != NULL){
    result = scm_append(scm_list_2(result, scm_list_1(scm_from_latin1_string(p->vblabel))));
    p = p->next;
  };

  scm_remember_upto_here_1(tree);
  return result;
}

static SCM
_wrap_tree_parent_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  scm_remember_upto_here_1(tree);
  return make_wrapped_pointer(smob_tree ,node->parent);
}

static SCM
_wrap_tree_peers_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  SCM result = SCM_EOL;
  struct tree* p = node->next_peer;
  while(p != NULL){
    result = scm_append(scm_list_2(result, scm_list_1(make_wrapped_pointer(smob_tree ,p))));
    p = p->next_peer;
  };

  scm_remember_upto_here_1(tree);
  return result;
}

static SCM
_wrap_tree_children_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  SCM result = SCM_EOL;
  struct tree* p = node->child_list;
  while(p != NULL){
    result = scm_append(
               scm_list_2(result,
                         scm_list_1(
                           make_wrapped_pointer(smob_tree ,p))));
    p = p->next_peer;
  };

  scm_remember_upto_here_1(tree);
  return result;
}

static SCM
_wrap_tree_module_get (SCM tree)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, tree);
  ASSERT_NOT_NULL_PTR( tree , node )
  scm_remember_upto_here_1(tree);
  return make_wrapped_pointer(smob_mib_module ,find_module(node->modid));
}

/*
 * Wrap struct module
 */

static SCM
_wrap_initialize_module (SCM obj)
{
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_module_name_get (SCM s_0)
{
  struct module *p = (struct module*) pointer_from_wrapped_smob(smob_mib_module, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  scm_remember_upto_here_1(s_0);
  return p->name != NULL ? scm_from_utf8_string(p->name) : SCM_BOOL_F;
}

static SCM
_wrap_module_file_get (SCM s_0)
{
  struct module *p = (struct module*) pointer_from_wrapped_smob(smob_mib_module, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  scm_remember_upto_here_1(s_0);
  return p->file != NULL ? scm_from_utf8_string(p->file) : SCM_BOOL_F;
}

/*
 * Wrap netsnmp_pdu* as pdu
 */

static SCM
_wrap_initialize_pdu (SCM obj, SCM args)
{
  void *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_pdu);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);

  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);

  return SCM_UNSPECIFIED;
}

static SCM
_wrap_pdu_errstat_get (SCM s_0)
{
  netsnmp_pdu *p = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  scm_remember_upto_here_1(s_0);
  return scm_constant_name_from_int("<snmp-err-status>", p->errstat);
}

static SCM
_wrap_pdu_variables_get (SCM s_0)
{
  netsnmp_pdu *p = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  SCM res = SCM_EOL;
  netsnmp_variable_list *curr = p->variables;
  while(curr){
    res = scm_append(scm_list_2(res, scm_list_1(make_wrapped_pointer(smob_pdu_variable,curr))));
    curr = curr->next_variable;
  };

  scm_remember_upto_here_1(s_0);
  return res;
}

#undef non_repeaters
static SCM
_wrap_pdu_non_repeaters_get (SCM s_0)
{
  netsnmp_pdu *p = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  scm_remember_upto_here_1(s_0);
  return scm_from_long(p->errstat);
}

static SCM
_wrap_pdu_non_repeaters_set (SCM s_0, SCM s_1)
{
  netsnmp_pdu *p = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  p->errstat = scm_to_long(s_1);
  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

#undef max_repeaters
static SCM
_wrap_pdu_max_repetitions_get (SCM s_0)
{
  netsnmp_pdu *p = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  scm_remember_upto_here_1(s_0);
  return scm_from_long(p->errindex);
}

static SCM
_wrap_pdu_max_repetitions_set (SCM s_0, SCM s_1)
{
  netsnmp_pdu *p = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  p->errindex = scm_to_long(s_1);
  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

/*
 * Wrap struct variable_list
 */

static SCM
_wrap_initialize_pdu_variable (SCM obj, SCM args)
{
  void *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_pdu_variable);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_pdu_variable_variables_get (SCM s_0)
{
  netsnmp_variable_list *p = (netsnmp_variable_list*) pointer_from_wrapped_smob(smob_pdu_variable, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  netsnmp_variable_list *curr = p;
  SCM res = SCM_EOL;
  while(curr){
    res = scm_append(scm_list_2(res, scm_list_1(make_wrapped_pointer(smob_pdu_variable,curr))));
    curr = curr->next_variable;
  };

  scm_remember_upto_here_1(s_0);
  return res;
}
SCM
_wrap_pdu_variable_name_get (SCM s_0)
{
  netsnmp_variable_list *var = (netsnmp_variable_list*) pointer_from_wrapped_smob(smob_pdu_variable, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , var )
  scm_remember_upto_here_1(s_0);
  return scm_from_oid(var->name,var->name_length);
}

SCM
_wrap_pdu_variable_type_get (SCM s_0)
{
  netsnmp_variable_list *var = (netsnmp_variable_list*) pointer_from_wrapped_smob(smob_pdu_variable, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , var )
  scm_remember_upto_here_1(s_0);
  return scm_constant_name_from_int("<asn-type>", var->type);
}

SCM
_wrap_pdu_variable_value_get(SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_variable_list *p = (netsnmp_variable_list*) pointer_from_wrapped_smob(smob_pdu_variable, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  switch(p->type){
    case ASN_OPAQUE:
    case ASN_NSAP:
    case ASN_OCTET_STR:
    case ASN_BIT_STR:
      {
        // These should probably be handled differently
        result = scm_from_latin1_stringn((p->val).string,p->val_len);
      }
      break;

    case ASN_INTEGER:
      {
        result = scm_from_int(*((p->val).integer));
      }
      break;

    case ASN_UINTEGER:
    case ASN_GAUGE:
    case ASN_COUNTER:
    case ASN_TIMETICKS:
      {
        result = scm_from_uint(*((p->val).integer));
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

    case ASN_NULL:
      {
        //lambda pdu: None,
        result = SCM_EOL;
      }
      break;

    case ASN_OBJECT_ID:
      {
        // Guile wants to take ownership of the array
        // so we copy it first.
        oid* temp = (oid*)scm_calloc(p->val_len);
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
        char* temp = (char*)scm_calloc(16*(sizeof(char)));
        unsigned int a = ((unsigned char*)((p->val).bitstring))[0];
        unsigned int b = ((unsigned char*)((p->val).bitstring))[1];
        unsigned int c = ((unsigned char*)((p->val).bitstring))[2];
        unsigned int d = ((unsigned char*)((p->val).bitstring))[3];
        snprintf(temp,16,"%u.%u.%u.%u",a,b,c,d);
        result = scm_from_locale_string(temp);
      }
      break;

    case ASN_FLOAT:
    case ASN_OPAQUE_FLOAT:
      {
        result = scm_from_double((double)*((p->val).floatVal));
      }
      break;

    case ASN_DOUBLE:
    case ASN_OPAQUE_DOUBLE:
      {
        result = scm_from_double(*((p->val).doubleVal));
      }
      break;

    case SNMP_NOSUCHOBJECT:
      {
        result = scm_string_to_symbol(scm_from_locale_string("noSuchObject"));
      }
      break;

    case SNMP_NOSUCHINSTANCE:
      {
        result = scm_string_to_symbol(scm_from_locale_string("noSuchInstance"));
      }
      break;

    case SNMP_ENDOFMIBVIEW:
      {
        result = scm_string_to_symbol(scm_from_locale_string("endOfMibView"));
      }
      break;

    default:
      // use snprint_value to format the value as a string
      break;
  };
  scm_remember_upto_here_1(s_0);
  return result;
};

SCM
_wrap_pdu_variable_value_bytes_get(SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_variable_list *p = (netsnmp_variable_list*) pointer_from_wrapped_smob(smob_pdu_variable, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  int len = p->val_len;
  scm_t_uint8 *data = p->val.string;
  result = scm_c_make_bytevector (len);
  int i;
  for(i = 0; i < len ; i++){
    scm_c_bytevector_set_x(result,i,data[i]);
  };

  scm_remember_upto_here_1(s_0);
  return result;
};

/*
 * Asynch fdset info
 */

#include <sys/select.h>

typedef struct snmp_fdinfo_s {
  int fds;
  int block;
  fd_set fdss;
  struct timeval timeout;
} snmp_fdinfo;

static SCM
_wrap_initialize_snmp_fdinfo (SCM obj, SCM args)
{
  snmp_fdinfo *ptr = (snmp_fdinfo *) scm_calloc(sizeof(snmp_fdinfo));
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_snmp_fdinfo);
  ptr->fds=0;
  ptr->block=1;
  FD_ZERO(&(ptr->fdss));

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_fdinfo_fd_list_get(SCM s_0)
{
  snmp_fdinfo *p = (void*) pointer_from_wrapped_smob(smob_snmp_fdinfo, s_0);

  int fd = 0;
  SCM res = SCM_EOL;
  for(fd = 0; fd < p->fds; fd++){
    if(FD_ISSET(fd,&(p->fdss))){
      res = scm_append(
          scm_list_2(res,
            scm_list_1(
              scm_from_int(fd))));
    };
  };

  scm_remember_upto_here_1(s_0);
  return res;
};


/*
 * netsnmp_transport
 */

static SCM
_wrap_initialize_netsnmp_transport (SCM obj, SCM args)
{
  netsnmp_transport *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_transport);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

/*
 * netsnmp_mib_handler
 */

static SCM
_wrap_initialize_netsnmp_mib_handler (SCM obj, SCM args)
{
  netsnmp_mib_handler *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_mib_handler);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

/*
 * netsnmp_handler_registration
 */

static SCM
_wrap_initialize_netsnmp_handler_registration (SCM obj, SCM args)
{
  netsnmp_handler_registration *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_handler_registration);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

/*
 * netsnmp_handler_args
 */

static SCM
_wrap_initialize_netsnmp_handler_args (SCM obj, SCM args)
{
  netsnmp_handler_args *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_handler_args);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

/*
 * netsnmp_mib_delegated_cache
 */

static SCM
_wrap_initialize_netsnmp_delegated_cache (SCM obj, SCM args)
{
  netsnmp_delegated_cache *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_delegated_cache);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}


/*
 * netsnmp_mib_agent_request_info
 */

static SCM
_wrap_initialize_netsnmp_agent_request_info (SCM obj, SCM args)
{
  netsnmp_agent_request_info *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_agent_request_info);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

SCM
_wrap_netsnmp_agent_request_info_mode_get(SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_agent_request_info *p = (netsnmp_agent_request_info*) pointer_from_wrapped_smob(smob_netsnmp_agent_request_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  result = scm_constant_name_from_int("<mib-handler-mode>", p->mode);

  scm_remember_upto_here_1(s_0);
  return result;
};

/*
 * netsnmp_mib_request_info
 */

static SCM
_wrap_initialize_netsnmp_request_info (SCM obj, SCM args)
{
  netsnmp_request_info *ptr = NULL;
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_request_info);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_request_info_requestvb_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_request_info *p = (netsnmp_request_info*) pointer_from_wrapped_smob(smob_netsnmp_request_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  netsnmp_variable_list *res = p->requestvb;
  result = make_wrapped_pointer(smob_pdu_variable,res);

  scm_remember_upto_here_1(s_0);
  return result;
}

static SCM
_wrap_netsnmp_request_info_next_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_request_info *p = (netsnmp_request_info*) pointer_from_wrapped_smob(smob_netsnmp_request_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  netsnmp_request_info *res = p->next;
  result = make_wrapped_pointer(smob_netsnmp_request_info,res);

  scm_remember_upto_here_1(s_0);
  return result;
}

static SCM
_wrap_netsnmp_request_info_prev_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_request_info *p = (netsnmp_request_info*) pointer_from_wrapped_smob(smob_netsnmp_request_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  netsnmp_request_info *res = p->prev;
  result = make_wrapped_pointer(smob_netsnmp_request_info,res);

  scm_remember_upto_here_1(s_0);
  return result;
}

/*
 * netsnmp_iterator_info
 */

netsnmp_variable_list *
guileSnmp_Netsnmp_First_Data_Point(
    void **my_loop_context,
    void **my_data_context,
    netsnmp_variable_list *put_index_data,
    netsnmp_iterator_info *mydata)
{
  SCM gfdp = scm_list_ref(mydata->myvoid,scm_from_int(0));
  *my_loop_context = (void*) scm_make_variable(SCM_BOOL_F);
  *my_data_context = (void*) scm_make_variable(SCM_BOOL_F);
  SCM vl = make_wrapped_pointer(smob_pdu_variable, put_index_data);
  SCM ii = make_wrapped_pointer(smob_netsnmp_iterator_info, mydata);
  SCM res = scm_call_4(gfdp, *my_loop_context, *my_data_context, vl, ii);
  return res != SCM_BOOL_F ? pointer_from_wrapped_smob(smob_pdu_variable, res) : NULL ;
}

netsnmp_variable_list *
guileSnmp_Netsnmp_Next_Data_Point(
    void **my_loop_context,
    void **my_data_context,
    netsnmp_variable_list *put_index_data,
    netsnmp_iterator_info *mydata)
{
  SCM gndp = scm_list_ref(mydata->myvoid,scm_from_int(1));
  SCM vl = make_wrapped_pointer(smob_pdu_variable, put_index_data);
  SCM ii = make_wrapped_pointer(smob_netsnmp_iterator_info, mydata);
  SCM res = scm_call_4(gndp, *my_loop_context, *my_data_context, vl, ii);
  return res != SCM_BOOL_F ? pointer_from_wrapped_smob(smob_pdu_variable, res) : NULL ;
}

static SCM
_wrap_initialize_netsnmp_iterator_info (SCM obj, SCM args)
{
  netsnmp_iterator_info *ptr = SNMP_MALLOC_TYPEDEF(netsnmp_iterator_info);
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_iterator_info);

// TODO Lifetime may be an issue here
  ptr->myvoid = scm_permanent_object(scm_list_2(SCM_BOOL_F,SCM_BOOL_F));
  ptr->get_first_data_point = guileSnmp_Netsnmp_First_Data_Point;
  ptr->get_next_data_point = guileSnmp_Netsnmp_Next_Data_Point;
  ptr->table_reginfo = NULL;

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_iterator_info_get_first_data_point_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_iterator_info *p =
    (netsnmp_iterator_info*) pointer_from_wrapped_smob(smob_netsnmp_iterator_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  SCM gfdp = scm_list_ref(p->myvoid,scm_from_int(0));

  scm_remember_upto_here_1(s_0);
  return gfdp;
}

static SCM
_wrap_netsnmp_iterator_info_get_first_data_point_set (SCM s_0, SCM s_1)
{
  netsnmp_iterator_info *p =
    (netsnmp_iterator_info*) pointer_from_wrapped_smob(smob_netsnmp_iterator_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  SCM gfdp = scm_list_set_x(p->myvoid,scm_from_int(0),s_1);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_iterator_info_get_next_data_point_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_iterator_info *p =
    (netsnmp_iterator_info*) pointer_from_wrapped_smob(smob_netsnmp_iterator_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  SCM gndp = scm_list_ref(p->myvoid,scm_from_int(1));

  scm_remember_upto_here_1(s_0);
  return gndp;
}

static SCM
_wrap_netsnmp_iterator_info_get_next_data_point_set (SCM s_0, SCM s_1)
{
  netsnmp_iterator_info *p =
    (netsnmp_iterator_info*) pointer_from_wrapped_smob(smob_netsnmp_iterator_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  SCM gndp = scm_list_set_x(p->myvoid,scm_from_int(1),s_1);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_iterator_info_table_reginfo_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_iterator_info *p =
    (netsnmp_iterator_info*) pointer_from_wrapped_smob(smob_netsnmp_iterator_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  result = make_wrapped_pointer(smob_netsnmp_table_registration_info, p->table_reginfo);

  scm_remember_upto_here_1(s_0);
  return result;
}

static SCM
_wrap_netsnmp_iterator_info_table_reginfo_set (SCM s_0, SCM s_1)
{
  netsnmp_iterator_info *p =
    (netsnmp_iterator_info*) pointer_from_wrapped_smob(smob_netsnmp_iterator_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  netsnmp_table_registration_info *ptinfo =
    (netsnmp_table_registration_info*) pointer_from_wrapped_smob(smob_netsnmp_table_registration_info, s_1);

  p->table_reginfo = ptinfo;

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

/*
 * netsnmp_table_registration_info
 */

static SCM
_wrap_initialize_netsnmp_table_registration_info (SCM obj, SCM args)
{
  netsnmp_table_registration_info *ptr = SNMP_MALLOC_TYPEDEF(netsnmp_table_registration_info);
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_table_registration_info);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_table_registration_info_min_column_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_table_registration_info *p =
    (netsnmp_table_registration_info*) pointer_from_wrapped_smob(smob_netsnmp_table_registration_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  unsigned int res = p->min_column;
  result = scm_from_uint(res);

  scm_remember_upto_here_1(s_0);
  return result;
}

static SCM
_wrap_netsnmp_table_registration_info_min_column_set (SCM s_0, SCM s_1)
{
  netsnmp_table_registration_info *p =
    (netsnmp_table_registration_info*) pointer_from_wrapped_smob(smob_netsnmp_table_registration_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  p->min_column = scm_to_uint(s_1);
  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_table_registration_info_max_column_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_table_registration_info *p =
    (netsnmp_table_registration_info*) pointer_from_wrapped_smob(smob_netsnmp_table_registration_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  unsigned int res = p->max_column;
  result = scm_from_uint(res);

  scm_remember_upto_here_1(s_0);
  return result;
}

static SCM
_wrap_netsnmp_table_registration_info_max_column_set (SCM s_0, SCM s_1)
{
  netsnmp_table_registration_info *p =
    (netsnmp_table_registration_info*) pointer_from_wrapped_smob(smob_netsnmp_table_registration_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )
  p->max_column = scm_to_uint(s_1);
  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
}

/*
 * netsnmp_table_request_info
 */

static SCM
_wrap_initialize_netsnmp_table_request_info (SCM obj, SCM args)
{
  netsnmp_table_request_info *ptr = SNMP_MALLOC_TYPEDEF(netsnmp_table_request_info);
  SCM smob;
  SCM_NEWSMOB (smob, snmp_wrap_smob_tag, ptr);
  SCM_SET_SMOB_FLAGS (smob, smob_netsnmp_table_request_info);

  SCM ptrsym = scm_from_utf8_symbol("ptr");
  scm_slot_set_x(obj,ptrsym,smob);
  scm_remember_upto_here_1(obj);
  scm_remember_upto_here_1(args);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_table_request_info_colnum_get (SCM s_0)
{
  SCM result = SCM_UNSPECIFIED;
  netsnmp_table_request_info *p =
    (netsnmp_table_request_info*) pointer_from_wrapped_smob(smob_netsnmp_table_request_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , p )

  unsigned int res = p->colnum;
  result = scm_from_uint(res);

  scm_remember_upto_here_1(s_0);
  return result;
}


#define DEFINE_SLOT_READWRITE(strtype , type , strslot , slot) \
  scm_c_define( strtype "-" strslot, scm_make_procedure_with_setter(\
    scm_c_define_gsubr( strtype "-" strslot "-get", 1, 0, 0, (void *) _wrap_ ## type ## _ ## slot ## _get),\
    scm_c_define_gsubr( strtype "-" strslot "-set", 2, 0, 0, (void *) _wrap_ ## type ## _ ## slot ## _set)));\
  scm_c_export( strtype "-" strslot , NULL);\
  scm_c_export( strtype "-" strslot "-get" , NULL);\
  scm_c_export( strtype "-" strslot "-set" , NULL);\

#define DEFINE_SLOT_READONLY(strtype , type , strslot , slot) \
  scm_c_define( strtype "-" strslot, scm_make_procedure_with_setter(\
    scm_c_define_gsubr( strtype "-" strslot "-get", 1, 0, 0, (void *) _wrap_ ## type ## _ ## slot ## _get),\
    scm_c_define_gsubr( strtype "-" strslot "-set", 2, 0, 0, (void *) read_only_setter)));\
  scm_c_export( strtype "-" strslot , NULL);\
  scm_c_export( strtype "-" strslot "-get" , NULL);\
  scm_c_export( strtype "-" strslot "-set" , NULL);\

static void init_snmp_wrap_structs(void)
{
  DEFINE_SLOT_READONLY("tree" , tree , "label" ,label)
  DEFINE_SLOT_READONLY("tree" , tree , "description" ,description)
  DEFINE_SLOT_READONLY("tree" , tree , "type" ,type)
  DEFINE_SLOT_READONLY("tree" , tree , "status" ,status)
  DEFINE_SLOT_READONLY("tree" , tree , "access" ,access)
  DEFINE_SLOT_READONLY("tree" , tree , "units" ,units)
  DEFINE_SLOT_READONLY("tree" , tree , "display-hint" ,hint)
  DEFINE_SLOT_READONLY("tree" , tree , "enums" ,enums)
  DEFINE_SLOT_READONLY("tree" , tree , "indexes" ,indexes)
  DEFINE_SLOT_READONLY("tree" , tree , "varbinds" ,varbinds)
  DEFINE_SLOT_READONLY("tree" , tree , "parent" ,parent)
  DEFINE_SLOT_READONLY("tree" , tree , "peers" ,peers)
  DEFINE_SLOT_READONLY("tree" , tree , "children" ,children)
  DEFINE_SLOT_READONLY("tree" , tree , "mib-module" ,module)
  scm_c_define_gsubr ("initialize-tree", 2, 0, 0, _wrap_initialize_tree);
  scm_c_export("initialize-tree" , NULL);

  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "community" ,community)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "peername" ,peername)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "localname" ,localname)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "local-port" ,local_port)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "version" ,version)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "context" ,context)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "retries" ,retries)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "timeout" ,timeout)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "callback" ,callback)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "securityName" ,securityName)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "securityLevel" ,securityLevel)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "securityAuthProto" ,securityAuthProto)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "securityAuthKey" ,securityAuthKey)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "securityPrivProto" ,securityPrivProto)
  DEFINE_SLOT_READWRITE("snmp-session" , snmp_session , "securityPrivKey" ,securityPrivKey)
  scm_c_define_gsubr ("initialize-snmp-session", 2, 0, 0, _wrap_initialize_snmp_session);
  scm_c_export("initialize-snmp-session" , NULL);

  scm_c_define_gsubr ("initialize-snmp-single-session", 2, 0, 0, _wrap_initialize_snmp_single_session);
  scm_c_export("initialize-snmp-single-session" , NULL);

  DEFINE_SLOT_READONLY("pdu" , pdu , "errstat" ,errstat)
  DEFINE_SLOT_READONLY("pdu" , pdu , "variables" ,variables)
  DEFINE_SLOT_READWRITE("pdu" , pdu , "non-repeaters" ,non_repeaters)
  DEFINE_SLOT_READWRITE("pdu" , pdu , "max-repetitions" ,max_repetitions)
  scm_c_define_gsubr ("initialize-pdu", 2, 0, 0, _wrap_initialize_pdu);
  scm_c_export("initialize-pdu" , NULL);

  DEFINE_SLOT_READONLY("pdu-variable" , pdu_variable, "variables" ,variables)
  DEFINE_SLOT_READONLY("pdu-variable" , pdu_variable, "name" ,name)
  DEFINE_SLOT_READONLY("pdu-variable" , pdu_variable, "type" ,type)
  DEFINE_SLOT_READONLY("pdu-variable" , pdu_variable, "value" ,value)
  DEFINE_SLOT_READONLY("pdu-variable" , pdu_variable, "value-bytes" ,value_bytes)
  scm_c_define_gsubr ("initialize-pdu-variable", 2, 0, 0, _wrap_initialize_pdu_variable);
  scm_c_export("initialize-pdu-variable" , NULL);

  DEFINE_SLOT_READONLY("snmp-fdinfo" , snmp_fdinfo, "fd-list" ,fd_list)
  scm_c_define_gsubr ("initialize-snmp-fdinfo", 2, 0, 0, _wrap_initialize_snmp_fdinfo);
  scm_c_export("initialize-snmp-fdinfo" , NULL);

  scm_c_define_gsubr ("initialize-netsnmp-transport", 2, 0, 0, _wrap_initialize_netsnmp_transport);
  scm_c_export("initialize-netsnmp-transport" , NULL);

  DEFINE_SLOT_READONLY("mib-module" , module, "name" ,name)
  DEFINE_SLOT_READONLY("mib-module" , module, "file" ,file)
  scm_c_define_gsubr ("initialize-mib-module", 2, 0, 0, _wrap_initialize_module);
  scm_c_export("initialize-mib-module" , NULL);

  scm_c_define_gsubr ("initialize-netsnmp-mib-handler", 2, 0, 0, _wrap_initialize_netsnmp_mib_handler);
  scm_c_export("initialize-netsnmp-mib-handler" , NULL);
  scm_c_define_gsubr ("initialize-netsnmp-mib-handler-registration", 2, 0, 0,
      _wrap_initialize_netsnmp_handler_registration);
  scm_c_export("initialize-netsnmp-mib-handler-registration" , NULL);
  scm_c_define_gsubr ("initialize-netsnmp-handler-args", 2, 0, 0, _wrap_initialize_netsnmp_handler_args);
  scm_c_export("initialize-netsnmp-handler-args" , NULL);
  scm_c_define_gsubr ("initialize-netsnmp-delegated-cache", 2, 0, 0, _wrap_initialize_netsnmp_delegated_cache);
  scm_c_export("initialize-netsnmp-delegated-cache" , NULL);

  DEFINE_SLOT_READONLY("netsnmp-agent-request-info" ,netsnmp_agent_request_info, "mode" ,mode)
  scm_c_define_gsubr ("initialize-netsnmp-agent-request-info", 2, 0, 0, _wrap_initialize_netsnmp_agent_request_info);
  scm_c_export("initialize-netsnmp-agent-request-info" , NULL);
  DEFINE_SLOT_READONLY("netsnmp-request-info" ,netsnmp_request_info, "requestvb" ,requestvb)
  DEFINE_SLOT_READONLY("netsnmp-request-info" ,netsnmp_request_info, "next" ,next)
  DEFINE_SLOT_READONLY("netsnmp-request-info" ,netsnmp_request_info, "prev" ,prev)
  scm_c_define_gsubr ("initialize-netsnmp-request-info", 2, 0, 0, _wrap_initialize_netsnmp_request_info);
  scm_c_export("initialize-netsnmp-request-info" , NULL);

  DEFINE_SLOT_READWRITE("netsnmp-iterator-info" , netsnmp_iterator_info , "get-first-data-point" , get_first_data_point)
  DEFINE_SLOT_READWRITE("netsnmp-iterator-info" , netsnmp_iterator_info , "get-next-data-point" , get_next_data_point)
  DEFINE_SLOT_READWRITE("netsnmp-iterator-info" , netsnmp_iterator_info , "table-reginfo" , table_reginfo)
  scm_c_define_gsubr ("initialize-netsnmp-iterator-info", 2, 0, 0, _wrap_initialize_netsnmp_iterator_info);
  scm_c_export("initialize-netsnmp-iterator-info" , NULL);

  DEFINE_SLOT_READWRITE("netsnmp-table-registration-info" , netsnmp_table_registration_info , "min-column" , min_column)
  DEFINE_SLOT_READWRITE("netsnmp-table-registration-info" , netsnmp_table_registration_info , "max-column" , max_column)
  scm_c_define_gsubr ("initialize-netsnmp-table-registration-info", 2, 0, 0, _wrap_initialize_netsnmp_table_registration_info);
  scm_c_export("initialize-netsnmp-table-registration-info" , NULL);

  DEFINE_SLOT_READONLY("netsnmp-table-request-info" ,netsnmp_table_request_info, "colnum" ,colnum)
  scm_c_define_gsubr ("initialize-netsnmp-table-request-info", 2, 0, 0, _wrap_initialize_netsnmp_table_request_info);
  scm_c_export("initialize-netsnmp-table-request-info" , NULL);
}

