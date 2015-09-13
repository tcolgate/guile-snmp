/*
 * -------------------------------------------------------------------
 *  Copyright (C) 2009-2012 Tristan Colgate
 *
 *  net-snmp_wrap_funcs.h
 * -------------------------------------------------------------------
 */

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
_wrap_read_module (SCM s_0)
{
   char *name = scm_to_locale_string(s_0);
   read_module(name);

   scm_remember_upto_here_1(s_0);
   return SCM_UNSPECIFIED;
}

static SCM
_wrap_which_module (SCM s_0)
{
   char *name = scm_to_locale_string(s_0);
   int modid = which_module(name);
   struct module *p = find_module(modid);
   SCM obj =  make_wrapped_pointer(smob_mib_module ,(void*) p);
   scm_remember_upto_here_1(s_0);
   return obj;
}

static SCM
_wrap_snmp_set_save_descriptions (SCM s_0)
{
  if(s_0 == SCM_BOOL_T){
    snmp_set_save_descriptions(1);
  } else {
    snmp_set_save_descriptions(0);
  };

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_get_tree_head (void)
{
   return make_tree_smob_from_ptr(get_tree_head());
}

static SCM
_wrap_get_tree (SCM oidscm, SCM treehead)
{
  struct tree *node = (struct tree*) pointer_from_wrapped_smob(smob_tree, treehead);
  size_t len = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(len * sizeof(oid));
  scm_to_oid(oidscm,&temp_oid,&len);

  if(len == 0) { return treehead ;};

  struct tree* result = get_tree(temp_oid,len,node);

  SCM scmresult = SCM_BOOL_F;
  if(NULL != result){
    scmresult = make_tree_smob_from_ptr(result);
  };

  free(temp_oid);

  scm_remember_upto_here_1(oidscm);
  scm_remember_upto_here_1(treehead);

  return scmresult;
}

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
    arg3 = (size_t*)scm_calloc(sizeof(size_t));
    *arg3=MAX_OID_LEN;
    arg2 = (oid*)scm_calloc(*arg3 * sizeof(oid));
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
    }
  }

  scm_remember_upto_here_1(s_0);
  return scmresult;
}

static SCM
_wrap_snmp_parse_oid (SCM oidname)
{

   size_t *oidlen = (size_t*)scm_calloc(sizeof(size_t));
   *oidlen=MAX_OID_LEN;
   oid *oidstore = (oid*)scm_calloc(*oidlen * sizeof(oid));
   char *str = scm_to_locale_string(oidname);
   oid *result = snmp_parse_oid(str, oidstore, oidlen);

   SCM scmresult = SCM_UNSPECIFIED;
   if(result){
     scmresult = scm_from_oid(oidstore,*oidlen);
   };

   scm_remember_upto_here_1(oidname);
   return scmresult;
}

static SCM
_wrap_snmp_sess_init (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  snmp_sess_init(session);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_sess_open (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  void *sessp = snmp_sess_open(session);
  if(sessp == NULL){
    return SCM_BOOL_F;
  };
  SCM obj =  make_wrapped_pointer(smob_snmp_single_session ,(void*) sessp);
  scm_remember_upto_here_1(s_0);
  return obj;
}

static SCM
_wrap_snmp_open (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  struct snmp_session *sessp = snmp_open(session);
  if(sessp == NULL){
    return SCM_BOOL_F;
  };
  SCM obj =  make_wrapped_pointer(smob_snmp_session ,(void*) sessp);
  scm_remember_upto_here_1(s_0);
  return obj;
}

static SCM
_wrap_snmp_sess_session (SCM s_0)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  struct snmp_session *childsess = snmp_sess_session(sessp);
  SCM obj = make_wrapped_pointer(smob_snmp_session ,(struct snmp_session*) childsess);
  scm_remember_upto_here_1(s_0);
  return obj;
}

static SCM
_wrap_snmp_sess_synch_response (SCM s_0, SCM s_1)
{
  int res;
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_1);
  netsnmp_pdu *respdu = NULL;
  SCM scmrespdu;

  res = snmp_sess_synch_response(sessp, pdu, &respdu);

  if(!res){
    scmrespdu = make_wrapped_pointer( smob_pdu , respdu);
  } else {
    scmrespdu = scm_constant_name_from_int( "<snmp-status>", res);
  };

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return scmrespdu;
}

static SCM
_wrap_snmp_synch_response (SCM s_0, SCM s_1)
{
  int res;
  struct snmp_session *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_1);
  netsnmp_pdu *respdu = NULL;
  SCM scmrespdu;

  res = snmp_synch_response(sessp, pdu, &respdu);

  if(!res){
    scmrespdu = make_wrapped_pointer( smob_pdu , respdu);
  } else {
    scmrespdu = scm_constant_name_from_int( "<snmp-status>", res);
  };

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return scmrespdu;
}

static SCM
_wrap_snmp_sess_close (SCM s_0)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  snmp_sess_close(sessp);
  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_close (SCM s_0)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  snmp_close(sessp);
  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_sess_error (SCM s_0)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  int cliberr,snmperr;
  char *errstring;

  snmp_sess_error(sessp,&cliberr,&snmperr,&errstring);

  scm_remember_upto_here_1(s_0);

  return scm_from_locale_string(errstring);
}

static SCM
_wrap_snmp_send (SCM s_0, SCM s_1)
{
  int res;
  SCM scmres;
  struct snmp_session *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_1);

  res = snmp_send(sessp, pdu);
  scmres = scm_constant_name_from_int( "<snmp-status>", res);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return scmres;
}

static SCM
_wrap_snmp_sess_send (SCM s_0, SCM s_1)
{
  int res;
  SCM scmres;
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_1);

  res = snmp_sess_send(sessp, pdu);
  scmres = scm_constant_name_from_int( "<snmp-status>", res);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return scmres;
}

static SCM
_wrap_snmp_async_send (SCM s_0, SCM s_1, SCM s_2)
{
  int res;
  SCM scmres;
  struct snmp_session *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_1);
  void* *callbackdata = (void*) s_2;

  res = snmp_async_send(sessp, pdu, guile_snmp_async_response, callbackdata);
  scmres = scm_constant_name_from_int( "<snmp-status>", res);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  scm_remember_upto_here_1(s_2);

  return scmres;
}

static SCM
_wrap_snmp_sess_async_send (SCM s_0, SCM s_1, SCM s_2)
{
  int res;
  SCM scmres;
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_1);
  void* *callbackdata = (void*) s_2;

  res = snmp_sess_async_send(sessp, pdu, guile_snmp_async_response, callbackdata);
  scmres = scm_constant_name_from_int( "<snmp-status>", res);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  scm_remember_upto_here_1(s_2);

  return scmres;
}

static SCM
_wrap_snmp_timeout(void)
{
  snmp_timeout();
  return SCM_UNSPECIFIED;
};

static SCM
_wrap_snmp_sess_timeout(SCM s_0)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  snmp_sess_timeout(sessp);
  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
};

static SCM
_wrap_snmp_select_info(void)
{
  snmp_fdinfo *fdinfo;
  fdinfo = (snmp_fdinfo*) scm_calloc(sizeof(snmp_fdinfo));
  fdinfo->fds=0;
  fdinfo->block=1;
  FD_ZERO(&(fdinfo->fdss));

  snmp_select_info(
    &(fdinfo->fds),
    &(fdinfo->fdss),
    &(fdinfo->timeout),
    &(fdinfo->block));

  SCM obj = make_wrapped_pointer( smob_snmp_fdinfo , fdinfo);
  return obj;
};

static SCM
_wrap_snmp_sess_select_info(SCM s_0)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);

  snmp_fdinfo *fdinfo;
  fdinfo = (snmp_fdinfo*) scm_calloc(sizeof(snmp_fdinfo));
  fdinfo->fds=0;
  fdinfo->block=1;
  FD_ZERO(&(fdinfo->fdss));

  snmp_sess_select_info(
    sessp,
    &(fdinfo->fds),
    &(fdinfo->fdss),
    &(fdinfo->timeout),
    &(fdinfo->block));

  SCM obj = make_wrapped_pointer( smob_snmp_fdinfo , fdinfo);
  scm_remember_upto_here_1(s_0);

  return obj;
};

static SCM
_wrap_snmp_select(SCM s_0)
{
  snmp_fdinfo *fdinfo = (void*) pointer_from_wrapped_smob(smob_snmp_fdinfo, s_0);

  fdinfo->fds = select(
      fdinfo->fds,
      &(fdinfo->fdss),
      NULL,
      NULL,
      fdinfo->block ?NULL : &(fdinfo->timeout));

  return scm_from_int(fdinfo->fds);
};

static SCM
_wrap_snmp_read(SCM s_0)
{
  snmp_fdinfo *fdinfo = (void*) pointer_from_wrapped_smob(smob_snmp_fdinfo, s_0);

  snmp_read(&(fdinfo->fdss));

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
};

static SCM
_wrap_snmp_sess_read(SCM s_0, SCM s_1)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  snmp_fdinfo *fdinfo = (void*) pointer_from_wrapped_smob(smob_snmp_fdinfo, s_1);

  snmp_sess_read(sessp, &(fdinfo->fdss));

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
};

static SCM
_wrap_snmp_pdu_create (SCM s_0)
{
  SCM obj = make_wrapped_pointer( smob_pdu , snmp_pdu_create( scm_int_from_constant("<snmp-msg>",s_0)));
  scm_remember_upto_here_1(s_0);
  return obj;
}

static SCM
_wrap_snmp_add_null_var (SCM s_0, SCM s_1)
{
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  size_t len = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(len * sizeof(oid));
  scm_to_oid(s_1,&temp_oid,&len);

  snmp_add_null_var(pdu,temp_oid,len);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_tdomain_transport (SCM s_0, SCM s_1, SCM s_2)
{
  char *location = scm_to_latin1_string(s_0);
  int local = s_1 == SCM_BOOL_T ? 1 : 0;
  char *trans = scm_to_latin1_string(s_2);

  netsnmp_transport *trs = netsnmp_tdomain_transport(location,local,trans);
  SCM obj = make_wrapped_pointer( smob_netsnmp_transport , trs);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  scm_remember_upto_here_1(s_2);
  return obj;
}

static SCM
_wrap_snmp_add (SCM s_0, SCM s_1)
{
  struct snmp_session *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  netsnmp_transport *trs = (void*) pointer_from_wrapped_smob(smob_netsnmp_transport, s_1);

  snmp_add(sessp,trs,NULL,NULL);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_sess_add (SCM s_0, SCM s_1)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  netsnmp_transport *trs = (void*) pointer_from_wrapped_smob(smob_netsnmp_transport, s_1);

  snmp_sess_add(sessp,trs,NULL,NULL);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return SCM_UNSPECIFIED;
}

void
scm_to_netsnmp_value_bytes(SCM valscm, int typespec, void** bytes, size_t *byteslen)
{
  scm_t_array_handle handle;
  void* pointer = NULL;
  size_t len = 0;
  size_t iter = 0;

  switch (typespec){
    case ASN_OPAQUE:
    case ASN_NSAP:
    case ASN_OCTET_STR:
    case ASN_BIT_STR:
    case ASN_IPADDRESS:
      {
        if ( ! scm_is_string(valscm) ){
          scm_throw(
              scm_string_to_symbol(
                scm_from_locale_string("snmperror")),
              scm_from_locale_string("Data is not a string"));
        };
        pointer = (void*) scm_to_latin1_stringn(valscm, &len);
      };
      break;

    case ASN_INTEGER:
      {
        if ( ! scm_is_signed_integer(valscm, LONG_MIN, LONG_MAX) ){
          scm_throw(
              scm_string_to_symbol(
                scm_from_locale_string("snmperror")),
              scm_from_locale_string("Data is not a signed integer"));
        };
        pointer =  (void*) (malloc(sizeof(long)));
        *((long*) pointer) = scm_to_long(valscm);
        len = sizeof(long);
      };
      break;

    case ASN_UINTEGER:
    case ASN_COUNTER:
    case ASN_TIMETICKS:
    case ASN_GAUGE:
      {
        if ( ! scm_is_unsigned_integer(valscm, 0, ULONG_MAX) ){
          scm_throw(
              scm_string_to_symbol(
                scm_from_locale_string("snmperror")),
              scm_from_locale_string("Data is not an unsuigned integer"));
        };
        pointer =  (void*) (malloc(sizeof(unsigned long)));
        *((unsigned long*) pointer) = scm_to_ulong(valscm);
        len = sizeof(unsigned long);
      };
      break;

    case ASN_COUNTER64:
      {
        if ( ! scm_to_int(scm_integer_length(valscm)) > 64){
          scm_throw(
              scm_string_to_symbol(
                scm_from_locale_string("snmperror")),
              scm_from_locale_string("Data is not an unsuigned integer"));
        };
        pointer =  (void*) (malloc(sizeof(struct counter64)));
        ((struct counter64*) pointer)->high = scm_to_ulong(scm_bit_extract(valscm, scm_from_int(31),scm_from_int(63)));
        ((struct counter64*) pointer)->low = scm_to_ulong(scm_bit_extract(valscm, scm_from_int(0),scm_from_int(31)));
        len = sizeof(struct counter64);
      };
      break;

    //case ASN_NULL:
    //lambda pdu: None,
    //      result = SCM_EOL;
    //            break;
    //
    //
    case ASN_OBJECT_ID:
      {
        //if ( ! scm_is_true(SCM_OIDVECTOR_P (valscm) )){
        //  scm_throw(
        //    scm_string_to_symbol(
        //      scm_from_locale_string("snmperror")),
        //    scm_from_locale_string("Data is not an oid"));
        //};
        len = MAX_OID_LEN;
        pointer = *bytes = (void*)scm_calloc(len * sizeof(oid));
        scm_to_oid(valscm,(oid**) bytes,&len);
        pointer = *bytes;
	len = len * sizeof(oid);
      };
      break;

    case ASN_FLOAT:
    case ASN_OPAQUE_FLOAT:
      {
        pointer =  (void*) (malloc(sizeof(float)));
        *((float*) pointer) = (float) scm_to_double(valscm);
        len = sizeof(float);
      };
      break;

    case ASN_DOUBLE:
    case ASN_OPAQUE_DOUBLE:
      {
        pointer =  (double*) (malloc(sizeof(double)));
        *((double*) pointer) = scm_to_double(valscm);
        len = sizeof(double);
      };
      break;

    default:
      {
        // signal an error
        scm_throw(
            scm_string_to_symbol(scm_from_locale_string("snmperror")),
            scm_string_append(
              scm_list_3(
                scm_from_locale_string("Unhandled type("),
                scm_number_to_string(scm_char_to_integer(scm_from_char(typespec)),SCM_UNDEFINED),
                scm_from_locale_string(") in set data"))));
      };
  };

  *bytes = pointer;
  *byteslen = len;
};

static SCM
_wrap_snmp_add_var (SCM s_0, SCM s_1, SCM s_2)
{
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  void* pointer = NULL;
  size_t len = 0;

  size_t oidlen = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(oidlen * sizeof(oid));
  scm_to_oid(s_1,&temp_oid,&oidlen);

  if (! SCM_CONSP(s_2) ){
    // signal an error
    scm_throw(
      scm_string_to_symbol(
        scm_from_locale_string("snmperror")),
      scm_from_locale_string("Malformed data passed to set"));
  };

  int typespec = scm_int_from_constant("<asn-type>",SCM_CAR(s_2));
  SCM valscm = SCM_CDR(s_2);

  scm_to_netsnmp_value_bytes(valscm, typespec, &pointer, &len);
  snmp_pdu_add_variable(pdu,temp_oid,oidlen,typespec,(void const *)pointer,len);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  scm_remember_upto_here_1(s_2);

  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_free_pdu (SCM s_0)
{
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  snmp_free_pdu(pdu);
  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_oid_is_subtree (SCM s_0, SCM s_1)
{
  size_t len1 = MAX_OID_LEN;
  oid* temp_oid1 = (oid*)scm_calloc(len1 * sizeof(oid));
  scm_to_oid(s_0,&temp_oid1,&len1);

  size_t len2 = MAX_OID_LEN;
  oid* temp_oid2 = (oid*)scm_calloc(len2 * sizeof(oid));
  scm_to_oid(s_1,&temp_oid2,&len2);

  SCM scmresult = scm_from_signed_integer(netsnmp_oid_is_subtree(temp_oid1, len1, temp_oid2, len2));

  free(temp_oid1);
  free(temp_oid2);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return scmresult;
}

static SCM
_wrap_mib_to_asn_type (SCM s_0)
{
  return scm_constant_name_from_int(
		  "<asn-type>",
		  mib_to_asn_type(
			  scm_int_from_constant("<mib-type>",s_0)));
}

/*
 *  Agent functions
 */

static SCM
_wrap_init_agent (SCM name)
{
  init_agent(scm_to_utf8_string (name));
  return SCM_UNSPECIFIED;
}


static SCM
_wrap_init_master_agent ()
{
  init_master_agent();
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_set_agent_agentx (SCM s_0)
{
  if(s_0 == SCM_BOOL_T){
    netsnmp_ds_set_boolean(NETSNMP_DS_APPLICATION_ID, NETSNMP_DS_AGENT_ROLE, 1);
  } else {
    netsnmp_ds_set_boolean(NETSNMP_DS_APPLICATION_ID, NETSNMP_DS_AGENT_ROLE, 0);
  };

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_init_vacm_vars ()
{
  init_vacm_vars();
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_init_usm ()
{
  init_usm();
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_netsnmp_daemonize (SCM s_0, SCM s_1)
{
  int quit = s_0 == SCM_BOOL_T ? 1 : 0;
  int logstderr = s_1 == SCM_BOOL_T ? 1 : 0;

  netsnmp_daemonize(quit, logstderr);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return SCM_UNSPECIFIED;
}

static SCM
_wrap_agent_check_and_processs (SCM s_0)
{
  if(s_0 == SCM_BOOL_T){
    agent_check_and_process(1);
  } else {
    agent_check_and_process(0);
  };

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_shutdown (SCM s_0)
{
  snmp_shutdown(scm_to_utf8_string (s_0));
  return SCM_UNSPECIFIED;
}

int
guile_snmp_Netsnmp_Node_Handler (netsnmp_mib_handler *handler,
  /** pointer to registration struct */
  netsnmp_handler_registration *reginfo,
  /** pointer to current transaction */
  netsnmp_agent_request_info *reqinfo,
  netsnmp_request_info *requests)
{
  SCM handfunc = (SCM) handler->myvoid;
  SCM scmhandler = make_wrapped_pointer( smob_netsnmp_mib_handler , handler);
  SCM scmreginfo = make_wrapped_pointer( smob_netsnmp_handler_registration , reginfo);
  SCM scmreqinfo = make_wrapped_pointer( smob_netsnmp_agent_request_info , reqinfo);
  SCM scmrequests = make_wrapped_pointer( smob_netsnmp_request_info , requests);
  int result = scm_int_from_constant("<snmp-err-status>",
                 scm_call_4(
                   handfunc,
                   scmhandler,
                   scmreginfo,
                   scmreqinfo,
                   scmrequests
                 ));

  scm_remember_upto_here_1(scmhandler);
  scm_remember_upto_here_1(handfunc);
  scm_remember_upto_here_1(scmreginfo);
  scm_remember_upto_here_1(scmreqinfo);
  scm_remember_upto_here_1(scmrequests);
  return result;
}

void
guile_snmp_Netsnmp_Node_Handler_data_free(void* p)
{
  return;
}

static SCM
_wrap_netsnmp_create_handler(SCM s_0, SCM s_1)
{
  char* name = scm_to_latin1_string(s_0);
  netsnmp_mib_handler *p = netsnmp_create_handler(name, guile_snmp_Netsnmp_Node_Handler);
  p->myvoid = (void*) s_1;
  p->data_free = guile_snmp_Netsnmp_Node_Handler_data_free;
  SCM obj = make_wrapped_pointer( smob_netsnmp_mib_handler , p);
  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return obj;
};

SCM
_wrap_netsnmp_handler_registration_create(SCM s_0, SCM s_1, SCM s_2, SCM s_3)
{
  char* name = scm_to_latin1_string(s_0);
  netsnmp_mib_handler *handler = pointer_from_wrapped_smob(smob_netsnmp_mib_handler, s_1);
  size_t temp_oidlen = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(temp_oidlen * sizeof(oid));
  scm_to_oid(s_2,&temp_oid,&temp_oidlen);
  int mode = scm_int_from_constant("<mib-handler-cap>",s_3);

  netsnmp_handler_registration* p =
    netsnmp_handler_registration_create(name, handler, temp_oid, temp_oidlen, mode);
  SCM obj = make_wrapped_pointer( smob_netsnmp_handler_registration , p);

  free(temp_oid);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  scm_remember_upto_here_1(s_2);
  scm_remember_upto_here_1(s_3);
  return obj;
};

SCM
_wrap_netsnmp_register_handler(SCM s_0)
{
  netsnmp_handler_registration *p = pointer_from_wrapped_smob(smob_netsnmp_handler_registration, s_0);

  int res = 0;
  res = netsnmp_register_handler(p);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
};

SCM
_wrap_netsnmp_unregister_handler(SCM s_0)
{
  netsnmp_handler_registration *p = pointer_from_wrapped_smob(smob_netsnmp_handler_registration, s_0);

  int res = 0;
  res = netsnmp_unregister_handler(p);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
};

SCM
_wrap_netsnmp_register_scalar(SCM s_0)
{
  netsnmp_handler_registration *p = pointer_from_wrapped_smob(smob_netsnmp_handler_registration, s_0);

  int res = 0;
  res = netsnmp_register_scalar(p);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
};

SCM
_wrap_netsnmp_check_vb_type(SCM s_0, SCM s_1)
{
  netsnmp_variable_list *p = pointer_from_wrapped_smob(smob_pdu_variable, s_0);
  int type = scm_int_from_constant("<asn-type>",s_1);
  SCM result = SCM_UNSPECIFIED;

  result = scm_constant_name_from_int("<snmp-err-status>", netsnmp_check_vb_type(p, type));

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return result;
};

SCM
_wrap_netsnmp_set_request_error(SCM s_0, SCM s_1, SCM s_2)
{
  netsnmp_agent_request_info *reqinfo = pointer_from_wrapped_smob(smob_netsnmp_agent_request_info, s_0);
  netsnmp_request_info *requests = pointer_from_wrapped_smob(smob_netsnmp_request_info, s_1);
  int err = scm_int_from_constant("<asn-type>",s_2);
  netsnmp_set_request_error(reqinfo, requests, err );
  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  scm_remember_upto_here_1(s_2);
  return SCM_UNSPECIFIED;
};

SCM
_wrap_snmp_set_var_typed_value(SCM s_0, SCM s_1, SCM s_2)
{
  netsnmp_variable_list *p = pointer_from_wrapped_smob(smob_pdu_variable, s_0);
  int type = scm_int_from_constant("<asn-type>",s_1);
  void* pointer = NULL;
  size_t len = 0;

  scm_to_netsnmp_value_bytes(s_2, type, &pointer, &len);
  snmp_set_var_typed_value(p, type, pointer,len);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  scm_remember_upto_here_1(s_2);
  return SCM_UNSPECIFIED;
};

#ifdef HAVE_SYSORTABLE_H
SCM
_wrap_register_sysor_table(SCM s_0, SCM s_1)
{
  size_t temp_oidlen = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(temp_oidlen * sizeof(oid));
  scm_to_oid(s_0,&temp_oid,&temp_oidlen);
  char *descr = scm_to_latin1_string(s_1);

  REGISTER_SYSOR_TABLE(temp_oid, temp_oidlen, descr);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
};

SCM
_wrap_unregister_sysor_table(SCM s_0)
{
  size_t temp_oidlen = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(temp_oidlen * sizeof(oid));
  scm_to_oid(s_0,&temp_oid,&temp_oidlen);

  UNREGISTER_SYSOR_TABLE(temp_oid, temp_oidlen);

  scm_remember_upto_here_1(s_0);
  return SCM_UNSPECIFIED;
};
#endif

SCM
_wrap_netsnmp_table_helper_add_index(SCM s_0, SCM s_1)
{
  netsnmp_table_registration_info *p = pointer_from_wrapped_smob(smob_netsnmp_table_registration_info, s_0);
  int type = scm_int_from_constant("<asn-type>",s_1);

  netsnmp_table_helper_add_index(p, type)

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
};

SCM
_wrap_netsnmp_register_table_iterator(SCM s_0, SCM s_1)
{
  netsnmp_handler_registration *p1 = pointer_from_wrapped_smob(smob_netsnmp_handler_registration, s_0);
  netsnmp_iterator_info *p2 = pointer_from_wrapped_smob(smob_netsnmp_iterator_info, s_1);

  netsnmp_register_table_iterator(p1, p2);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);
  return SCM_UNSPECIFIED;
};

SCM
_wrap_netsnmp_extract_iterator_context(SCM s_0)
{
  netsnmp_request_info *request = pointer_from_wrapped_smob(smob_netsnmp_request_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , request )
  void* result = netsnmp_extract_iterator_context(request);

  scm_remember_upto_here_1(s_0);
  return result != NULL ? (SCM) result : SCM_BOOL_F;
};

SCM
_wrap_netsnmp_extract_table_info(SCM s_0)
{
  netsnmp_request_info *request = pointer_from_wrapped_smob(smob_netsnmp_request_info, s_0);
  ASSERT_NOT_NULL_PTR( s_0 , request )
  netsnmp_table_request_info *tinfo = netsnmp_extract_table_info(request);

  SCM obj = make_wrapped_pointer( smob_netsnmp_table_request_info , tinfo);

  scm_remember_upto_here_1(s_0);
  return obj;
};

static void
init_snmp_wrap_funcs(void)
{

  scm_c_define_gsubr("init-mib", 0, 0, 0, (void *) _wrap_init_mib);
  scm_c_export("init-mib" , NULL);

  scm_c_define_gsubr("init-snmp", 1, 0, 0, (void *) _wrap_init_snmp);
  scm_c_export("init-snmp" , NULL);

  scm_c_define_gsubr("read-module", 1, 0, 0, (void *) _wrap_read_module);
  scm_c_export("read-module" , NULL);

  scm_c_define_gsubr("which-module", 1, 0, 0, (void *) _wrap_which_module);
  scm_c_export("which-module" , NULL);

  scm_c_define_gsubr("snmp-set-save-descriptions", 1, 0, 0, (void *) _wrap_snmp_set_save_descriptions);
  scm_c_export("snmp-set-save-descriptions" , NULL);

  scm_c_define_gsubr("snmp-parse-oid", 1, 0, 0, (void *) _wrap_snmp_parse_oid);
  scm_c_export("snmp-parse-oid" , NULL);

  scm_c_define_gsubr("get-tree-head", 0, 0, 0, (void *) _wrap_get_tree_head);
  scm_c_export("get-tree-head" , NULL);

  scm_c_define_gsubr("get-tree", 2, 0, 0, (void *) _wrap_get_tree);
  scm_c_export("get-tree" , NULL);

  scm_c_define_gsubr("oid-from-tree-node", 1, 0, 0, (void *) _wrap_oid_from_tree_node);
  scm_c_export("oid-from-tree-node" , NULL);

  scm_c_define_gsubr("snmp-sess-init", 1, 0, 0, (void *) _wrap_snmp_sess_init);
  scm_c_export("snmp-sess-init" , NULL);

  scm_c_define_gsubr("snmp-sess-open", 1, 0, 0, (void *) _wrap_snmp_sess_open);
  scm_c_export("snmp-sess-open" , NULL);

  scm_c_define_gsubr("snmp-open", 1, 0, 0, (void *) _wrap_snmp_open);
  scm_c_export("snmp-open" , NULL);

  scm_c_define_gsubr("snmp-sess-session", 1, 0, 0, (void *) _wrap_snmp_sess_session);
  scm_c_export("snmp-sess-session" , NULL);

  scm_c_define_gsubr("snmp-sess-synch-response", 2, 0, 0, (void *) _wrap_snmp_sess_synch_response);
  scm_c_export("snmp-sess-synch-response" , NULL);

  scm_c_define_gsubr("snmp-synch-response", 2, 0, 0, (void *) _wrap_snmp_synch_response);
  scm_c_export("snmp-synch-response" , NULL);

  scm_c_define_gsubr("snmp-sess-error", 1, 0, 0, (void *) _wrap_snmp_sess_error);
  scm_c_export("snmp-sess-error" , NULL);

  scm_c_define_gsubr("snmp-sess-close", 1, 0, 0, (void *) _wrap_snmp_sess_close);
  scm_c_export("snmp-sess-close" , NULL);

  scm_c_define_gsubr("snmp-close", 1, 0, 0, (void *) _wrap_snmp_close);
  scm_c_export("snmp-close" , NULL);

  scm_c_define_gsubr("snmp-sess-send", 2, 0, 0, (void *) _wrap_snmp_sess_send);
  scm_c_export("snmp-sess-send" , NULL);

  scm_c_define_gsubr("snmp-send", 2, 0, 0, (void *) _wrap_snmp_send);
  scm_c_export("snmp-send" , NULL);

  scm_c_define_gsubr("snmp-sess-async-send", 3, 0, 0, (void *) _wrap_snmp_sess_async_send);
  scm_c_export("snmp-sess-async-send" , NULL);

  scm_c_define_gsubr("snmp-async-send", 3, 0, 0, (void *) _wrap_snmp_async_send);
  scm_c_export("snmp-async-send" , NULL);

  scm_c_define_gsubr("snmp-sess-read", 2, 0, 0, (void *) _wrap_snmp_sess_read);
  scm_c_export("snmp-sess-read" , NULL);

  scm_c_define_gsubr("snmp-read", 1, 0, 0, (void *) _wrap_snmp_read);
  scm_c_export("snmp-read" , NULL);

  scm_c_define_gsubr("snmp-pdu-create", 1, 0, 0, (void *) _wrap_snmp_pdu_create);
  scm_c_export("snmp-pdu-create" , NULL);

  scm_c_define_gsubr("snmp-add-null-var", 2, 0, 0, (void *) _wrap_snmp_add_null_var);
  scm_c_export("snmp-add-null-var" , NULL);

  scm_c_define_gsubr("snmp-add-var", 3, 0, 0, (void *) _wrap_snmp_add_var);
  scm_c_export("snmp-add-var" , NULL);

  scm_c_define_gsubr("netsnmp-tdomain-transport", 3, 0, 0, (void *) _wrap_netsnmp_tdomain_transport);
  scm_c_export("netsnmp-tdomain-transport" , NULL);

  scm_c_define_gsubr("snmp-free-pdu", 1, 0, 0, (void *) _wrap_snmp_free_pdu);
  scm_c_export("snmp-free-pdu" , NULL);

  scm_c_define_gsubr("netsnmp-oid-is-subtree", 2, 0, 0, (void *) _wrap_netsnmp_oid_is_subtree);
  scm_c_export("netsnmp-oid-is-subtree" , NULL);

  scm_c_define_gsubr("mib-to-asn-type", 1, 0, 0, (void *) _wrap_mib_to_asn_type);
  scm_c_export("mib-to-asn-type" , NULL);

  scm_c_define_gsubr("snmp-select-info", 0, 0, 0, (void *) _wrap_snmp_select_info);
  scm_c_export("snmp-select-info" , NULL);

  scm_c_define_gsubr("snmp-sess-select-info", 1, 0, 0, (void *) _wrap_snmp_sess_select_info);
  scm_c_export("snmp-sess-select-info" , NULL);

  scm_c_define_gsubr("snmp-select", 1, 0, 0, (void *) _wrap_snmp_select);
  scm_c_export("snmp-select" , NULL);

  scm_c_define_gsubr("snmp-timeout", 0, 0, 0, (void *) _wrap_snmp_timeout);
  scm_c_export("snmp-timeout" , NULL);

  scm_c_define_gsubr("snmp-sess-timeout", 1, 0, 0, (void *) _wrap_snmp_sess_timeout);
  scm_c_export("snmp-sess-timeout" , NULL);

  scm_c_define_gsubr("snmp-add", 2, 0, 0, (void *) _wrap_snmp_add);
  scm_c_export("snmp-add" , NULL);

  scm_c_define_gsubr("snmp-sess-add", 2, 0, 0, (void *) _wrap_snmp_sess_add);
  scm_c_export("snmp-sess-add" , NULL);

  scm_c_define_gsubr("init-agent", 1, 0, 0, (void *) _wrap_init_agent);
  scm_c_export("init-agent" , NULL);

  scm_c_define_gsubr("init-master-agent", 0, 0, 0, (void *) _wrap_init_master_agent);
  scm_c_export("init-master-agent" , NULL);

  scm_c_define_gsubr("snmp-set-agent-agentx", 1, 0, 0, (void *) _wrap_snmp_set_agent_agentx);
  scm_c_export("snmp-set-agent-agentx" , NULL);

  scm_c_define_gsubr("init-vacm-vars", 0, 0, 0, (void *) _wrap_init_vacm_vars);
  scm_c_export("init-vacm-vars" , NULL);

  scm_c_define_gsubr("init-usm", 0, 0, 0, (void *) _wrap_init_usm);
  scm_c_export("init-usm" , NULL);

  scm_c_define_gsubr("netsnmp-daemonize", 2, 0, 0, (void *) _wrap_netsnmp_daemonize);
  scm_c_export("netsnmp-daemonize" , NULL);

  scm_c_define_gsubr("agent-check-and-process", 1, 0, 0, (void *) _wrap_agent_check_and_processs);
  scm_c_export("agent-check-and-process" , NULL);

  scm_c_define_gsubr("snmp-shutdown", 1, 0, 0, (void *) _wrap_snmp_shutdown);
  scm_c_export("snmp-shutdown" , NULL);

  scm_c_define_gsubr("netsnmp-create-handler", 2, 0, 0, (void *) _wrap_netsnmp_create_handler);
  scm_c_export("netsnmp-create-handler" , NULL);

  scm_c_define_gsubr("netsnmp-handler-registration-create", 4, 0, 0, (void *) _wrap_netsnmp_handler_registration_create);
  scm_c_export("netsnmp-handler-registration-create" , NULL);

  scm_c_define_gsubr("netsnmp-register-handler", 1, 0, 0, (void *) _wrap_netsnmp_register_handler);
  scm_c_export("netsnmp-register-handler" , NULL);

  scm_c_define_gsubr("netsnmp-unregister-handler", 1, 0, 0, (void *) _wrap_netsnmp_unregister_handler);
  scm_c_export("netsnmp-unregister-handler" , NULL);

  scm_c_define_gsubr("netsnmp-register-scalar", 1, 0, 0, (void *) _wrap_netsnmp_register_scalar);
  scm_c_export("netsnmp-register-scalar" , NULL);

  scm_c_define_gsubr("netsnmp-check-vb-type", 2, 0, 0, (void *) _wrap_netsnmp_check_vb_type);
  scm_c_export("netsnmp-check-vb-type" , NULL);

  scm_c_define_gsubr("netsnmp-set-request-error", 3, 0, 0, (void *) _wrap_netsnmp_set_request_error);
  scm_c_export("netsnmp-set-request-error" , NULL);

  scm_c_define_gsubr("snmp-set-var-typed-value", 3, 0, 0, (void *) _wrap_snmp_set_var_typed_value);
  scm_c_export("snmp-set-var-typed-value" , NULL);

#ifdef HAVE_SYSORTABLE_H
  scm_c_define_gsubr("register-sysor-table", 2, 0, 0, (void *) _wrap_register_sysor_table);
  scm_c_export("register-sysor-table" , NULL);

  scm_c_define_gsubr("unregister-sysor-table", 1, 0, 0, (void *) _wrap_unregister_sysor_table);
  scm_c_export("unregister-sysor-table" , NULL);
#else
  scm_c_define("register-sysor-table", SCM_BOOL_F);
  scm_c_export("register-sysor-table" , NULL);

  scm_c_define("unregister-sysor-table", SCM_BOOL_F);
  scm_c_export("unregister-sysor-table", NULL);
#endif

  scm_c_define_gsubr("netsnmp-table-helper-add-index", 2, 0, 0, (void *) _wrap_netsnmp_table_helper_add_index);
  scm_c_export("netsnmp-table-helper-add-index" , NULL);

  scm_c_define_gsubr("netsnmp-register-table-iterator", 2, 0, 0, (void *) _wrap_netsnmp_register_table_iterator);
  scm_c_export("netsnmp-register-table-iterator" , NULL);

  scm_c_define_gsubr("netsnmp-extract-iterator-context", 1, 0, 0, (void *) _wrap_netsnmp_extract_iterator_context);
  scm_c_export("netsnmp-extract-iterator-context" , NULL);

  scm_c_define_gsubr("netsnmp-extract-table-info", 1, 0, 0, (void *) _wrap_netsnmp_extract_table_info);
  scm_c_export("netsnmp-extract-table-info" , NULL);
}

