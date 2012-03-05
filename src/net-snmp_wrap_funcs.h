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
  //snmp_set_save_descriptions(1);
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

  SCM scmresult = make_tree_smob_from_ptr(result);

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
_wrap_snmp_parse_oid (SCM oidname)
{
	
   size_t *oidlen = (size_t*)scm_calloc(sizeof(size_t));
   *oidlen=MAX_OID_LEN;
   oid *oidstore = (oid*)scm_calloc(*oidlen * sizeof(oid));

   oid *result = snmp_parse_oid(scm_to_locale_string(oidname), oidstore, oidlen);
  
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
_wrap_snmp_add_var (SCM s_0, SCM s_1, SCM s_2)
{
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);

  size_t oidlen = MAX_OID_LEN;
  oid* temp_oid = (oid*)scm_calloc(oidlen * sizeof(oid));
  scm_to_oid(s_1,&temp_oid,&oidlen);

  scm_t_array_handle handle;
  u_char typespec; 
  SCM valscm;
  void* pointer = NULL; 
  size_t len = 0;
  size_t iter = 0;
  
  // Temporary storage
  in_addr_t       atmp;
  long            ltmp;
  u_long          utmp;
  int             itmp;
  oid*            oidtmp;
  struct counter64 c64tmp;
  
  if (! SCM_CONSP(s_2) ){
    // signal an error
    scm_throw(
      scm_string_to_symbol(
        scm_from_locale_string("snmperror")),
      scm_from_locale_string("Malformed data passed to set"));
  };
  
  typespec = scm_int_from_constant("<asn-type>",SCM_CAR(s_2));
  valscm = SCM_CDR(s_2);
  
  switch (typespec){
  case ASN_INTEGER:
    {
      if ( ! scm_is_signed_integer(valscm, LONG_MIN, LONG_MAX) ){
        scm_throw(
          scm_string_to_symbol(
            scm_from_locale_string("snmperror")),
          scm_from_locale_string("Data is not a signed integer"));
      };
      ltmp = scm_to_long(valscm);
      pointer = &ltmp;
      len = sizeof(long);
    };
    break;
    
  case ASN_UINTEGER:
  case ASN_GAUGE:
  case ASN_COUNTER:
  case ASN_TIMETICKS:
    {
      if ( ! scm_is_unsigned_integer(valscm, LONG_MIN, LONG_MAX) ){
        scm_throw(
          scm_string_to_symbol(
            scm_from_locale_string("snmperror")),
          scm_from_locale_string("Data is not an unsuigned integer"));
      };
      utmp = scm_to_ulong(valscm);
      pointer = (void*) &utmp;
      len = sizeof(u_long);
    };
    break;
    
  case ASN_IPADDRESS:
  case ASN_OCTET_STR: 
  case ASN_OPAQUE:
  case ASN_NSAP:
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
    
  case ASN_OBJECT_ID:
    {
      if ( ! scm_is_true(SCM_OIDVECTOR_P (valscm) )){
        scm_throw(
          scm_string_to_symbol(
            scm_from_locale_string("snmperror")),
          scm_from_locale_string("Data is not an oid"));
      };
      pointer = (void*) SCM_OIDVECTOR_ELEMENTS(valscm, &handle, &len, &iter);
    };
    break;
    
  case ASN_BIT_STR:
  case ASN_COUNTER64:
  default:
    {
      // signal an error
      scm_throw(
        scm_string_to_symbol(scm_from_locale_string("snmperror")),
        scm_string_append(
          scm_list_3(
            scm_from_locale_string("Unhandled type("),
            scm_number_to_string( scm_char_to_integer (SCM_CAR(s_2)),SCM_UNDEFINED),
            scm_from_locale_string(") in set data"))));
    };
  };
  
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


static void 
init_snmp_wrap_funcs(void)
{

  scm_c_define_gsubr("guile-snmp-async-response", 5, 0, 0, (void *) _wrap_guile_snmp_async_response);

  scm_c_define_gsubr("init-mib", 0, 0, 0, (void *) _wrap_init_mib);
  scm_c_export("init-mib" , NULL);

  scm_c_define_gsubr("read-module", 1, 0, 0, (void *) _wrap_read_module);
  scm_c_export("read-module" , NULL);

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

  scm_c_define_gsubr("snmp-send", 1, 0, 0, (void *) _wrap_snmp_send);
  scm_c_export("snmp-send" , NULL);

  scm_c_define_gsubr("snmp-pdu-create", 1, 0, 0, (void *) _wrap_snmp_pdu_create);
  scm_c_export("snmp-pdu-create" , NULL);

  scm_c_define_gsubr("snmp-add-null-var", 2, 0, 0, (void *) _wrap_snmp_add_null_var);
  scm_c_export("snmp-add-null-var" , NULL);

  scm_c_define_gsubr("snmp-add-var", 3, 0, 0, (void *) _wrap_snmp_add_var);
  scm_c_export("snmp-add-var" , NULL);

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
}

