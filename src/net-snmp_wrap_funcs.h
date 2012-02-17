
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
  oid* temp_oid = (oid*)malloc(len * sizeof(oid));
  scm_to_oid(oidscm,&temp_oid,&len);

  struct tree* result = get_tree(temp_oid,len,node);
  scm_remember_upto_here_1(treehead);

  SCM scmresult = make_tree_smob_from_ptr(result);

  free(temp_oid);
  scm_remember_upto_here_1(oidscm);

  return scmresult;
}

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
_wrap_init_snmp (SCM name)
{
  init_snmp(scm_to_utf8_string (name));
  snmp_sess_init(&global_snmp_session);
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

static SCM
_wrap_snmp_sess_open (SCM s_0)
{
  struct snmp_session *session = (struct snmp_session*) pointer_from_wrapped_smob(smob_snmp_session, s_0);
  void *sessp = snmp_sess_open(session);
  return make_wrapped_pointer(smob_snmp_single_session ,(void*) sessp);
}

static SCM
_wrap_snmp_sess_session (SCM s_0)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  struct snmp_session *childsess = snmp_sess_session(sessp);
  return make_wrapped_pointer(smob_snmp_session ,(struct snmp_session*) childsess);
}

static SCM
_wrap_snmp_sess_close (SCM s_0)
{
  void *sessp = (void*) pointer_from_wrapped_smob(smob_snmp_single_session, s_0);
  snmp_sess_close(sessp);
  return SCM_UNSPECIFIED;
}

static SCM
_wrap_snmp_pdu_create (SCM s_0)
{
  return make_wrapped_pointer( smob_pdu , snmp_pdu_create( scm_int_from_constant("<snmp-msg>",s_0)));
}

static SCM
_wrap_snmp_add_null_var (SCM s_0, SCM s_1)
{
  netsnmp_pdu *pdu = (netsnmp_pdu*) pointer_from_wrapped_smob(smob_pdu, s_0);
  size_t len = MAX_OID_LEN;
  oid* temp_oid = (oid*)malloc(len * sizeof(oid));
  scm_to_oid(s_1,&temp_oid,&len);

  snmp_add_null_var(pdu,temp_oid,len);

  free(temp_oid);

  scm_remember_upto_here_1(s_0);
  scm_remember_upto_here_1(s_1);

  return SCM_UNSPECIFIED;
}
static void 

init_snmp_wrap_funcs(void)
{

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

  scm_c_define_gsubr("snmp-sess-open", 1, 0, 0, (void *) _wrap_snmp_sess_open);
  scm_c_export("snmp-sess-open" , NULL);

  scm_c_define_gsubr("snmp-sess-session", 1, 0, 0, (void *) _wrap_snmp_sess_session);
  scm_c_export("snmp-sess-session" , NULL);

  scm_c_define_gsubr("snmp-sess-close", 1, 0, 0, (void *) _wrap_snmp_sess_close);
  scm_c_export("snmp-sess-close" , NULL);

  scm_c_define_gsubr("snmp-pdu-create", 1, 0, 0, (void *) _wrap_snmp_pdu_create);
  scm_c_export("snmp-pdu-create" , NULL);

  scm_c_define_gsubr("snmp-add-null-var", 2, 0, 0, (void *) _wrap_snmp_add_null_var);
  scm_c_export("snmp-add-null-var" , NULL);
}

