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
#include <limits.h>
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

%}

%init %{

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


   
%}


%include<ports.i>

#define NETSNMP_IMPORT extern

/* A typemap to pass read-only oids */

%typemap(in)(const oid* , size_t )(oid* temp_oid){
  scm_t_array_handle handle;
  size_t len,i;
  ssize_t inc;
  const SCM_T_OID* elt = 
    SCM_OIDVECTOR_ELEMENTS(
      scm_slot_ref($input, scm_oid_vec_slot),
      &handle, &len, &inc);
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
  (const oid* hashtype , u_int hashtype_len)
}

%apply(const oid* , size_t ){
  (const oid* name , size_t name_length)
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
  gswig_result = SCM_UNSPECIFIED;

  if(result){
    int i = 0;
    SCM newoid = SCM_TAKE_OIDVECTOR((SCM_T_OID*) $1, *$2);
    gswig_result = scm_apply(scm_goops_make,scm_list_3(scm_class_oid,scm_kw_value,newoid),SCM_EOL);
  } else {
    free($1);
  };

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

/* A typemap to pass data to snmp_pdu_add_variable for
  typesafe snmp set */

%typemap(in)( u_char, const void *, size_t ) (scm_t_array_handle handle){
  u_char typespec; SCM valscm;
  void* pointer = NULL; 
  size_t len = 0;
  size_t iter = 0;

  // Temporary storage
  in_addr_t       atmp;
  long            ltmp;
  u_long          utmp;
  int             itmp;
  oid*            oidtmp;
#ifdef NETSNMP_WITH_OPAQUE_SPECIAL_TYPES
  double          dtmp;
  float           ftmp;
#endif                          /* NETSNMP_WITH_OPAQUE_SPECIAL_TYPES */
  struct counter64 c64tmp;

  if (! SCM_CONSP($input) ){
    // signal an error
    scm_throw(
      scm_string_to_symbol(
        scm_from_locale_string("snmperror")),
      scm_from_locale_string("Malformed data passed to set"));
  };
  
  typespec = SCM_CHAR(SCM_CAR($input));
  valscm = SCM_CDR($input);

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
        pointer = (void*) scm_to_locale_stringn(valscm, &len);
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
      { // signal an error
        scm_throw(
          scm_string_to_symbol(scm_from_locale_string("snmperror")),
          scm_string_append(
            scm_list_3(
              scm_from_locale_string("Unhandled type("),
              scm_number_to_string( scm_char_to_integer (SCM_CAR($input)),SCM_UNDEFINED),
              scm_from_locale_string(") in set data"))));
     };
  };

  $1 = typespec;
  $2 = (void*) pointer;
  $3 = len;
}

%typemap(freearg)( u_char, const void *, size_t ) {
  scm_array_handle_release(&handle$argnum);
}
%apply( u_char, const void *, size_t ){
  (u_char type , const void * value , size_t len )
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


/* Typemap for returning error strings. */


%typemap(in,numinputs=0)(int *, int *, char **)(int* pint1, int* pint2, char **pcharp){
  // allocate a new oid( of maximum length)
  $1 = (int*)malloc(sizeof(int));
  $2 = (int*)malloc(sizeof(int));
  $3 = (char**)malloc(sizeof(char*));
  pint1 = $1;
  pint2 = $2;
  pcharp = $3;
}

%typemap(argout)(int *, int *, char **){
  SCM scmout = SCM_UNSPECIFIED;

  if($3){
    scmout = scm_from_locale_string(*$3);
  };

  gswig_result = scmout;
}

%typemap(freearg)(int *, int *, char ** ) {
  free(pint1$argnum);
  free(pint2$argnum);
  free(pcharp$argnum);
}


%extend variable_list {
       const SCM value;
}

// Specific implementation of set/get functions
%{
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
        result = SCM_TAKE_OIDVECTOR((SCM_T_OID *)temp, (p->val_len)/sizeof(oid));
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
%}

%inline %{
int
oid_from_varbind(struct variable_list* varbind, oid* objid, size_t* objidlen){
  memcpy( objid, varbind->name, (varbind->name_length * sizeof(oid)));
  *objidlen =  varbind->name_length;
  return 1 ;
};
%}

%inline %{
int
oid_from_tree_node(struct tree *tree_node, oid* objid, size_t* objidlen) {
  struct tree *currnode = tree_node;
  struct tree *head = get_tree_head();
  int currid = 0;

  *objidlen = 0;
  if(tree_node->parent == NULL) return 1;

  while(currnode != head)
  {
    *objidlen += 1;
    currnode = currnode->parent;
  };
  *objidlen += 1;

  currnode = tree_node;
  objid[0] = head->subid;
  while(currnode != head)
  {
    objid[*objidlen - currid - 1] = currnode->subid;
    currid++;
    currnode = currnode->parent;
  };
  return 1;
};
%}

# Callback Support

%extend snmp_session {
       const SCM value;
}

%inline %{
int guile_snmp_async_response(int op, struct snmp_session *sp, int reqid,
                     struct snmp_pdu *pdu, void *magic){
  return 1;
};
%}

// Specific implementation of set/get functions
%{

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

%}


# Parse Headers

%include "constants.i"
%include "renames.i"
%include "ignores.i"

%include "net-snmp/net-snmp-config.h"
%include "net-snmp/library/snmp.h"
%include "net-snmp/library/default_store.h"
%include "net-snmp/library/parse.h"
%include "net-snmp/library/asn1.h"
%include "net-snmp/library/keytools.h"
%include "net-snmp/types.h"
%include "net-snmp/session_api.h"
%include "net-snmp/varbind_api.h"
%include "net-snmp/mib_api.h"
%include "net-snmp/pdu_api.h"
#%include "net-snmp/agent/snmp_agent.h"
#%include "net-snmp/agent/snmp_vars.h"

# we use the local patched version of these
%include "snmp_api.h"
%include "snmp_client.h"
%include "mib.h"

%goops %{ 

(eval-when (eval load compile)

  (use-modules (oop goops))
  (use-modules (srfi srfi-39))
  
  ; The module will hook these up with architecture specific
  ; srfi-4 routines
  (define empty-oidvec #f)
  (define oidvector #f)
  (define make-oidvector #f)
  (define oidvector? #f)
  (define oidvector-length #f)
  (define list->oidvector #f)
  (define oidvector->list #f)
  (define oidvector-ref #f)
  (define oidvector-set! #f)

  (define-class <oid> ()
    (_vec #:init-value empty-oidvec
          #:init-keyword #:value))

  (define oid-translate (make-parameter #t))

  (define-method (display (this <oid>) port)
    (if (oid-translate)
      (let* ((node     (get-tree this (get-tree-head)))
             (basename (slot-ref node 'label))
             (diff     (- (oid-from-tree-node node) this)))
        (format port "~a~{.~d~}" basename (oid->list diff)))
      (format port "~{.~d~}" (oid->list this))))

  (define-method (write (this <oid>) port)
    (if (oid-translate)
      (let* ((node     (get-tree this (get-tree-head)))
             (basename (slot-ref node 'label))
             (diff     (- (oid-from-tree-node node) this)))
        (format port "#<oid: ~a~{.~d~}>#" basename (oid->list  diff)))
      (format port "#<oid: ~{.~d~}>#" (oid->list this))))

  (define-method (equal? (a <oid>) (b <oid>))
    (equal? (slot-ref a '_vec) (slot-ref  b '_vec)))

  (define-method (equal? (a <oid>) b)
    (equal? (slot-ref a '_vec) b))

  (define-method (equal? (a <oid>) b)
    (equal? (slot-ref a '_vec) b))

  (define-method (equal? a (b <oid>))
    (equal? a (slot-ref  b '_vec)))

  (define-method (oid->list (this <oid>))
    (oidvector->list (slot-ref  this '_vec)))

  (define-method (list->oid this)
    (make <oid> #:value (list->oidvector this)))

  (export 
    <oid> 
    oid-translate
    oidvector 
    make-oidvector 
    oidvector? 
    oidvector-length
    list->oidvector 
    oidvector->list
    oidvector-ref
    oidvector-set!
    list->oid 
    oid->list 
    empty-oidvec)
  
  (load-extension "libguile_snmp_net-snmp" "scm_init_snmp_net_snmp_module"))
%}


