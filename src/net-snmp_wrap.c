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

struct snmp_session global_snmp_session;

#include "net-snmp_wrap_oid.h"
#include "net-snmp_wrap_constants.h"
#include "net-snmp_wrap_structs.h"
#include "net-snmp_wrap_funcs.h"

static void init_snmp_wrap(void *data)
{

  init_oid_class();
  init_snmp_wrap_classes();
  init_snmp_wrap_constants();
  init_snmp_wrap_structs();
  init_snmp_wrap_funcs();
};

SCM
scm_init_snmp_net_snmp_module (void)
{
  scm_c_define_module("snmp net-snmp-primitive", init_snmp_wrap, NULL);
  return SCM_UNSPECIFIED;
}

#ifdef __cplusplus
}
#endif

