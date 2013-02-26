#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ(2.63)
AC_INIT([Guile SNMP],[0.2.3],[tcolgate <at> gmail.com],[guile-snmp],[http://github.com/tcolgate/guile-snmp])
AC_CONFIG_MACRO_DIR([m4])
AC_SUBST(PACKAGE_TARNAME)
AC_SUBST(PACKAGE)
AC_SUBST(PACKAGE_URL)
AC_SUBST(PACKAGE_BUGREPORT)

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER(src/config.h)
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

GUILE_PREFIX=`$GUILE_CONFIG info prefix`
AC_SUBST(GUILE_PREFIX)


if $GUILE_TOOLS | grep -q compile; then
   GUILE_COMPILE="$GUILE_TOOLS compile"
   HAVE_GUILE_COMPILE=1
else
   GUILE_COMPILE=""
   HAVE_GUILE_COMPILE=0
fi

AC_SUBST(GUILE_COMPILE)
AM_CONDITIONAL(HAVE_GUILE_COMPILE, test "$HAVE_GUILE_COMPILE" = 1)
AM_PROG_CC_C_O

CHECK_NETSNMP_CONFIG
NETSNMP_FLAGS

AC_CHECK_LIB([snmp], [init_snmp])
AC_CHECK_LIB([netsnmp], [init_snmp])
if test "x$ac_cv_lib_snmp_init_snmp" != "xyes" || test "x$ac_cv_lib_netsnmp_init_snmp" != "xyes"; then
  AC_MSG_ERROR([net-snmp libraries are not found.])
fi

# Checks for header files.

AC_CHECK_HEADER([net-snmp/net-snmp-config.h])
AC_CHECK_HEADER([net-snmp/net-snmp-includes.h],
		,
		[AC_MSG_ERROR([net-snmp headers are not found.])],
		[
#include <net-snmp/net-snmp-config.h>
])

AC_CHECK_HEADER([net-snmp/agent/net-snmp-agent-includes.h],
		[AC_DEFINE([HAVE_AGENT_H],[1],[Found net-snmp agent headers])],
		,
		[
#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-includes.h>
])

AC_CHECK_HEADER([net-snmp/agent/sysORTable.h],
		[AC_DEFINE([HAVE_SYSORTABLE_H],[1],[Found sysORTable headers])],
		,
		[
#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-includes.h>
#include <net-snmp/agent/net-snmp-agent-includes.h>
])

# Checks for typedefs, structures, and compiler characteristics.

AC_CHECK_SIZEOF([oid],,[#include <net-snmp/net-snmp-config.h> 
#include <net-snmp/types.h>])

# Checks for library functions.

AC_CONFIG_FILES([Makefile doc/Makefile src/Makefile unit-tests/Makefile meta/Makefile])
AC_CONFIG_FILES([test-environ], [chmod +x test-environ])
AC_CONFIG_FILES([build-environ], [chmod +x build-environ])
AC_CONFIG_FILES([meta/snmp-shell], [chmod +x meta/snmp-shell])
AC_CONFIG_FILES([guile-snmp.spec])

AC_OUTPUT

