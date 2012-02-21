#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ(2.63)
AC_INIT([guile-snmp],[0.2],[tcolgate@gmail.com])
#AC_CONFIG_MACRO_DIR([m4/])
AC_CONFIG_MACRO_DIR([m4])

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
# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

AC_CHECK_SIZEOF([oid],,[#include <net-snmp/net-snmp-config.h> 
#include <net-snmp/types.h>])

# Checks for library functions.

AC_CONFIG_FILES([Makefile doc/Makefile src/Makefile unit-tests/Makefile])
AC_CONFIG_FILES([test-environ], [chmod +x test-environ])
AC_CONFIG_FILES([build-environ], [chmod +x build-environ])
AC_OUTPUT

