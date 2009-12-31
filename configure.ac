#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

#AC_PREREQ(2.63)
AC_INIT([guile-snmp],[0.1],[tcolgate@gmail.com])
#AC_CONFIG_MACRO_DIR([m4/])
AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_SWIG
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER(config.h)
GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

if $GUILE_TOOLS | grep -q compile; then
   GUILE_COMPILE="$GUILE_TOOLS compile"
   HAVE_GUILE_COMPILE=1
else
   GUILE_COMPILE=""
   HAVE_GUILE_COMPILE=0
fi

AC_SUBST(GUILE_COMPILE)
AM_CONDITIONAL(HAVE_GUILE_COMPILE, test "$HAVE_GUILE_COMPILE" = 1)

AC_CHECK_LIB([snmp], [init_snmp])
AC_CHECK_LIB([netsnmp], [init_snmp])
# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile src/Makefile unit-tests/Makefile unit-tests/test-agent/Makefile])
AC_OUTPUT

