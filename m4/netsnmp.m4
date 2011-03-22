AC_DEFUN([CHECK_NETSNMP_CONFIG],
 [AC_PATH_PROG(NETSNMP_CONFIG,net-snmp-config)
  AC_MSG_CHECKING([net-snmp-config])
  if test "$NETSNMP_CONFIG" = "" ; then
      AC_MSG_ERROR([net-snmp-config required but not found])
  fi
  AC_MSG_RESULT([$NETSNMP_CONFIG])
  AC_SUBST(NETSNMP_CONFIG)
 ])

AC_DEFUN([NETSNMP_FLAGS],
 [AC_REQUIRE([CHECK_NETSNMP_CONFIG])dnl
  AC_MSG_CHECKING([net-snmp prefix])
  NETSNMP_PREFIX="`$NETSNMP_CONFIG --prefix`"
  AC_MSG_RESULT([$NETSNMP_PREFIX])
  AC_MSG_CHECKING([net-snmp compile flags])
  NETSNMP_CFLAGS="`$NETSNMP_CONFIG --cflags`"
  AC_MSG_RESULT([$NETSNMP_CFLAGS])
  AC_MSG_CHECKING([net-snmp link flags])
  NETSNMP_LDFLAGS="`$NETSNMP_CONFIG --libs`"
  AC_MSG_RESULT([$NETSNMP_LDFLAGS])
  AC_MSG_CHECKING([net-snmp link flags])
  NETSNMPAGENT_LDFLAGS="`$NETSNMP_CONFIG --agent-libs`"
  AC_MSG_RESULT([$NETSNMPAGENT_LDFLAGS])
  AC_SUBST(NETSNMP_PREFIX)
  AC_SUBST(NETSNMP_CFLAGS)
  AC_SUBST(NETSNMP_LDFLAGS)
  AC_SUBST(NETSNMPAGENT_LDFLAGS)
 ])

