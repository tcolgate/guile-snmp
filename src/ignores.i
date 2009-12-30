/*-------------------------------------------------------------------
 * Copyright (C) 2009,2010 Tristan Colgate 
 *
 * ignores.i - Force swig to ignore some net-snmp declarations that
 * cause problems during the build. Many of the patch files can be
 * handled here.
 *
 -------------------------------------------------------------------*/

%ignore snmp_get_fd_for_session;
