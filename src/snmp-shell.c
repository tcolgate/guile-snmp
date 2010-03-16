/* gaul.c
 * 
 *	Copyright (C) 1998, 2006 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 */

#include <libguile.h>

static void
snmp_shell_module (void* data)
{
  scm_c_use_module("ice-9 threads");
  scm_c_use_module("ice-9 getopt-long");
  scm_c_use_module("ice-9 common-list");
  scm_c_use_module("srfi srfi-39");

  scm_c_use_module("ice-9 session");
  scm_c_use_module("ice-9 history");

  scm_c_use_module("snmp reports");

  scm_c_eval_string("(init-reports)");

  scm_c_define("program-name", 
    scm_c_eval_string("(make-parameter (basename (car (command-line))))"));
  scm_c_define("program-version", 
    scm_c_eval_string ("(make-parameter  \"1\")"));
  scm_c_define("program-help", 
    scm_c_eval_string("(make-parameter (string-append (program-name) \": version \" (program-version) \"\n\"))"));
  scm_c_define("script-arguments", 
    scm_c_eval_string ("(make-parameter #f)"));

  putenv(
   scm_to_locale_string(
     scm_c_eval_string("(string-append \"GUILE_HISTORY=\" (getenv \"HOME\") \"/.\" (program-name) \"_history\")")));

  scm_c_use_module("ice-9 readline");
  scm_c_eval_string("(activate-readline)");

  scm_c_eval_string("(set-repl-prompt! (string-append (program-name) \"> \"))");
  scm_c_eval_string("(scm-style-repl)");
  return;
}

static void
inner_main (void *closure, int argc, char **argv)
{
  /* module initializations would go here */
 // init_image_type();
  
  scm_c_define_module(basename(argv[0]), snmp_shell_module, (void*)NULL);
  return;
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
