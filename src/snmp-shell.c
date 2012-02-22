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
#include <getopt.h>
#include <libgen.h>

#include "config.h"

void
display_help()
{
  exit(0);
};

int help_flag = 0;
char* opt_defversion = "2c";
char* opt_defcommunity = "public";
char* opt_defcontext = "";
char* opt_defhost = "localhost";
char* opt_script = NULL;
char* opt_eval = NULL;

static void
snmp_shell_module (void* arguments)
{
  scm_c_use_module("oop goops");
  scm_c_eval_string("(set-module-duplicates-handlers! (current-module) (list (module-ref duplicate-handlers 'merge-generics)))");
  scm_c_use_module("ice-9 threads");
  scm_c_use_module("ice-9 getopt-long");
  scm_c_use_module("ice-9 common-list");
  scm_c_use_module("srfi srfi-1");
  scm_c_use_module("srfi srfi-39");

  scm_c_use_module("ice-9 session");
  scm_c_use_module("ice-9 history");

  scm_c_use_module("system repl common");
  scm_c_use_module("system repl repl");

  scm_c_define("program-name", 
    scm_c_eval_string("(make-parameter  (symbol->string (car (module-name (current-module)))))"));
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

  scm_c_use_module("snmp net-snmp");
  scm_c_use_module("snmp reports");
  scm_c_use_module("snmp reports session");

  scm_c_eval_string("(init-reports)");

  if(0 != help_flag){
    display_help();
  };

  if(NULL != opt_defcommunity){
    scm_apply_1(
      scm_variable_ref(scm_c_lookup("current-community")),
      scm_from_locale_string(opt_defcommunity),
      SCM_EOL);
  };

  if(NULL != opt_defcontext){
    scm_apply_1(
      scm_variable_ref(scm_c_lookup("current-context")),
      scm_from_locale_string(opt_defcontext),
      SCM_EOL);
  };

  if(NULL != opt_defhost){
    scm_apply_1(
      scm_variable_ref(scm_c_lookup("current-host")),
      scm_from_locale_string(opt_defhost),
      SCM_EOL);
  };

  if(NULL != opt_defversion){
    SCM val;
    if(!strcmp(opt_defversion, "1")){
      val = scm_variable_ref(scm_c_lookup("SNMP-VERSION-1"));
    } else if (!strcmp(opt_defversion, "2c")){ 
      val = scm_variable_ref(scm_c_lookup("SNMP-VERSION-2c"));
    } else if (!strcmp(opt_defversion, "3")){
      val = scm_variable_ref(scm_c_lookup("SNMP-VERSION-3"));
    } else {
      val = scm_variable_ref(scm_c_lookup("SNMP-VERSION-2c"));
    };
    
    scm_apply_1(
      scm_variable_ref(scm_c_lookup("current-version")), val, SCM_EOL);
  };

  scm_c_eval_string("(current-session (new-snmp-session))");
  
  scm_apply_1(scm_variable_ref(scm_c_lookup("script-arguments")), (SCM) arguments, SCM_EOL);
  
  SCM version_sym = scm_c_lookup("*version*");
  SCM versionstring = scm_from_locale_string( PACKAGE_STRING );
  scm_variable_set_x(version_sym, versionstring);

  scm_c_eval_string("(repl-default-prompt-set! (string-append (program-name) \"> \"))");

  if(NULL != opt_script){
    scm_c_primitive_load(opt_script);
  };

  if(NULL != opt_eval){
    scm_simple_format(
      scm_current_output_port(),
      scm_from_locale_string("~A~%"),
      scm_list_1(scm_c_eval_string(opt_eval)));
  };

  if(NULL == opt_script && NULL == opt_eval){
    //scm_c_eval_string("(start-repl #:welcome #f)");
    scm_c_eval_string("(start-repl 'scheme)");
  };

  return;
}

static void
inner_main (void *closure, int argc, char **argv)
{
  SCM scriptargs = SCM_EOL;
  int c;
  while(1){
    int option_index = 0;
    static struct option long_options[] = {
      {"version"      ,no_argument       ,&help_flag ,  1},
      {"help"         ,no_argument       ,&help_flag ,  1},
      {"snmp-version" ,required_argument ,0             ,'v'},
      {"host"         ,required_argument ,0             ,'h'},
      {"community"    ,required_argument ,0             ,'c'},
      {"context"      ,required_argument ,0             ,'C'},
      {"eval"         ,required_argument ,0             ,'e'},
      {"script"       ,required_argument ,0             ,'s'},
      {0, 0, 0, 0}
    }; 
    c = getopt_long (argc,argv,"VHv:h:c:C:e:s:",long_options, &option_index);

    if (c == -1) break;
    
    switch(c){
      case   0:
        break;

      case 'V':
      case 'H':
        help_flag = 1;
        break;

      case 'v':
        opt_defversion = optarg; 
        break;

      case 'h':
        opt_defhost = optarg;
        break;

      case 'c':
        opt_defcommunity = optarg;
        break;

      case 'C':
        opt_defcontext = optarg;
        break;

      case 'e':
        opt_eval = optarg;
        break;

      case 's':
        opt_script = optarg;
        break;

      default: 
        abort();
    };
  };
  
  if (optind < argc){
    while (optind < argc){
      scriptargs = scm_append(
       scm_list_2(
         scriptargs,
         scm_list_1(scm_from_locale_string(argv[optind]))));
      optind++;
    };
  };

  if(NULL == opt_script){
    scm_c_define_module(basename(argv[0]), snmp_shell_module, (void*)scriptargs);
  } else {
    scm_c_define_module(basename(opt_script), snmp_shell_module, (void*)scriptargs);
  }
  return;
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
