/*
 * -------------------------------------------------------------------
 *  Copyright (C) 2009-2012 Tristan Colgate
 *
 *  snmp-shell.c
 * -------------------------------------------------------------------
 */

#include <libguile.h>
#include <getopt.h>
#include <libgen.h>

#include "config.h"

static void
print_help(void)
{
  printf( "Usage: snmp-shell [OPTION] ... [FILE]\n" 
          "A Guile Scheme based SNMP reporting environment\n"
          "Part of %s\n"
          "Mandatory arguments to long options are mandatory for short options too.\n"
          "\n"
          "  -H, --help                             Print this help message\n"
          "  -V, --version                          Print the version of snmp-shell\n"
          "  -e, --eval EXP                         Evaluate this expression and exit\n"
          "  -s, --script FILE                      Run this file as a script and exit\n"
          " \n"
          "  If no script or expression are given, and interactive shell is run\n"
          " \n"
          "Default SNMP session prameters\n" 
          "  -h, --host HOSTNAME[:PORT]             Set the defuult session host nnd udp port\n"
          "                                         Default: localhost:161\n"
          "  -v, --snmp-version [2c|1]              Set the default SNMP version\n"
          "                                         Default: 2c\n"
          "  -c, --community STRING                 Sst the default community string\n"
          "                                         Default: public\n"
          "  -C, --context STRING                   Set the default v2c/v3 context\n"
          "                                         Default: (none)\n"
          "Default MIB parsing options\n" 
          " \n"
          "  -D, --mib-descriptions                 Load and keep MIB textual descriptions when\n"
          "                                         reading MIBs. This can use large amounts of memory\n"
          , PACKAGE_STRING);
};

static void
print_version(void)
{
  printf( "snmp-shell from %s\n", PACKAGE_STRING);
};

int opt_help = 0;
int opt_version = 0;
char* opt_defversion = "2c";
char* opt_defcommunity = "public";
char* opt_defcontext = "";
char* opt_defhost = "localhost";
char* opt_script = NULL;
char* opt_eval = NULL;
int opt_mibdesc = 0;

static void
snmp_shell_module (void* arguments)
{
  scm_c_use_module("oop goops");
  scm_c_eval_string("(set-module-duplicates-handlers! "
                      "(current-module)"
                      "(list (module-ref duplicate-handlers 'merge-generics)))");
  scm_c_use_module("ice-9 format");
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
  scm_c_use_module("snmp oids");
  scm_c_use_module("snmp reports");
  scm_c_use_module("snmp reports cache");
  scm_c_use_module("snmp reports session");

  scm_c_eval_string("(init-reports)");

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
      {"help"                   ,no_argument       ,&opt_help     ,1},
      {"version"                ,no_argument       ,&opt_version  ,1},
      {"eval"                   ,required_argument ,0             ,'e'},
      {"script"                 ,required_argument ,0             ,'s'},
      {"host"                   ,required_argument ,0             ,'h'},
      {"snmp-version"           ,required_argument ,0             ,'v'},
      {"community"              ,required_argument ,0             ,'c'},
      {"context"                ,required_argument ,0             ,'C'},
      {"mib-descriptions"       ,no_argument       ,&opt_mibdesc  ,1},
      {0, 0, 0, 0}
    }; 
    c = getopt_long (argc,argv,"VHv:h:c:C:e:s:",long_options, &option_index);

    if (c == -1) break;
    
    switch(c){
      case   0:
        break;

      case 'V':
      case 'H':
	opt_help=1;
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

      case 'D':
	opt_mibdesc=1;
        break;

      default: 
        abort();
    };
  };
 
  if (opt_help){
    print_help();
    exit(0);
  };

  if (opt_version){ 
    print_version();
    exit(0);
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
