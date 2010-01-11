Some patches are currently needed for swig to work with
guile-2.0.

I use the following patch to the swig source...

Index: configure.in
===================================================================
--- configure.in	(revision 11725)
+++ configure.in	(working copy)
@@ -1155,7 +1155,7 @@
 guilesafe_LIBS=$LIBS
 # Filter out "-ansi -pedantic" because Guile header files will not compile with these flags.
 # (The flags -ansi -pedantic are automatically added by ac_compile_warnings.m4)
-CFLAGS="`echo $CFLAGS | sed 's/-ansi//g;s/-pedantic//g;'` $GUILEINCLUDE"
+CFLAGS="`echo $CFLAGS | sed 's/-ansi//g;s/-pedantic//g;'` -L$GUILEINCLUDE"
 LIBS="$LIBS $GUILELINK"
 
 AC_MSG_CHECKING(whether Guile's gh_ API works)
@@ -1168,7 +1168,7 @@
 fi
 AC_MSG_CHECKING(whether Guile's SCM_ API works)
 AC_LINK_IFELSE([#include <libguile.h>
-  int main() { SCM s; scm_slot_exists_p(SCM_BOOL_F, SCM_BOOL_F); return SCM_STRING_LENGTH(s); }], GUILE_SCM_INTERFACE=1, )
+  int main() { SCM s; scm_slot_exists_p(SCM_BOOL_F, SCM_BOOL_F); return scm_c_string_length(s); }], GUILE_SCM_INTERFACE=1, )
 if test -n "$GUILE_SCM_INTERFACE" ; then
         AC_MSG_RESULT(yes)
 else
Index: Lib/guile/guile_scm_run.swg
===================================================================
--- Lib/guile/guile_scm_run.swg	(revision 11725)
+++ Lib/guile/guile_scm_run.swg	(working copy)
@@ -26,9 +26,9 @@
 #define SWIG_scm2str(s) \
   SWIG_Guile_scm2newstr(s, NULL)
 #define SWIG_malloc(size) \
-  SCM_MUST_MALLOC(size)
+  scm_malloc(size)
 #define SWIG_free(mem) \
-  scm_must_free(mem)
+  free(mem)
 #define SWIG_ConvertPtr(s, result, type, flags) \
   SWIG_Guile_ConvertPtr(s, result, type, flags)
 #define SWIG_MustGetPtr(s, type, argnum, flags) \
@@ -66,15 +66,12 @@
   char *ret;
   size_t l;
 
-  SCM_ASSERT (SCM_STRINGP(str), str, 1, FUNC_NAME);
+  SCM_ASSERT (scm_string_p(str), str, 1, FUNC_NAME);
   
-  l = SCM_STRING_LENGTH(str);
-  ret = (char *) SWIG_malloc( (l + 1) * sizeof(char));
-  if (!ret) return NULL;
-
-  memcpy(ret, SCM_STRING_CHARS(str), l);
-  ret[l] = '\0';
+  ret = scm_to_locale_stringn(str, &l);
+  ret[l] = (char) NULL ;
   if (len) *len = l;
+
   return ret;
 #undef FUNC_NAME
 }

