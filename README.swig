Some patches are currently needed for swig to work with
guile-2.0.

I use the following patch to the swig source...

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
@@ -66,16 +66,9 @@
   char *ret;
   size_t l;
 
-  SCM_ASSERT (SCM_STRINGP(str), str, 1, FUNC_NAME);
-  
-  l = SCM_STRING_LENGTH(str);
-  ret = (char *) SWIG_malloc( (l + 1) * sizeof(char));
-  if (!ret) return NULL;
+  SCM_ASSERT (scm_string_p(str), str, 1, FUNC_NAME);
 
-  memcpy(ret, SCM_STRING_CHARS(str), l);
-  ret[l] = '\0';
-  if (len) *len = l;
-  return ret;
+  return scm_to_locale_string( str );
 #undef FUNC_NAME
 }
 

