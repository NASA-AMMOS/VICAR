#ifndef ERRORS_INCLUDE
#   define ERRORS_INCLUDE

#define ENV_VARNAME "EPRINTF"	/* Environment variable name must be in
				   quotes */

#ifdef __STDC__
#define prm(x) x
#else
#define prm(x) ()
#endif


#ifdef __cplusplus
    extern "C" {
#endif

/* Eprintf is an ugly hack.  So to make it work with SGI's CC compiler
   we have to ensure that only one extern "C" declaration is used.  If
   the declaration doesn't match the definition EXACTLY, the definition
   is treated as an overloaded function and gets its name mangled.  So
   We declare it with "..." for every file BUT the errors.c source file.
*/
#ifdef ERRORS_C_SOURCE
/*VARARGS1*/
int eprintf (const char *cntl, long int a, long int b, long int c, long int d,
	     long int e, long int f, long int g, long int h, long int i,
	     long int j, long int k, long int l);
#else
int eprintf prm((const char *fmt, ...));
#endif
void einit prm((void));

#ifdef __cplusplus
}
#endif

#undef prm
#endif
