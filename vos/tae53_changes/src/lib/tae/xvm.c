/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/*
 *	TAE version 4 XVM inner bridges for manipulation of 
 *	dynamic memory variables (XVM Package)
 *
 *	CHANGE LOG:
 *
 *	6/22/89 Initial Cut...rsg/AS
 *	26-feb-90	Added support xvm_copy, xvm_find, xvm_findvar...krw
 *	21-aug-91	did xvm_set_valid_string() ever work?
 *			it had a misplaced `}'...ljn
 *	04-apr-91	PR1308: Use SVAL instead of +i...ljn
 *	09-jul-93	Use of vmcoproto.h required some changes...rt
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include 	"parblk.inc"	/* get P_BADNAME, etc.                  */ 
#include 	"symtab.inc" 
#include        "fileinc.inp"
#include        "vminc.inc"     /* Dynamic allocation definitions      */
#include        "terminc.inc"
#include        "tmhost.inp"
#include        <stdio.h>
#include "taeintproto.h"


#ifdef UNIX
#define	GETLUN_TAE	FOR_NAME2_(getlun_tae, GETLUN_TAE)
#define	SETLUN	FOR_NAME2(setlun, SETLUN)
#define	GETSTD	FOR_NAME2(getstd, GETSTD)
#define SETSTD	FOR_NAME2(setstd, SETSTD)
#define OPNSTD	FOR_NAME2(opnstd, OPNSTD)
#else			/* for VMS 	*/	
#define	GETLUN_TAE	getlun_tae
#define	SETLUN	setlun
#define	GETSTD	getstd
#define SETSTD	setstd
#define OPNSTD	opnstd
#endif

FUNCTION  VOID  SETLUN
(
 int		*lun		/* out: lun number		*/
);
FUNCTION  VOID  GETLUN_TAE
(
 int		*lun		/* in: lun number		*/	
 );
FUNCTION  VOID GETSTD
(
 int		*term		/* out: TRUE if terminal	*/
 );
FUNCTION  VOID SETSTD
(
 int		*term		/* in: TRUE if terminal		*/
 );
FUNCTION void OPNSTD(char*, int*, int*);

    extern COUNT	bytes;
    extern TEXT  	cname[STRINGSIZ+1]; /* generic C string for F77 conversion */
    extern CODE	code;

/*
*	 xvm_call -- installation dependent initialization
*/

FUNCTION VOID BRIDGE2_NAME(xvm_call) 
(
	TAEINT *vmid,		/* in:id for a vm object. */
	TAEINT *status		/* out:  SUCCESS or error coded */  

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 	  Vm_Call((GENPTR) *vmid); */
/* 		*status = SUCCESS;		 */
/* 		return; */
	}


/*
*	 xvm_copy -- copy one Vm object  to another
*/

FUNCTION VOID BRIDGE2_NAME(xvm_copy) 
(
        TAEINT *targetId,       /* in:id for target vm object. */
        TAEINT *sourceId,       /* in:id for source vm object. */
	TAEINT *status		/* out:  SUCCESS or error coded */  

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 	  Vm_Copy((struct VM_STRUCT *) *targetId,  */
/* 		  (struct VM_STRUCT *) *sourceId); */
/* 		*status = SUCCESS;		 */
/* 		return; */
	}


/*
*	xvm_get_attribute -- extract variable attributes
*/

FUNCTION VOID BRIDGE2_NAME(xvm_get_attribute) 
(
	TAEINT *vmid,	/* in:id for a vm object. */
	FORSTR *name,	/* in: name of the variable to extract attributes */
	TAEINT *type,	/* out: type of the variable. */
	TAEINT *n,	/* out: current number of values for the variable. */
	BOOL   *dflt,	/* out: TRUE if value of variable is the default . */
	TAEINT *access,	/* out: if variable is a file, the access mode.  */
	TAEINT *status	/* out:  SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c (name, cname, 0); */
/* 		s_strip (cname); */
/* 		code = Vm_GetAttribute(*vmid, cname, type, n, dflt, access); */
/* 		*status = code;		 */
/* 		return; */
	}



/*
*	xvm_get_intg -- extract  an integer variable
*/

FUNCTION VOID BRIDGE2_NAME(xvm_get_intg) 
(
	TAEINT *vmid,		/* in: id for a vm object. */
	FORSTR *varnam,		/* in: name of the integer variable to locate */
	TAEINT *dim,		/* in: maximum number of values expected. */
	TAEINT intg[],		/* out: current number of values for variable */
	TAEINT *count,		/* out:  actual number of variables received */
	TAEINT *status		/* out:  SUCCESS or error code */

 )
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(varnam, cname, 0); */
/* 		s_strip(cname); */
/* 		code - Vm_GetIntg (*vmid, cname, *dim, intg, count); */
/* 		*status = code; */
/* 		return; */
	}


/*
*	xvm_get_dble -- extract  a double precision variable
*/

FUNCTION VOID BRIDGE2_NAME(xvm_get_dble) 
(
	TAEINT *vmid,		/* in:  id for a vm object */
	FORSTR *varnam,		/* in:  name of the real variable to extract */
	TAEINT *dim,		/* in: maximum number of values expected */
	double dble[],		/* out:  double precision values */
	TAEINT *count,		/* out:  actual number of strings received */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(varnam, cname, 0); */
/* 		s_strip(cname); */
/* 			/\* TAE uses TAEFLOAT *\/ */
/*                 if (sizeof(double) == sizeof(TAEFLOAT)) */
/*   		    code = Vm_GetReal(*vmid, cname, *dim, dble, count);  */
/* 		else */
/*                     { */
/*                     TAEFLOAT taefloat[MAXVAL]; */
/*                     COUNT i; */
/*   		    code = Vm_GetReal(*vmid, cname, *dim, taefloat, count);  */
/*                     for (i=0; i<*count; ++i) */
/*                         dble[i] = (double) taefloat[i]; */
/*                     }		 */
/* 		*status = code;		return; */
	}
/*
*	xvm_get_real -- extract  a real variable
*/

FUNCTION VOID BRIDGE2_NAME(xvm_get_real) 
(
	TAEINT *vmid,		/* in:  id for a vm object */
	FORSTR *varnam,		/* in:  name of the real variable to extract */
	TAEINT *dim,		/* in: maximum number of values expected */
	float  real[],		/* out:  single precision real values */
	TAEINT *count,		/* out:  actual number of strings received */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(varnam, cname, 0); */
/* 		s_strip(cname); */
/*                 if (sizeof(float) == sizeof(TAEFLOAT)) */
/*   		    code = Vm_GetReal(*vmid, cname, *dim, real, count);  */
/* 		else */
/*                     { */
/*                     TAEFLOAT taefloat[MAXVAL]; */
/*                     COUNT i; */
/*   		    code = Vm_GetReal(*vmid, cname, *dim, taefloat, count);  */
/*                     for (i=0; i<*count; ++i) */
/*                         real[i] = (float) taefloat[i]; */
/*                     }		 */
/* 		*status = code; */
/* 		return; */
	}


/*
*	xvm_get_str -- extract  a string variable
*/


FUNCTION VOID BRIDGE2_NAME(xvm_get_str) 
(
	TAEINT	*vmid,		/* in:	id for a vm object */
	FORSTR	*varnam,	/* in:	name of string variable to extract */
	TAEINT	*dim,		/* in:	maximum number of values expected */
	FORSTR	*string,	/* out:	Array of strings in FOR-77 format */
	TAEINT	*count,		/* out:	actual number of strings received */
	TAEINT	*status	/* out:	SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 	     TEXT     		**sptr;	/\* pointer to string vetor *\/ */
/* 	     TAEINT   		i; */
/* 	     IMPORT TEXT         pm_dim[],pk_dim[]; */
/* 	     struct VM_STRUCT    *vm; */
/* 	     struct PARBLK       *p; */
/* 	     struct VARIABLE 	 *v; */
	     
/*   	     vm = (struct VM_STRUCT *)*vmid;  */
/* 	     p = (struct PARBLK *)&((*vm).npblk);  */
/* /\* modelled after xrgen:xrstr *\/ */
/* 	     s_for2c(varnam, cname, 0); */
/* 	     s_strip(cname); */
/* 	     v = p_find(p, cname); */
/* 	     if (v == NULL) */
/* 	     { */
/* 		*status = P_BADNAME; */
/* 		return; */
/* 	     } */
/* 	     if ((*v).v_type != V_STRING) */
/* 	     { */
/* 		*status = P_BADTYPE; */
/* 		return; */
/* 	     } */
/* 	     sptr = (TEXT **) (*v).v_cvp;        /\* value pointer          *\/ */
/* 	     *status = SUCCESS; */
/* 	     i = (*v).v_count; /\* number of strings *\/ */
/* 	     *count = i;  */
/* 	     if ((*v).v_count > *dim) */
/* 	     { */
/* 	       x_error((*vm).npblk.mode, pm_dim, pk_dim, (uintptr_t) cname,  */
/* 		       0, 0);  */
/* 		  *status = P_BADCOUNT;           /\* bad count            *\/ */
/* 		  return; */
/* 	     }  */
/* 	     for (i=0; i < (*v).v_count && i < *dim; i++) */
/* 	     { */
/* 		  code = s_c2for(sptr[i], string, i); /\* string to caller *\/ */
/* 		  if (code != SUCCESS) */
/* 		  { */
/* 		    x_error((*p).mode, "String size overflows buffer",  */
/* 			    "TAE-OVER",0,0,0); */
/* 		    *status = P_OVER; */
/* 		    return; */
/* 		  } */
/* 	     } */
/* 	     *status = SUCCESS; */
/* 	     return; */
	}
	
#ifdef VMFIND 
/*
*	 xvm_find -- find a variable within a Vm object
*		return the VARIABLE pointer, aborts if no such variable
*/

FUNCTION VOID BRIDGE2_NAME(xvm_find) 
(
        TAEINT *vmid,		/* in:id for vm object. */
	FORSTR *varnam,		/* in: name of the variable to locate */
        TAEINT *var,       	/* out: 'C' pointer to variable structure*/
	TAEINT *status		/* out:  SUCCESS or error coded */  

 )
	{
        	struct  VARIABLE    *Vm_Find();
		var = (TAEINT *)Vm_Find(*vmid, (*varnam).pointer);
		*status = SUCCESS;
		return;
	}

/*
*	 xvm_findvar -- find a variable within a Vm object
*		return the VARIABLE pointer, status = FAIL if not found
*/

FUNCTION VOID BRIDGE2_NAME(xvm_findvar) 
(
        TAEINT *vmid,		/* in:id for vm object. */
	FORSTR *varnam,		/* in: name of the variable to locate */
        TAEINT *var,       	/* out: 'C' pointer to variable structure*/
	TAEINT *status		/* out:  SUCCESS or error coded */  

 )
	{
        	struct  VARIABLE    *Vm_FindVar();
		var = (TAEINT *)Vm_FindVar(*vmid, (*varnam).pointer);
		if (var)
		    *status = SUCCESS;
		else
		    *status = FAIL;
		return;
	}
#endif

/*
*	xvm_free -- deallocate an xv parameter object
*/

FUNCTION VOID BRIDGE2_NAME(xvm_free) 
(
	TAEINT	*vmid,			/*  in:	  id of a vm object */
	TAEINT	*status		/*  out:  SUCCESS of error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 	  Vm_Free((GENPTR) *vmid); */
/* 		*status = SUCCESS;		 */
/* 		return; */
	}


/*
*	xvm_format_var -- format variables
*/

FUNCTION VOID BRIDGE2_NAME(xvm_format_var) 
(
	TAEINT *vmid,	/* in:  id of a vm object */
	FORSTR *line,	/* out: string containing variables from Vm object */
	TAEINT *length,	/* in:  length of output line in bytes */
	TAEINT *status		/*  out:  SUCCESS of error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/*   		code = Vm_FormatVar(*vmid, (*line).pointer, *length);  */
/* 		(*line).length = s_length((*line).pointer); */
/* 		*status = code;		 */
/* 		return; */
	}

/*
*	xvm_get_host_err -- get host-dependent error code
*/

FUNCTION VOID BRIDGE2_NAME(xvm_get_host_err) 
(
	TAEINT *vmid,		/* in:  id of a vm object */
	TAEINT *vmcode,		/* out:  host-dependent error code */
	TAEINT *status		/* out:  SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		Vm_GetHostError(*vmid, vmcode); */
/* 		*status = SUCCESS;		 */
/* 		return; */
	}


/*
*	xvm_dyn_tutor -- initiate dynamic tutor
*/

FUNCTION VOID BRIDGE2_NAME(xvm_dyn_tutor) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *pdfnam,		/* in: pdf file spec for tae monitor */
	TAEINT *pdfset,		/* in:  mode */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(pdfnam, cname, 0); */
/* 		code = Vm_DynTutor(*vmid, cname, *pdfset); */
		 
/* 		*status = code;		 */
/* 		return; */

	}



/*
*	xvm_init_format -- initialize formatting variables
*/

FUNCTION VOID BRIDGE2_NAME(xvm_init_format) 
(
	TAEINT *vmid,		/* in:  id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */ 

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		Vm_InitFormat(*vmid); */
/* 		*status = SUCCESS;		 */
/* 		return; */
	}



/*
*	xvm_new -- allocate a new xv parameter object
*/

FUNCTION VOID BRIDGE2_NAME(xvm_new) 
(
	TAEINT *vmid,		/* out: id of a vm object */
	TAEINT *mode,		/* in: how function should handle errors */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/*                 /\* We should not assume a pointer is sizeof(TAEINT) *\/ */
/* 		*vmid = (TAEINT)Vm_New(*mode);  */
/* 		*status = SUCCESS;		return; */
	}


/*
*	xvm_init -- initialize I/O and open the current standard output device
*/
/* 		  Modelled after zxinit and Vm_OpenStdout */

FUNCTION VOID BRIDGE2_NAME(xvm_init) 
(
TAEINT *vmid,		/* in: id of a vm object */
TAEINT *lun,		/* in: logical unit number */
TAEINT *status		/* out: SUCCESS or error code */

 )
{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/*     IMPORT	BOOL	term_std;    /\* true if terminal is stdout   *\/ */
/*     IMPORT	TEXT	applic_type;    /\* application type ('for')  *\/ */
/*     IMPORT	TEXT    pm_dim[],pk_dim[],pm_type[],pk_type[]; */
/*     struct  VARIABLE    *v; */
/*     CODE		length, termtype; */
/*     COUNT		termlines; */
/*     COUNT		termcols; */
/*     TEXT		msgbuf[STRINGSIZ+1]; */
/*     TEXT		**stdr_ptr = 0;	/\* stdrec string vector pointer	*\/ */
/*     COUNT		len; */
/*     CODE		code; */
/*     struct    VM_STRUCT *vm; */
/*     BOOL		newfile, opened; */
/*     GENPTR		h; */
/*     vm = (struct VM_STRUCT *)*vmid; */
/*     h  = (GENPTR)*vmid; */
/*     SETLUN ( lun); */
/*     t_init(&termlines, &termcols, &termtype);	/\* initialize terminal pkg *\/ */

/*     Vm_Call (h);				/\* application init	   *\/ */
/*     if ((code = Vm_ReadFromTM(h)) != SUCCESS)  */
/*     { */
/* 	*status = code; */
/* 	return; */
/*     } */
/*     v = Vm_Find(h, "_STDOUT");		        /\* get the string	   *\/ */
/*     if ( v == NULL ) */
/*       x_error( (*vm).npblk.mode,pm_type, pk_type, (uintptr_t) "_STDOUT",0,0); */
/*     else if ((*v).v_type != V_STRING) */
/*       x_error( (*vm).npblk.mode,pm_type, pk_type, (uintptr_t) "_STDOUT",0,0); */
/*     else */
/*         stdr_ptr = (TEXT **) (*v).v_cvp;	/\* get value pointer	   *\/ */

/*     term_std = (s_equal(TERMINAL, stdr_ptr[0]));   /\* filename = terminal ? *\/ */
/*     SETSTD (&term_std); */
/*     if (*lun == 6) */
/*     { */
/* 	*status = SUCCESS; */
/* 	return; */
/*     } */
/* #ifdef UNIX */
/*     length = s_lower ( stdr_ptr[0] ); */
/* #else */
/*     length = s_length( stdr_ptr[0] ); */
/* #endif */
/*     if (!s_equal(stdr_ptr[1] , "CREATE")) */
/* 	newfile = TRUE; */
/*     else */
/* 	newfile = FALSE; */
/*     OPNSTD (stdr_ptr[0], &newfile, &opened); */
/*     if (opened == FALSE) */
/*     { */
/* 	if ((*vm).npblk.mode == P_ABORT) */
/* 	{ */
/* 	    len = s_copy("Could not open standard output file ", msgbuf); */
/* 	    s_bcopy(stdr_ptr[0], &msgbuf[len], STRINGSIZ); */
/* 	    t_write(msgbuf, T_STDCC);			/\* errmsg to terminal*\/ */
/* 	    z_exit(-1, "TAE-STDOPEN");			/\* set SFI, SKEY     *\/ */
/*  	} */
/*     } */
/*     applic_type = FORTRAN_TYPE;			/\* 'F77' language application *\/ */
/*     *status = SUCCESS; */
/*     return ; */
    }



/*
*	xvm_read_from_disk -- read from disk
*/


FUNCTION VOID BRIDGE2_NAME(xvm_read_from_disk) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *fspec,		/* in: host file specification of file */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		struct  VM_STRUCT   *vm; */
/* 		struct PARBLK       *p; */
		
/* 		code = s_for2c (fspec, cname, 0); */
/* 		s_strip(cname); */
/* 		if (code != SUCCESS)  */
/* 		{ */
/* 	   		vm = (struct VM_STRUCT *)*vmid; */
/* 	   		p = (struct PARBLK *)&(*vm).npblk; */
/* 			x_error ((*p).mode, "Filename too long", "TAE-OVER",0,0,0); */
/* 			*status = P_BADNAME; */
/* 			return; */
/* 		} */
/* 		code = Vm_ReadFromDisk(*vmid,cname); */
/* 		*status = code; */
/* 		return; */
	}

/*
*	xvm_read_from_tm -- receive variables from tm
*/

FUNCTION VOID BRIDGE2_NAME(xvm_read_from_tm) 
(
	TAEINT *vmid,		/* in: id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		code = Vm_ReadFromTM (*vmid); */
/* 		*status = code; */
/* 		return; */
	}


/*
*	xvm_set_intg -- set values for integer variables
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_intg) 
(
	TAEINT *vmid,	/* in: id of a vm object */
	FORSTR *name,	/* in: name of the tcl integer variable to be set */
	TAEINT *count,	/* in: multiplicity of the variable */
	TAEINT intg[],	/* in: an array of integer values for the variables */
	TAEINT *mode,	/* in: variable being added or being modified */
	TAEINT *status	/* out: SUCCESS or error code */

 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();

/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */
/* 		code = Vm_SetIntg(*vmid, cname, *count, intg, *mode);  */
/* 		*status = code;		 */
/* 		return; */
	}

/*
*	xvm_set_valid_intg -- set integer valids
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_valid_intg) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of tcl integer variable */
	TAEINT *count,		/* in: multiplicity of the valids */
	TAEINT ilo[],		/* in: array of integers for low values */
	TAEINT ihi[],		/* in: array of integers for high values */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */
/* 		code = Vm_SetValidIntg(*vmid, cname, *count, ilo, ihi);  */
/* 		*status = code; */
/* 		return; */
	}



/*
*	xvm_set_max -- set maximum vector count of a variable
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_max) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *maxcnt,		/* in: max vector count of variable */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */

/* 		code = Vm_SetMax(*vmid, cname, *maxcnt);  */
/* 		*status = code; */
/* 		return; */
	}



/*
*	xvm_set_min -- set minimum vector count of a variable
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_min) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *mincnt,		/* in: min vector count of variable */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */

/* 		code = Vm_SetMin(*vmid, cname, *mincnt);  */
/* 		*status = code; */
/* 		return; */
	}



/*
*	xvm_set_next_menu -- set next menu to be executed
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_next_menu) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */
/* 		code = Vm_SetNextMenu(*vmid, cname); */
/* 		*status = code; */

/* 		return; */
	}




/*
*	xvm_set_parm_page -- set parameter (variable) page indicator
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_parm_page) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: FORSTR *name of the variable */
	BOOL   *flag,		/* in: sets page indicator */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */
/* 		code = Vm_SetParmPage(*vmid, cname, *flag); */
/* 		*status = code; */
/* 		return; */
	}


/*
*	xvm_set_dble -- set values for double precision variables
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_dble) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *count,		/* in: number of real values in the real array */
	double dble[],		/* in: array of double precision values */
	TAEINT *mode,		/* in: variable being added or being modified */	
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */

/*                 if (sizeof(double) == sizeof(TAEFLOAT)) */
/* 		    code = Vm_SetReal(*vmid, cname, *count, dble, *mode); */
/*                 else */
/*                     { */
/*                     TAEFLOAT taefloat[MAXVAL]; */
/*                     COUNT i, maxcnt; */
/*                     maxcnt = (*count > MAXVAL) ? MAXVAL : *count; */
/*                     for (i=0; i<maxcnt; ++i) */
/*                         taefloat[i] = (TAEFLOAT) dble[i]; */
/* 		    code = Vm_SetReal(*vmid, cname, maxcnt, taefloat, *mode); */
/*                     } */
/* 		*status = code; */
/* 		return; */
	}
/*
*	xvm_set_real -- set values for real  variables
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_real) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the variable */
	TAEINT *count,		/* in: number of real values in the real array */
	float  real[],		/* in: array of real values for the variable */
	TAEINT *mode,		/* in: variable being added or being modified */	
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */
/*                 if (sizeof(float) == sizeof(TAEFLOAT)) */
/* 		    code = Vm_SetReal(*vmid, cname, *count, real, *mode); */
/*                 else */
/*                     { */
/*                     TAEFLOAT taefloat[MAXVAL]; */
/*                     COUNT i, maxcnt; */
/*                     maxcnt = (*count > MAXVAL) ? MAXVAL : *count; */
/*                     for (i=0; i<maxcnt; ++i) */
/*                         taefloat[i] = (TAEFLOAT) real[i]; */
/* 		    code = Vm_SetReal(*vmid, cname, maxcnt, taefloat, *mode); */
/*                     } */
/* 		*status = code; */
/* 		return; */
	}

/*
*	xvm_set_valid_dble -- set double precision valids
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_valid_dble) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the tcl integer variable. */
	TAEINT *count,		/* in: multiplicity of the valids */
	double dlo[],		/* in: array of doubles for the low values */
	double dhi[],		/* in: array of doubles for the high values */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */

/*                 if (sizeof(double) == sizeof(TAEFLOAT)) */
/* 		    code = Vm_SetValidReal(*vmid, cname, *count, dlo, dhi);  */
/*                 else */
/*                     { */
/*                     TAEFLOAT tflo[MAXVAL]; */
/*                     TAEFLOAT tfhi[MAXVAL]; */
/*                     COUNT i, maxcnt; */
/*                     maxcnt = (*count > MAXVAL) ? MAXVAL : *count; */
/*                     for (i=0; i<maxcnt; ++i) */
/*                         { */
/*                         tflo[i] = (TAEFLOAT) dlo[i]; */
/*                         tfhi[i] = (TAEFLOAT) dhi[i]; */
/*                         } */
/* 		    code = Vm_SetValidReal(*vmid, cname, maxcnt, tflo, tfhi);  */
/* 		    } */
/* 		*status = code; */
/* 		return; */
	}

/*
*	xvm_set_valid_real -- set real valids
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_valid_real) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of the tcl integer variable. */
	TAEINT *count,		/* in: multiplicity of the valids */
	float  rlo[],		/* in: array of reals for the low values */
	float  rhi[],		/* in: array of reals for the high values */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */
/*                 if (sizeof(float) == sizeof(TAEFLOAT)) */
/* 		    code = Vm_SetValidReal(*vmid, cname, *count, rlo, rhi);  */
/*                 else */
/*                     { */
/*                     TAEFLOAT tflo[MAXVAL]; */
/*                     TAEFLOAT tfhi[MAXVAL]; */
/*                     COUNT i, maxcnt; */
/*                     maxcnt = (*count > MAXVAL) ? MAXVAL : *count; */
/*                     for (i=0; i<maxcnt; ++i) */
/*                         { */
/*                         tflo[i] = (TAEFLOAT) rlo[i]; */
/*                         tfhi[i] = (TAEFLOAT) rhi[i]; */
/*                         } */
/* 		    code = Vm_SetValidReal(*vmid, cname, maxcnt, tflo, tfhi);  */
/* 		    } */
/* 		*status = code; */
/* 		return; */
	}

/*
*	xvm_set_string_length -- set maximum string length of a string or keyword variable
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_string_length) 
(	
	TAEINT *vmid,	/* in: id of a vm object */
	FORSTR *name,	/* in: name of tcl variable or keyword */
	TAEINT *strlen,	/* in: max length of a string for the variable */
	TAEINT *status	/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(name, cname, 0); */
/* 		s_strip(cname); */
/* 		code = Vm_SetStringLength(*vmid, cname, *strlen); */
/* 		*status = code; */
/* 		return; */
	}

/*
*	xvm_set_string -- set values for a string or keyword variable
*		(modeled after q_string in qparmgen.c)
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_string) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *name,		/* in: name of tcl string variable */
	TAEINT *count,		/* in: multiplicity of the variable */
	FORSTR *string,		/* in: array of strings F77 format */
	TAEINT *mode,		/* in: variable being added or being modified */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 	   struct PARBLK       *p; */
/* 	   struct VM_STRUCT    *vm; */
/* 	   struct VARIABLE     *v; */
/* 	   TAEINT i; */
/* 	   IMPORT TEXT pm_size[], pk_size[];   /\* error message and key    *\/ */
/* 	   IMPORT TEXT pm_room[], pk_room[];   /\* error message and key    *\/ */
/* 	   IMPORT TEXT pm_count[], pk_count[];   /\* error message and key    *\/ */
/* 	   TEXT        cstring[STRINGSIZ+1];   /\* local c string           *\/ */
/* 	   TEXT        *loc_vector[MAXVAL];    /\* local ptr vector     *\/ */
 	   	
/* 	   vm = (struct VM_STRUCT *)*vmid; */
/* 	   p = (struct PARBLK *)&(*vm).npblk; */
/* 	   if (*count > MAXVAL) */
/* 	   { */
/* 	     x_error ((*p).mode, pm_count, pk_count, (uintptr_t) count,0,0); */
/* 		*status = P_BADCOUNT; */
/* 		return; */
/* 	   } */
/* 	   s_for2c(name, cname, 0); */
/* 	   s_strip(cname); */
/* 	   for (i = 0; i < *count ; i++) */
/* 	   { */
/* 	    code = s_for2c(string, cstring, i); /\* convert ith string to c *\/ */
/* 	    if (code != SUCCESS) */
/* 	    { */
/* 	      x_error ((*p).mode, pm_size, pk_size, (uintptr_t) cname,0,0);  */
/* 		*status = P_OVER; */
/* 		return; */
/* 	    } */
/* 	    s_strip(cstring);        /\* strip trailing blanks             *\/ */
/* 	    loc_vector[i] = Vm_Save(cstring); */
/* 	    if (loc_vector[i] == NULL) */
/* 	    { */
/* 	      x_error((*p).mode, pm_room, pk_room, (uintptr_t) cname,0,0); */
/* 		*status = P_NOROOM; */
/* 		return; */
/* 	    } */
/* 	   } */
/* 	   code = q_prep(p, cname, *count, V_STRING, *mode, &v, STRINGSIZ); */
/* 	   if (code != SUCCESS) */
/* 	   { */
/* 		   *status = code; */
/* 		   return; */
/* 	   } */
/* 	   for(i=0; i < *count; i++)          /\* move string ptrs to (*v)     *\/ */
/* 		   SVAL(*v,i) = loc_vector[i]; */
/* 	   *status = SUCCESS; */
/* 	   return; */
	}


/*
*	xvm_set_valid_string -- set string valids modelled after q_validstr
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_valid_string) 
(	
TAEINT *vmid,		/* in: id of a vm object */
FORSTR *name,		/* in: name of the variable */
TAEINT *count,		/* in: multiplicity of the valids */
FORSTR vector[],	/* in: array of strings for the variable */
TAEINT *status		/* out: SUCCESS or error code */

	)
{
 	   	
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/*     struct VARIABLE	*v; */
/*     IMPORT TEXT pm_room[], pk_room[];   /\* error message and key    *\/ */
/*     COUNT		i,j; */
/*     CODE		alloctype, code; */
/*     struct S_VALID      *svalid; */
/*     TEXT		*s; */
/*     struct PARBLK       *p; */
/*     struct VM_STRUCT    *vm; */
/*     TEXT        cstring[STRINGSIZ+1];   /\* local c string           *\/ */
/*     COUNT		maxLength; */
/*     COUNT		length; */
/*     IMPORT TEXT pm_count[], pk_count[];   /\* error message and key    *\/ */
/*     IMPORT TEXT pm_dim[],pk_dim[],pm_type[],pk_type[]; */
/*     IMPORT TEXT pm_size[], pk_size[];   /\* error message and key    *\/ */

/*     vm = (struct VM_STRUCT *)*vmid; */
/*     p = (struct PARBLK *)&(*vm).npblk; */
/*     if (*count > MAXVAL) */
/*     { */
/*       x_error ((*p).mode, pm_count, pk_count, (uintptr_t) count,0,0); */
/* 	*status = P_BADCOUNT; */
/* 	return; */
/*     } */
/*     s_for2c(name, cname, 0); */
/*     s_strip(cname); */
/*     if ( (*p).mode == P_ABORT || (*p).mode ==P_CONT ) */
/*         alloctype = P_MODE_RESTRICT; */
/*     else */
/*         alloctype = (*p).mode & ( P_MODE_RESTRICT | P_MODE_TAE_ALLOC); */

/*     code = q_validprep(p, cname, *count, V_STRING, &v); */
/*     if (code != SUCCESS) */
/*     { */
/* 	*status = code; */
/*         return; */
/*     } */

/*     svalid = (struct S_VALID *)(*v).v_valid; */
/*     if ( svalid == NULL ) */
/*     { */
/* 	*status = code; */
/*         return; */
/*     } */
/*     (*svalid).count = *count;		/\* set new count		*\/ */
/*     maxLength = 0; */
    
/*     for(i=0; i < *count; i++) */
/*     { */
/* 	code = s_for2c(vector, cstring, i); /\* convert ith string to c *\/ */
/* 	if (code != SUCCESS) */
/* 	{ */
/* 	  x_error ((*p).mode, pm_size, pk_size, (uintptr_t) cname,0,0);  */
/* 		*status = P_OVER; */
/* 		return; */
/* 	} */
/* 	length = s_strip(cstring);   /\* strip trailing blanks        *\/ */

/* #ifdef POINTER_VALIDS */
/* 	    if (length > maxLength) */
/* 		maxLength = length; */
/* 	    if (alloctype == P_MODE_TAE_ALLOC) */
/* 	        s = (TEXT *) tae_alloc (1, length+1);  */
/* 	    else */
/* 	    { */
/* 		s = (TEXT *) r_alloc ((*p).pool, length+1); */
/* 		if (s == NULL) */
/* 		{ */
/* 			*status = P_FAIL; */
/* 			return; */
/* 		} */
/* 	    } */
/* 	    (*svalid).slist[i].string = s; */
/* #endif */
/* 	s_bcopy(cstring, (*svalid).slist[i].string, VALIDSIZ); */
/*     } */
/* #ifdef PONITER_VALIDS */
/*     (*v).v_size = maxLength; */
/* #endif */
/*     j = 0; */
/*     while ( j < (*v).v_count ) */
/*     { */
/*         if ( SVAL(*v,j) != NULL ) */
/* 	{ */
/*                 code = P_FAIL; */
/*         	if ( s_equal ( SVAL(*v,j), cstring) ) */
/* 		{ */
/*                         code = SUCCESS; */
/*                 	break; */
/*                 } */
/* 	} */
/*         if ( code != SUCCESS ) */
/* 	{ */
/* 		*status = code; */
/* 		return; */
/* 	} */
/*         j++; */
/*      } */
/*      *status = code; */
/*      return; */
}

/*
*	xvm_set_tcl_var -- set tcl variable
*/

FUNCTION VOID BRIDGE2_NAME(xvm_set_tcl_var) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		code = Vm_SetTCLVar(*vmid); */
/* 		*status = code; */
/* 		return; */
	}


/*
*	xvm_write_to_disk -- write the vm object to a disk file
*/

FUNCTION VOID BRIDGE2_NAME(xvm_write_to_disk) 
(	
	TAEINT *vmid,		/* in: id of a vm object */
	FORSTR *fspec,		/* host file specification of file to be created */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(fspec, cname, 0); */
/* 		s_strip(cname); */
/* 		*status = Vm_WriteToDisk(*vmid, cname); */
/* 		return; */
	}
