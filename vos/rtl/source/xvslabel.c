/******************************************************************************
 * int status = zvslabel(char ***labels, int *nlabels, int mvlimit,
 *                       int incl_def)
 *
 * Returns in an array of strings all the user-specified parameters.  The
 * array of strings is dynamically allocated by this routine and returned.
 * Each label keyword is in a separate element of the array.  The label itself
 * is formatted as in the VICAR label.  To do a full free the caller should:
 *
 *    char **labels;
 *    int nlabels;
 *    status = zvslabel(&labels, &nlabels, 0, 0, 0);
 *    for (int i=0; i < nlabels; i++)
 *       if (labels[i] != NULL) free labels[i];
 *    free labels;
 *
 * labels: Pointer to a string array.  The array itself is allocated by this
 *    routine, as are the strings themselves (see above).  Note that pointers
 *    in the array may be null.
 * nlabels: Pointer to int.  Returns the size of the labels array.
 * mvlimit: Input int.  Puts a limit on the number of values of a multi-value
 *    parameter to be printed.  0 means unlimited (up to 32767 in theory,
 *    although each string is limited to 32767 chars).
 * incl_def: Input int.  If 0 (false), defaulted parameters are not included.
 *    This most closely matches what was actually given on the command line.
 *    If non-0 (true), defaults are included.
 *
 * Returns SUCCESS on success.  Does NOT participate in normal RTL error
 * processing because that requires a unit, which this routine does not have.
 *
 * Written:
 *         rgd   2020-10-29
 *
 * IMPORTANT NOTE
 *
 * There is no Fortran-callable version of this at this time.  The complexity
 * of dealing with the string array combined with the lack of a use case makes
 * it not worth the trouble.  It can be added later if needed.
 *
 ******************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "strcasecmp.h"

int zvslabel(char ***labels, int *nlabels, int mvlimit, int incl_def) 
{  
   struct SYMTAB *symtab;
   struct PARBLK *block;     
   struct VARIABLE *v;

   char *pname;
   int i;
   int limit;

   char strbuf[32767];
   char tmpval[255];

   if (mvlimit<=0) {
      mvlimit=32767;
   }    
    
   block=(struct PARBLK*)&parb;   
   symtab=&((*block).symtab);

   /* Pre-count the number of symbols so we can allocate the array */

   *nlabels = 0;

   for (v=(*symtab).link; v != NULL; v=(*v).v_link) {                

      if ( incl_def || (*v).v_default==0 ) {
      
         pname = (*v).v_name;
         
         /* system labels start with $ or _ */
         if ( pname[0]!='$' && pname[0]!='_') {

            (*nlabels)++;         
         }
      }
   }

   *labels = malloc(*nlabels * sizeof(char *));

   if (*labels == NULL)
      return INSUFFICIENT_MEMORY;

   int lnum = 0;

   for (v=(*symtab).link; v != NULL; v=(*v).v_link) {                

      if ( incl_def || (*v).v_default==0 ) {
      
         limit = min( mvlimit,(*v).v_count );
         pname = (*v).v_name;

         /* system labels start with $ or _ */
         if ( pname[0]!='$' && pname[0]!='_') {

	    /* write keyword to string */
            sprintf(strbuf, "%s=", pname);

            if (limit > 1)
               strcat(strbuf, "(");	/* multivalued */

            for (i = 0;  i < limit ; i++) {
 
               if (i != 0)
                  strcat(strbuf, ",");

               switch ((*v).v_type) {	/* add value according to type */
	    
                  case V_INTEGER: 
 
                     sprintf(tmpval, "%d",IVAL(*v,i));
                     strcat(strbuf, tmpval);
                     break;

                  case V_REAL:                                 
 
                     sprintf(tmpval, "%.16g", (double)RVAL(*v,i));
                     strcat(strbuf, tmpval);
                     break;
	
                  case V_STRING:
                   
                     /* This is complicated because we have to double quotes */
                     strcat(strbuf, "'");
                     char *p = SVAL(*v,i);
                     while (*p) {
                        if (*p == '\'') {
                           strcat(strbuf,"''");		/* double the quotes */
                        } else {
                           tmpval[0] = *p;
                           tmpval[1] = '\0';
                           strcat(strbuf, tmpval);
                        }
                        p++;
                     }
                     strcat(strbuf, "'");
                     break;           

               }
            }
            if (limit > 1)
               strcat(strbuf, ")");	/* multivalued */

            (*labels)[lnum] = strdup(strbuf);
            if ((*labels)[lnum] == 0)
               return INSUFFICIENT_MEMORY;
            lnum++;

         }	/* if ( pname[0]!= */                
      } 	/* if default */             
   } 	/* for v */            

   return SUCCESS;
}

