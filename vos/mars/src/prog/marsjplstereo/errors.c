#include <stdio.h>
#include <stdlib.h> /* getenv */
#define ERRORS_C_SOURCE
#include "errors.h"

/* Error file output --
	Program by:  Mark Maimone  1/26/86
	Last Modified:  1/26/86

HISTORY
	26-Jan-86 (mwm)   Created.


	The functions defined below implement an effective device for
   communicating error conditions.  The function   einit   must be called
   before any errors will be output.  A source file may then call function
   eprintf   at any point (in the same format as   printf), and the text
   will be output to one of three places:

	1. not output:  if   einit()   was not invoked or the environment
			variable defined below in ENV_VARNAME (presently
			"EPRINTF") is not defined, no text will be output.
	2. stderr:	if   einit()   was invoked and ENV_VARNAME is defined
			but holds an empty string, output will go to
			stderr.  This will also be the case if ENV_VARNAME
			holds a value, but that value could not be opened
			as a file for appending.
	3. filename:	if   einit()   was invoked and ENV_VARNAME is defined
			and holds a value, that value will be treated as a
			filename to which error output should be sent.
*/




#define ENV_VARNAME "EPRINTF"	/* Environment variable name must be in
				   quotes */

static int e_output = 0;	/* Boolean flag; true iff e_fp is valid.
				   Also points to the string contained in
				   environment variable ENV_VARNAME */
static FILE *e_fp = NULL;	/* Pointer to error output stream */

/* einit -- defines values for global parameters   e_output and e_fp.
   e_output   will hold 0 if the ENV_VARNAME is not defined, and will
   point to the string value otherwise.   e_fp   will be the error output
   stream defined as follows:

	1. NULL - if ENV_VARNAME is not defined.
	2. stdout - if ENV_VARNAME is defined but does not hold an
		    accessible filename.
	3. file pointer - if ENV_VARNAME holds a filename which can be
			  opened for appending.

   PARAMETERS:
	none

   GLOBALS:
	e_output -- BOOLEAN, true iff error output should be generated.
	e_fp -- error output stream, may be NULL.
*/

void einit ()
{
    char *env_ptr = getenv (ENV_VARNAME);

    e_output = (int) (env_ptr != NULL);
    if (env_ptr && *env_ptr == '\0')

/* If environment variable is defined but empty, use   stderr */

	e_fp = stderr;
    else
	if (env_ptr == NULL || (e_fp = fopen (env_ptr, "a")) == NULL)

/* If environment variable has a filename, attempt to send output there.
   If the file cannot be opened, default to   stderr */

	    e_fp = stderr;
} /* einit */

/* eprintf -- write error output to appropriate error file.

   PARAMETERS:
	exactly those defined for   printf.

   GLOBALS:
	e_output -- BOOLEAN, true iff error output should be generated.
	e_fp -- error output stream, may be NULL.
*/

int eprintf (const char *cntl, long int a, long int b, long int c, long int d,
	     long int e, long int f, long int g, long int h, long int i,
	     long int j, long int k, long int l)
{
    if (e_output && e_fp)
	return fprintf (e_fp, cntl, a, b, c, d, e, f, g, h, i, j, k, l);
    return 0;
} /* eprintf */
