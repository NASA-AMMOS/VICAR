/* must_malloc.c
 * HISTORY
    22-Jul-92 (mwm)  Created BufferAlloc()
    22-Feb-92 (jdc)  Initial port to DOS: Tested for Borland C 3.0
	   Added #ifdef UNIX  for the UNIX dependent stuff
	   Added #ifdef MSDOS for the DOS  dependent stuff
    14-Mar-91 (mwm)  Created.
*/
#include <stdio.h>
#include <string.h>
#ifdef __unix__
#include <stdlib.h>
#ifdef __sgi__
#include <sys/select.h>
#endif
#include <unistd.h>		/* for exit(2) */
#else
#include <stdlib.h>		/* for malloc(3) */
#endif
#include "mwm.h"

extern char *this_program;

char *must_malloc (int bytes, char *desc)
{
    char *retval = NULL;

    retval = (char *) malloc ((unsigned) bytes);
    if (retval == NULL) {
	fprintf (stderr,
		"%s:  Failed to allocate %d bytes for %s, exiting\n",
		this_program, bytes, desc);
	exit (1);
    } /* if retval == NULL */

    return retval;
} /* must_malloc */



/************************************************************************
 *	BUFFERALLOC -- Allocate storage as needed; increase the		*
 *	existing buffer size if necessary.				*
 ************************************************************************/

void BufferAlloc (char **ptr, int elt_size, int *old_len, int len, char *what)
{
  if (what == NULL)
    what = "input";
  if (ptr == NULL || old_len == NULL) {
    fprintf (stderr, "%s [BufferAlloc]:  NULL %s pointer!!\n", this_program,
             what);
    exit (1);
  } /* if */
  if (*ptr == NULL || len > *old_len) {
    if (*ptr) free (*ptr);
    *old_len = len;
    *ptr = must_malloc (len * elt_size, what);
  } /* if */
} /* BufferAlloc */


/************************************************************************
 *	BUFFERLALLOC -- Allocate storage as needed; increase the	*
 *	existing (long) )buffer size if necessary.			*
 ************************************************************************/

void BufferLAlloc (char **ptr, int elt_size, long *old_len, long len,
		   char *what)
{
  if (what == NULL)
    what = "input";
  if (ptr == NULL || old_len == NULL) {
    fprintf (stderr, "%s [BufferLAlloc]:  NULL %s pointer!!\n", this_program,
             what);
    exit (1);
  } /* if */
  if (*ptr == NULL || len > *old_len) {
    if (*ptr) free (*ptr);
    *old_len = len;
    *ptr = must_malloc (len * elt_size, what);
  } /* if */
} /* BufferLAlloc */



/************************************************************************
 *	BUFFERREALLOC -- Allocate storage as needed; increase the	*
 *	existing buffer size if necessary.				*
 ************************************************************************/

void BufferRealloc (char **ptr, int elt_size, int *old_len, int len,
		    char *what)
{
  char *old;

  if (what == NULL)
    what = "input";
  if (ptr == NULL || old_len == NULL) {
    fprintf (stderr, "%s [BufferAlloc]:  NULL %s pointer!!\n", this_program,
             what);
    exit (1);
  } /* if */
  if (*ptr == NULL || len > *old_len) {
    old = *ptr;
    *ptr = must_malloc (len * elt_size, what);
    if (old) {
      strncpy (*ptr, old, *old_len * elt_size);
      free (old);
    } /* if */
    *old_len = len;
  } /* if */
} /* BufferRealloc */

