#include <stdio.h>
#include <stdint.h>
#ifdef __unix__
#  include <stdlib.h>
#else
#  include <stdlib.h>		/* for malloc(3) */
#endif
#define NO_MWM_VARS
#include "mwm.h"

extern char *this_program;
static void free_st_chain (StackMemoryT *mem);

/* Stack Memory Abstract Data Type

	-- Implements stack memory using a non-circular, doubly linked list
	-- Can allocate any amount of memory, but the largest expected
	      block is the input parameter   block_len.   Larger blocks
	      will be allocated if needed, but a warning message will be
	      output.
*/

StackMemoryT *st_init (int block_len)
{
    StackMemoryT *mem;

    mem = (StackMemoryT *) malloc (sizeof (StackMemoryT));
    if (mem == NULL) {
	fprintf (stderr,
	    "st_init:  Couldn't allocate stack memory table pointer\n");
	return NULL;
    } /* if mem == NULL */
    mem -> prev = NULL;
    mem -> next = NULL;
    mem -> block_len = block_len;
    if ((mem -> start = (char *) malloc ((unsigned) block_len)) == NULL) {
	fprintf (stderr, "st_init:  Couldn't allocate table with %d bytes\n",
		block_len);
	return NULL;
    } /* if mem -> start == NULL */
    mem -> top = mem -> start;
    return mem;
} /* st_init */



char *st_alloc (StackMemoryT **memptr, int bytes)
{
    char *retval;
    int new_block_len;
    StackMemoryT *mem;

    if (memptr == NULL || *memptr == NULL) {
	fprintf (stderr, "st_alloc:  NULL StackMemory pointer\n");
	return NULL;
    } /* if mem == NULL */
    mem = *memptr;

/* Need to round off to a word size */

    bytes = ((bytes + sizeof (long) - 1) / sizeof (long)) * sizeof (long);
    if (bytes > (new_block_len = mem -> block_len)) {
	fprintf (stderr,
		"st_alloc:  WARNING, block length increasing from %d to %d\n",
		mem -> block_len, bytes);
	new_block_len = bytes;
    } /* if bytes > mem -> block_len */

    if (bytes + (mem -> top - mem -> start) > mem -> block_len) {
	if (mem -> next == NULL || bytes > mem -> next -> block_len) {
	    free_st_chain (mem -> next);  /* if the already-allocated
					     block isn't big enough, lose
					     it and its successors */
	    mem -> next = st_init (new_block_len);
	    if (mem -> next == NULL) {
		fprintf (stderr,
			"st_alloc:  out of memory, no room for %d bytes\n",
			bytes);
		return NULL;
	    } /* if mem -> next == NULL */
	    mem -> next -> prev = mem;
	} /* if mem -> next == NULL || */
	mem = mem -> next;
	mem -> top = mem -> start;
    } /* if bytes + top - start > block_len */

    retval = mem -> top;
    mem -> top += bytes;
    *memptr = mem;
    return retval;
} /* st_alloc */


/* st_in -- check to see if a memory address lies within the stack memory.
   */

int st_in (StackMemoryT **memptr, char *loc)
{
    StackMemoryT *mem;

    if (memptr == NULL) {
	fprintf (stderr, "st_in: NULL StackMemory pointer, loc=%lx\n",
		 (uintptr_t) loc);
	return 0;
    } /* if memptr == NULL */

    for (mem = *memptr; mem; mem = mem -> prev)
	if (mem -> start <= loc && loc < mem -> top)
	    return 1;
    return 0;
} /* st_in */


/* st_free -- free up all stack frames above a certain address.  Note that
   only the current pointer gets adjusted, no memory is actually
   deallocated.  We do this to prevent thrashing on malloc calls.  If
   you need to conserve memory, add a call to free_st_chain as indicated.
   */

int st_free (StackMemoryT **memptr, char *loc)
{
    StackMemoryT *mem;

    if (memptr == NULL) {
      fprintf (stderr, "st_free: NULL StackMemory pointer, loc=%lx\n",
		 (uintptr_t) loc);
	return 0;
    } /* if memptr == NULL */

    for (mem = *memptr; mem; mem = mem -> prev)
	if (mem -> start <= loc && mem -> start + mem -> block_len > loc) {
	    *memptr = mem;
	    mem -> top = loc;

/* DO NOT ADD THIS LINE, unless memory starts getting *really* tight. */
/*	    free_st_chain (mem -> next); mem -> next = NULL; */

	    return 1;
	} /* if mem -> start <= loc */
    return 0;
} /* st_free */



/* free_st_chain -- frees all stack frames from the current one to the
   end, following   next   pointers. */

static void free_st_chain (StackMemoryT *mem)
{
    if (mem != NULL) {
	while (mem -> next != NULL) {
	    free ((char *) (mem -> start));
	    mem = mem -> next;
	    free ((char *) (mem -> prev));
	} /* while mem != NULL */

	free ((char *) mem);
    } /* if mem != NULL */
} /* free_st_chain */



