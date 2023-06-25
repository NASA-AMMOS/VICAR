#include <stdio.h>
#include "errors.h"
#define NO_MWM_VARS
#include "list_type.h"			/* includes mwm.h, declares fmultout */

/* list_type
	Program by:  Mark Maimone  1/26/86
	Last modified:  2/13/88

	The set of functions and macros defined below and in   list_type.h
   implement a list abstract data type.  Function names exactly correspond
   to similar data manipulation functions in LISP.

   HISTORY
	12-Jul-92 (mwm)  Fixed bug in append() on nil first arg, added
   unsafe_append() for speed.

	24-Nov-90 (mwm)  Modified to use StackMemoryT for use with the
   Miro' verifier.  So now you can free up chunks of memory.
   Unfortunately, I had to leave in all the external hooks to libmwm, like
   the error routine eprintf().  CHANGED THE SEMANTICS OF FREE_CONS to be
   those of st_free.  This is quite okay since free_cons wasn't used for
   anything in the first place.

	Summer-87 (mwm)  Added functions, enhanced   print_list,   updated
   the include file to identify parameter types

	Summer-86 (mwm)  Corrected minor bugs, developed parser, added levels
   to   print_list,   added new functions.

	12-May-86 (mwm)  Modified   print_list   to support arbitrary atoms,
   added   list_copy.

	26-Jan-86 (mwm)  Created.  Functions defined are   consp,
   cons_hlp, assoc_hlp, print_list.  Other definitions are contained in
   list_type.h.
*/



#if defined(__STDC__) || defined(__cplusplus)
#define prm(x) x
#else
#define prm(x) ()
#endif


#define BLOCKSIZE 2000

static int list_depth;
static boolean first_in_list, did_close_paren;
static StackMemoryT *stack = NULL;

void print_lhlp (FILE *outfp, list_type list, int item_num,
		 char *(*atomprint)
		 prm((list_type, int is_head, int depth, int num, FILE *)));
list_type new_cell prm((void));

/* new_cell -- returns a pointer to a memory location in reserved memory.
   Memory is allocated in   BLOCKSIZE   blocks, and values returned in
   sizeof(cell_type)   increments.  Uses the StackMemoryT abstract data type.

   PARAMETERS:
	none.

   GLOBALS:
	stack - where all cells are allocated
*/

list_type new_cell ()
{
    list_type retval = nil;
    union { char *s; list_type l; } u;

    if (stack == NULL)
	stack = st_init (BLOCKSIZE);

    u.s = st_alloc (&stack, sizeof (cell_type));
    retval = u.l;
    if (retval == NULL)
	retval = nil;

    return retval;
} /* new_cell */

/* consp -- returns true iff its argument is a cons cell.  The is true
   when   list   holds an integer value which is an address in the range
   of blocks allocated on the heap.

   PARAMETERS:
	elt - element to be tested

   GLOBALS:
	stack - where all cells are allocated
*/

boolean consp (list_type elt)
{
    return st_in (&stack, (char *) elt);
} /* consp */





/* cons_hlp -- creates a new cons cell and initializes it with   elt and
   list.

   PARAMETERS:
	elt - element to be inserted into the CAR of the new cell.
	list - element to be inserted into the CDR of the new cell.
*/

list_type cons_hlp (int *elt, int *list)
{
    list_type newc = new_cell ();

    newc -> car_elt = elt;
    newc -> cdr_elt = list;
    return newc;
} /* cons_hlp */

/* assoc_hlp -- given a   list   of sublists, look for the given   elt   as
   the CAR of each of the sublists, using   equality_function   to verify
   its presence.

   PARAMETERS:
	elt - element to match
	list - association list
	equality_function - pointer to a function which will compare   elt
   with a similarly-typed element.
*/

list_type assoc_hlp (list_type elt, list_type list,
		     boolean (*equality_function) (list_type, list_type))
{
    while (!atom (list)) {
	if (!atom (car (list))) {
	    if ((*equality_function) (elt, caar (list)))
		return car (list);
	} else

/* Next line assumes that all atoms are strings */

	    eprintf ("assoc_hlp: tried to take car of atom '%s'\n",
		    car (list));
	list = cdr (list);
    } /* while */

    return nil;
} /* assoc_hlp */

/* print_list -- output a list representation of the given   list   to
   given stream   outfp.

   PARAMETERS:
	outfp - output stream.
	list - list to be output.
	atomprint - pointer to a function to display atoms; must return a
		    string.
*/

void print_list (FILE *outfp, list_type list, 
		 char *(*atomprint)
		 prm((list_type, int is_head, int depth, int num, FILE *)))
{

    if (outfp == (FILE *) NULL)
	return;
    list_depth = 0;
    first_in_list = did_close_paren = FALSE;
    print_lhlp (outfp, list, 1, atomprint);
    (void) putc ('\n', outfp);
} /* print_list */





void print_lhlp (FILE *outfp, list_type list, int item_num,
		 char *(*atomprint)
		 prm((list_type, int is_head, int depth, int num, FILE *)))
     /* int item_num;  Item number; 1=car, 2=cadr, 3=caddr, etc */
{
    boolean is_head = TRUE;

    if (outfp == (FILE *) NULL)
	return;

    if (null (list))
	fprintf (outfp, "nil");
    else if (atom (list)) {
	fprintf (outfp, "%s\n", (*atomprint) (list, FALSE, list_depth,
			item_num, outfp));
	did_close_paren = first_in_list = FALSE;
    } else {

	list_depth++;
	if (did_close_paren) {
	    (void) putc ('\n', outfp);
	    fmultout (outfp, list_depth, (char) ' ');
	} /* if */

	if (!first_in_list)
	    (void) putc (' ', outfp);

	first_in_list = TRUE;
	did_close_paren = FALSE;

/*   list   has some elements, list each between parenthesis with spaces
   separating elements */

	(void) putc ('(', outfp);
	while (!null (list)) {
	    if (listp (car (list)))
		print_lhlp (outfp, car (list), 1, atomprint);
	    else
		fprintf (outfp, "%s", (*atomprint) (car (list), is_head,
				list_depth, item_num, outfp));

	    item_num++;
	    list = cdr (list);
	    is_head = FALSE;

/* Write a space only if there are more elements in the list */

	    if (!null (list))
		(void) putc (' ', outfp);

	    if (!listp (list)) {
		fprintf (outfp, ". %s", (*atomprint) (list, FALSE, list_depth,
				item_num, outfp));
		break;
	    } /* if */
	} /* while */
	(void) putc (')', outfp);

	did_close_paren = TRUE;
	first_in_list = FALSE;
	list_depth--;
    } /* else */
} /* print_lhlp */



list_type list_copy (list_type lis)
{
    if (atom (lis))
	return lis;

    return cons (list_copy (car (lis)), list_copy (cdr (lis)));
} /* list_copy */


int list_equal (list_type l1, list_type l2)
{
    if (null (l1))	return null (l2);
    if (null (l2))	return 0;
    if (atom (l1))	return atom (l2) && l1 == l2;
    if (atom (l2))	return 0;
    return list_equal (car (l1), car (l2)) && list_equal (cdr (l2), cdr (l2));
} /* list_equal */



#define TACK_ON(x) if (null (retval)) retval = tail = cons ((x), nil); \
	else { rplacd (tail, cons ((x), nil)); tail = cdr (tail); }

list_type flatten (list_type lis)
{
    list_type retval = nil, tail = nil;

    for (; !atom (lis); lis = cdr (lis)) {
	if (atom (car (lis))) {
	    TACK_ON (car (lis));
	} else {
	    list_type res = flatten (car (lis));

	    if (null (retval))
		retval = tail = res;
	    else
		rplacd (tail, res);
	    tail = last (tail);
	} /* else */
    } /* for */

    if (!null (lis)) {
	TACK_ON (lis);
    } /* if */

    return retval;
} /* flatten */




list_type rplaca (list_type cons_cell, list_type new_car)
{
    if (!consp (cons_cell))
	eprintf ("rplaca:  tried to replace non-cons cell '%ld'\n",
		(long int) cons_cell);
    else
	cons_cell -> car_elt = (int *) new_car;

    return cons_cell;
} /* rplaca */




list_type rplacd (list_type cons_cell, list_type new_cdr)
{
    if (!consp (cons_cell))
	eprintf ("rplacd:  tried to replace non-cons cell '%ld'\n",
		(long int) cons_cell);
    else
	cons_cell -> cdr_elt = (int *) new_cdr;

    return cons_cell;
} /* rplacd */



void free_cons (list_type cons_cell)
{
    if (!consp (cons_cell))
	eprintf ("free_cons:  tried to free non-cons cell '%ld'\n",
		(long int) cons_cell);
    else if (st_free (&stack, (char *) cons_cell) == 0)
	eprintf ("free_cons:  call to st_free failed on %x\n", cons_cell);
} /* free_cons */



void free_list (list_type lis)
{
    if (null (lis))
	eprintf ("free_list:  tried to free NIL\n");
    else if (atom (lis))
	eprintf ("free_list:  fried to free non-NIL atom '%ld'\n", (long) lis);
    else {
	if (consp (car (lis)))
	    free_list (car (lis));
	if (consp (cdr (lis)))
	    free_list (cdr (lis));
	free_cons (lis);
    } /* else */
} /* free_list */



list_type last (list_type lis)
{
    if (null (lis))
	eprintf ("last:  null list parameter\n");
    else if (atom (lis))
	eprintf ("last:  argument '%ld' is a non-NIL atom\n", (long) lis);
    else
	while (consp (cdr (lis)))
	    lis = cdr (lis);

    return lis;
} /* last */



int length (list_type lis)
{
    int count;

    if (!listp (lis)) {
	eprintf ("length:  argument '%ld' is not a list\n", (long int) lis);
	count = -1;
    } else
	for (count = 0; !atom (lis); count++)
	    lis = cdr (lis);

    return count;
} /* length */



list_type reverse (list_type lis)
{
    list_type new_lis;

    if (!listp (lis) || (!null (lis) && !null (cdr (last (lis))))) {
	eprintf ("reverse:  argument '%ld' is not a nil-terminated list\n",
		(long int) lis);
	new_lis = lis;
    } else
	for (new_lis = nil; !atom (lis); lis = cdr (lis))
	    new_lis = cons (car (lis), new_lis);

    return new_lis;
} /* reverse */



list_type subst (list_type new_expr, list_type old_atom, list_type expr,
		 boolean (*equality_function) (list_type, list_type))
{
    list_type ret_val = expr;

    if (!atom (old_atom)) {
	eprintf ("subst:  argument 2 is not an atom '%ld'\n", (long) old_atom);
	ret_val = expr;

    } else if (atom (expr))
	if ((*equality_function) (old_atom, expr))
	    ret_val = new_expr;

	else
	    ret_val = expr;

    else
	ret_val = cons (subst (new_expr, old_atom, car (expr),
					 equality_function),
			subst (new_expr, old_atom, cdr (expr),
					 equality_function));

    return ret_val;
} /* subst */



list_type pairlis (list_type lis1, list_type lis2)
{
    if (null (lis1) || null (lis2))
	return nil;

    return cons (cons (car (lis1), car (lis2)), pairlis (cdr (lis1), cdr (lis2)));
} /* pairlis */



list_type member (list_type exp, list_type lis,
		  boolean (*equality_function) (list_type, list_type))
{
    for (; !atom (lis); lis = cdr (lis))
	if ((*equality_function) (exp, car (lis)))
	    return lis;

    return nil;
} /* member */



list_type append (list_type l1, list_type l2)
{
    list_type ret_val = list_copy (l1);

    if (consp (ret_val))
	rplacd (last (ret_val), l2);
    else
	ret_val = l2;

    return ret_val;
} /* append */



list_type unsafe_append (list_type l1, list_type l2)
{
    list_type ret_val  = nil;
    list_type tail = nil;

    for (; !null (l1); l1 = CDR (l1))
	if (null (ret_val))
	    tail = ret_val = cons (CAR (l1), nil);
	else {
	    rplacd (tail, cons (CAR (l1), nil));
	    tail = CDR (tail);
	} /* else */

    if (!null (tail))
	rplacd (tail, l2);
    else
	ret_val = l2;

    return ret_val;
} /* unsafe_append */


list_type nreverse (list_type lis)
{
    list_type prev, tmp;

    prev = nil;
    if (atom (lis))
	prev = lis;

    while (!atom (lis)) {
	tmp = lis;
	lis = cdr (lis);
	rplacd (tmp, prev);
	prev = tmp;
    } /* while */

    return prev;
} /* nreverse */
