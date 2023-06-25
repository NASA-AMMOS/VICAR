#include <stdio.h>
#include <stdlib.h>		/* atoi */
#include <ctype.h>		/* for isdigit, isalpha, isspace */
#include <stdint.h>

#include <string.h>

#include "errors.h"
#include "range.h"

#define MAX_IN_PAIR 30
#define BAD_STR "**"
#define UNDEF_INDEX_VALUE -12345


/* HISTORY
	2-Sep-92  Mark Maimone (mwm@cmu.edu)  Changed range2s to output
	   "by#" when appropriate.

	1-Sep-92  Mark Maimone (mwm@cmu.edu)  Added range_size(), changed
	   the s2range grammar to allow "by#".

	11-Aug-92  Mark Maimone (mwm@cmu.edu)  Created.
*/

typedef struct {
    char *s;
    int i;
} IndexT;

static char *to_chars = "-:|.";	/* Would be nice to allow "to" as well */
static char *sep_chars = ",; \t";
static char *open_chars = "[({";
static char *close_chars = "])}";

static IndexT numerals[] = {
/* English */
    {"zero", 0}, {"one", 1}, {"two", 2}, {"three", 3}, {"four", 4}, {"five", 5},
    {"six", 6}, {"seven", 7}, {"eight", 8}, {"nine", 9}, {"ten", 10},
    {"eleven", 11}, {"twelve", 12}, {"thirteen", 13}, {"fourteen", 14},
    {"fifteen", 15}, {"sixteen", 16}, {"seventeen", 17}, {"eighteen", 18},
    {"nineteen", 19}, {"twenty", 20}, {"thirty", 30}, {"forty", 40},
    {"fifty", 50}, {"sixty", 60}, {"seventy", 70}, {"eighty", 80},
    {"ninety", 90},
/* Spanish */
    {"zero", 0}, {"uno", 1}, {"dos", 2}, {"tres", 3}, {"cuatro", 4},
    {"cinco", 5}, {"seis", 6}, {"siete", 7}, {"ocho",8},  {"nueve", 9},
    {"diez", 10}, {"once", 11}, {"doce", 12}, {"trece", 13}, {"catorce", 14},
    {"quince", 15}, {"diesiseis", 16}, {"diesisiete", 17}, {"diesiocho", 18},
    {"diesinueve", 19}, {"veinte", 20}, {"treinta", 30}, {"cuarenta", 40},
    {"cincuenta", 50}, {"sesenta", 60}, {"setenta", 70}, {"ochenta", 80},
    {"noventa", 90},
/* Tagalog */
    {"isa", 1}, {"dalawa", 2}, {"tatlo", 3}, {"apat", 4}, {"lima", 5},
    {"anim", 6}, {"pito", 7}, {"walo", 8}, {"siyam", 9}, {"sampu", 10},
    {"labingisa", 11}, {"labingdalawa", 12}, {"labingtatlo", 13},
    {"labingapat", 14}, {"labinglima", 15}, {"labinganim", 16},
    {"labingpito", 17}, {"labingwalo", 18}, {"labingsiyam", 19},
    {"dalawampu", 20},
/* French */
    {"un", 1}, {"deux", 2}, {"trois", 3}, {"quatre", 4}, {"cinq", 5},
    {"six", 6}, {"sept", 7}, {"huit", 8}, {"neuf", 9}, {"dix", 10},
    {"onze", 11}, {"douze", 12}, {"treize", 13}, {"quatorze", 14},
    {"quinze", 15}, {"seize", 16}, {"dix-sept", 17}, {"dix-huit", 18},
    {"dix-neuf", 19}, {"vingt", 20},
    { 0, 0}
}; /* numerals */

typedef union {
    list_type l;
    int i;
} Both;

extern char *this_program;

/* dflt_char_s2index -- returns number of chars parsed; 0 implies no
   parse, negative implies legal but out of range.  */

int dflt_char_s2index (char *str, int mini, int maxi, int *result)
{
    unsigned char c;
    int chars;

/* Since fmt2c happily accepts \0 we've got to filter it out here */

    if (str == NULL || *str == '\0') return 0;
    if (mini > maxi) return 0;
    chars = fmt2c (str, (char *) &c);
    if (mini < 0) mini = 0;
    if (maxi > 255) maxi = 255;
    if (c < mini) {
	c = mini;
	chars = -chars;
    } else if (c > maxi) {
	c = maxi;
	chars = -chars;
    } /* else if */
    if (result) *result = c;
    return chars;
} /* dflt_char_s2index */

/* dflt_s2index -- returns number of chars parsed; 0 implies no parse,
   negative implies legal, but out of range. */

int dflt_s2index (char *str, int mini, int maxi, int *result)
{
    int index = -1;
    int chars = 0;
    char *ptr;

    if (str == NULL) return 0;
    if (isdigit (*str) || (*str == '-' && isdigit (str[1]))) {
	char c;
	for (ptr = str + 1; isdigit (*ptr); ptr++);
	c = *ptr;
	*ptr = '\0';
	index = atoi (str);
	chars = ptr - str;
	*ptr = c;
    } else {
	int i;

	for (i = 0; numerals[i].s; i++)
	    if (cistrncmp (numerals[i].s, str, strlen (numerals[i].s)) == 0) {
		index = numerals[i].i;
		chars = strlen (numerals[i].s);
		break;
	    } /* if */

/* Last Resort, try the letters a-z */

	if (chars == 0) {
	    if (!isalpha (*str) || isalpha (str[1]))
		return 0;
	    index = mklow (*str) - 'a';
	    chars = 1;
	} /* if chars == 0 */
    } /* else */

    if (index < mini || index > maxi) chars = -chars;

    if (result)
	*result = index;
    return chars;
} /* dflt_s2index */

RangeT *mk_range (int i1, int i2)
{
    union { char *s; RangeT *r; } u;
    RangeT *result;
    Both l1, l2;

    u.s = must_malloc (sizeof(RangeT), "Range template");
    result = u.r;

    l1.i = i1;
    l2.i = i2;
    if (i1 <= i2)
	result->l = cons (cons (l1.l, l2.l), nil);
    else
	result->l = nil;

    return result;
} /* mk_range */



char *range2s (RangeT *r, char *buf, char *(*index2s) (int, char *))
{
    static char *store = NULL;
    static int storelen = 0;
    list_type lis;
    char *ptr;

/* Check for NULL range (other empty ranges are handled below) */

    if (r == NULL) {
	static char incase[10];
	if (buf == NULL) buf = incase;
	sprintf (buf, "%c%c", *open_chars, *close_chars);
	return buf;
    } /* if */

/* Make sure there's enough room.  Assumes that each pair will take no
   more than MAX_IN_PAIR bytes */

    if (buf == NULL && length (r->l) * MAX_IN_PAIR + 10 > storelen) {
	if (store) free(store);
	storelen = length (r->l) * MAX_IN_PAIR + 10;
	store = must_malloc (storelen, "range2s buffer");
    } /* if */
    if (buf == NULL) buf = store;
    ptr = buf;
    *ptr++ = *open_chars;
    for (lis = r->l; !atom (lis); lis = cdr (lis)) {
	Both l, h;

	if (lis != r->l)
	    *ptr++ = *sep_chars;
	if (atom (car (lis))) {
	    sprintf (ptr, "%s", BAD_STR), ptr += strlen (ptr);
	} else {

/* Always write the low number of the pair */

	    l.l = caar (lis); h.l = cdar (lis);
	    (void) (*index2s) (l.i, ptr), ptr += strlen (ptr);
	    if (l.i != h.i) {
		*ptr++ = *to_chars;
		(void) (*index2s) (h.i, ptr), ptr += strlen (ptr);
	    } else {

/* Look for periodic sequences */

		int diff = 0, count = 0, islast = l.i, skip = 1;
		Both l1, h1;
		list_type next = lis, prev = lis;

/* Loop over all pairs with the same top and bottom elts.   diff   holds
   the change between first and second pairs.  Process all following pairs
   that have the same difference, keeping the total number in   count   */

		while (!atom (cdr (next))) {
		    count++;
		    prev = next;
		    next = cdr (next);
		    if (atom (car (next))) {
			count = 0;	/* Bad range format */
			break;
		    } /* if */
		    l1.l = caar (next); h1.l = cdar (next);
		    if (l1.i != h1.i) {	/* not a singleton range */
			skip = 0;
			break;
		    } /* if */
		    if (count == 1)	/* init   diff   1st time through */
			diff = l1.i - l.i;
		    else if (l1.i - islast != diff) {
			skip = 0;
			break;
		    } /* else if */
		    islast = l1.i;
		} /* while */

/* Only group pairs together if there are at least 3 similar increments */

		if (count > 2) {
		    *ptr++ = *to_chars;
		    (void) (*index2s) (islast, ptr), ptr += strlen (ptr);
		    *ptr++ = 'b'; *ptr++ = 'y';
		    (void) (*index2s) (diff, ptr), ptr += strlen (ptr);
		    lis = skip ? next : prev;
		} /* if */
	    } /* else */
	} /* else */
    } /* for */
    *ptr++ = *close_chars;
    *ptr = '\0';
    return buf;
} /* range2s */

static Both b1_tmp, b2_tmp;

#define low_below_low(l1,l2) (b1_tmp.l = caar (l1), b2_tmp.l = caar (l2), \
	(b1_tmp.i < b2_tmp.i) ? 1 : 0)
#define LWHICH (which ? l1 : l2)
#define LASGN(x) (which ? (l1 = (x)) : (l2 = (x)))
#define HWHICH (which ? l2 : l1)
#define HASGN(x) (which ? (l2 = (x)) : (l1 = (x)))
#define high_below_low(l1,l2) (b1_tmp.l = cdar (l1), b2_tmp.l = caar (l2), \
	(b1_tmp.i < b2_tmp.i) ? 1 : 0)
#define high_below_high(l1,l2) (b1_tmp.l = cdar (l1), b2_tmp.l = cdar (l2), \
	(b1_tmp.i < b2_tmp.i) ? 1 : 0)
#define APP(x) (null (lis) ? (tail = lis = cons ((x), nil)) : \
	(rplacd (tail, cons ((x), nil)), tail = cdr (tail)))
#define high_eq_low_less_1(l1,l2) (!atom (car (l1)) && !atom (car (l2)) && \
	(b1_tmp.l = cdar (l1), b2_tmp.l = caar (l2), b1_tmp.i+1 == b2_tmp.i))

static list_type compress_range (list_type in)
{
    list_type prev, lis = in;

    if (atom (lis)) return nil;
    for (prev = lis, lis = cdr (lis); !atom (lis); lis = cdr (lis)) {
	if (high_eq_low_less_1 (prev, lis)) {
	    rplacd (car (prev), cdar (lis));
/*	    free_cons (car (lis));*/
	} else {
	    rplacd (prev, lis);
	    prev = lis;
	} /* else */
    } /* for */
    rplacd (prev, nil);
    return in;
} /* compress_range */

RangeT *range_merge (RangeT *r1, RangeT *r2)
{
    RangeT *result;
    list_type lis, tail, l1, l2;
    union { char *s; RangeT *r; } u;

    if (r1 == NULL) return r2;
    if (r2 == NULL) return r1;
    tail = lis = nil;
    l1 = list_copy (r1->l);
    l2 = list_copy (r2->l);

    while (!null (l1) && !null (l2)) {
	int which;

	if (atom (CAR (l1)) || atom (CAR (l2))) {
	    fprintf (stderr,
		    "%s [range_merge]:  Bad list format! %lx,%lx\n",
		    this_program, (uintptr_t) car (l1),
		    (uintptr_t) car (l2));
	    break;
	} /* if */

	which = low_below_low (l1, l2);

	if (high_below_low (LWHICH, HWHICH)) {
	    APP (CAR (LWHICH));
	    LASGN (CDR (LWHICH));
	} else if (high_below_high (LWHICH, HWHICH)) {
	    APP (cons (CAAR (LWHICH), cdar (HWHICH)));
	    l1 = CDR (l1);
	    l2 = CDR (l2);
	} else
	    HASGN (CDR (HWHICH));
    } /* while */
    if (!atom (l1)) {
	if (atom (lis))
	    lis = l1;
	else
	    rplacd (tail, l1);
    } else if (!atom (l2)) {
	if (atom (lis))
	    lis = l2;
	else
	    rplacd (tail, l2);
    } /* else if */
    
    u.s = must_malloc (sizeof (RangeT), "Merged Range Buffer");
    result = u.r;
    result->l = compress_range (lis);
    return result;
} /* range_merge */


RangeT *range_intersect (RangeT *r1, RangeT *r2)
{
    RangeT *result;
    list_type lis, tail, l1, l2;
    union { char *s; RangeT *r; } u;


    if (r1 == NULL || r2 == NULL) return NULL;
    tail = lis = nil;
    l1 = list_copy (r1->l);
    l2 = list_copy (r2->l);

    while (!atom (l1) && !atom (l2)) {
	int which;

	if (atom (car (l1)) || atom (car (l2))) {
	    fprintf (stderr,
		     "%s [range_intersect]:  Bad list format! %lx,%lx\n",
		     this_program, (uintptr_t) car (l1),
		     (uintptr_t) car (l2));
	    break;
	} /* if */

	which = low_below_low (l1, l2);

	if (high_below_low (LWHICH, HWHICH)) {
	    LASGN (cdr (LWHICH));
	} else if (high_below_high (LWHICH, HWHICH)) {
	    APP (cons (CAAR (HWHICH), cdar (LWHICH)));
	    l1 = cdr (l1);
	    l2 = cdr (l2);
	} else {
	    APP (car (HWHICH));
	    HASGN (cdr (HWHICH));
	} /* else */
    } /* while */
    u.s = must_malloc (sizeof (RangeT), "Intersected Range");
    result = u.r;
    result->l = compress_range (lis);
    return result;
} /* range_intersect */


RangeT *range_diff (RangeT *r1, RangeT *r2)
{
    RangeT *result;
    list_type lis, tail, l1, l2;
    union { char *s; RangeT *r; } u;

    if (r1 == NULL) return NULL;
    if (r2 == NULL) return r1;
    tail = lis = nil;
    l1 = list_copy (r1->l);
    l2 = list_copy (r2->l);

    while (!atom (l1) && !atom (l2)) {

	if (atom (car (l1)) || atom (car (l2))) {
	    fprintf (stderr,
		     "%s [range_diff]:  Bad list format! %lx,%lx\n",
		     this_program, (uintptr_t) car (l1),
		     (uintptr_t) car (l2));
	    break;
	} /* if */

	if (low_below_low (l1, l2)) {
	    if (high_below_low (l1, l2)) {
		APP (car (l1));
		l1 = cdr (l1);
	    } else if (high_below_high (l1, l2)) {
		Both less1;

		less1.l = caar (l2);
		less1.i--;
		APP (cons (CAAR (l1), less1.l));
		l1 = cdr (l1);
		/* Keep l2 around in case it overlaps again */
	    } else {
		Both less1;

		less1.l = caar (l2);
		less1.i--;
		APP (cons (CAAR (l1), less1.l));
		less1.l = cdar (l2);
		less1.i++;
		l1 = cons (cons (less1.l, cdar (l1)), cdr (l1));
		l2 = cdr (l2);
	    } /* else */
	} else {
	    if (high_below_low (l2, l1))
		l2 = cdr (l2);
	    else if (high_below_high (l1, l2))
		l1 = cdr (l1);
	    else {
		Both less1;

		less1.l = cdar (l2);
		less1.i++;
		l1 = cons (cons (less1.l, cdar (l1)), cdr (l1));
		l2 = cdr (l2);
	    } /* else */
	} /* else */
    } /* while */
    if (!atom (l1)) {
	if (!atom (tail))
	    rplacd (tail, l1);
	else
	    lis = l1;
    } /* if */

    u.s = must_malloc (sizeof (RangeT), "Merged Range Buffer");
    result = u.r;
    result->l = compress_range (lis);
    return result;
} /* range_diff */

/* free_range -- returns TRUE on success */

int free_range (RangeT *r)
{
    if (r) {
	free_list (r->l);
	free (r);
    } /* if */
    return 1;
} /* free_range */



#define SKIP_WHITE(s) while (isspace (*s)) s++



/* s2range -- returns # chars parsed; 0 implies no parse */

int s2range (char *str, int (*s2index) (char *, int, int, int *),
	     int low, int hi, RangeT **result)
{
    RangeT *retval = NULL;
    int i1, i2;
    int cnt, open = -1;
    char *start = str;
    char *found_open = NULL;

    if (s2index == NULL || str == NULL) return 0;
    if (low > hi) {
	fprintf (stderr, "%s [s2range]:  Empty range spec!  %d > %d\n",
		this_program, low, hi);
	return 0;
    } /* if */

    SKIP_WHITE (str);
    if (*str && (found_open = strchr (open_chars, *str))) {
	str++;
	open = found_open - open_chars;
    }
    SKIP_WHITE (str);
    while ((cnt = (*s2index) (str, low, hi, &i1)) != 0 ||
	    (*str && strchr (to_chars, *str))) {
	if (cnt > 0)
	    str += cnt;
	else
	    i1 = low;
	i2 = i1;
	SKIP_WHITE (str);

/* This "if" needs the stuff after the "||" to handle the case where
   whitespace is also in "to_chars" */

	if (*str && (strchr (to_chars, *str) || (str > start &&
		strchr (to_chars, str[-1])))) {
	    str++;
	    SKIP_WHITE (str);
	    if ((cnt = (*s2index) (str, low, hi, &i2)) <= 0)
		i2 = hi;
	    else
		str += cnt;
	} else if (cnt < 0)
	    break;

	SKIP_WHITE (str);
	if (mklow (*str) == 'b' && mklow (str[1]) == 'y') {
	    int inc;

	    str += 2;
	    SKIP_WHITE (str);
	    if ((cnt = (*s2index) (str, low, hi, &inc)) < 0)
		break;
	    if (cnt == 0)
		str -= 2;
	    else {
		str += cnt;
		if ((i1 > i2 && inc > 0) || (i2 > i1 && inc < 0))
		    inc = (-inc);
/* Zero increment makes no sense, so ignore it */

		if (inc) {

/* range_merge appends to the end of a list, so always call it from the
   highest int down to the lowest */

		    if (inc > 0) {
			int end;	/* True top end of range */
			for (end = i1; end <= i2; end += inc);
			i2 = i1;
			i1 = end - inc;
			inc = -inc;
		    } /* if inc > 0 */
		    for (; i1 >= i2; i1 += inc)
			retval = range_merge (retval, mk_range (i1, i1));
		} /* if (inc) */
		i1 = i2 + 1;
		/* Since i1 > i2, the following range_merge is a no-op */
	    } /* else */
	} /* if */
	retval = range_merge (retval, mk_range (i1, i2));

/* I think that I can remove the restriction that there *must* be a
   separator between range elts if I just comment out the next IF
   statement.  Ideally, this would be externally controllable, so that
   character parsing need *not* require it, but other ranges (especially
   integer ranges) would. -- 21 Oct 92 mwm */

	if (*str && (strchr (sep_chars, *str) == NULL && (str == start ||
		strchr (sep_chars, str[-1]) == NULL)))
	    break;
	while (*str && (isspace (*str) || strchr (sep_chars, *str)))
	    str++;
    } /* while */
    if (open >= 0) {
	if (*str != close_chars[open])
	    return 0;
	str++;
    } /* if */
    if (result) *result = retval;

    return str - start;
} /* s2range */


int range_low (RangeT *r)
{
    Both v;
    if (r && !range_empty (r) && consp (r->l) && consp (car (r->l))) {
    	v.l = caar (r->l);
	return v.i;
    } /* if */
    return UNDEF_INDEX_VALUE;
} /* range_low */



int range_high (RangeT *r)
{
    Both v;
    if (r && !range_empty (r) && consp (r->l) && consp (car (r->l))) {
    	v.l = cdar (last (r->l));
	return v.i;
    } /* if */
    return UNDEF_INDEX_VALUE;
} /* range_high */


int range_equal (RangeT *r1, RangeT *r2)
{
    if (range_empty (r1))
	return range_empty (r2);
    if (range_empty (r2)) return 0;
    return list_equal (r1->l, r2->l);
} /* range_equal */


int range_subset (RangeT *r1, RangeT *r2)
{
    return range_equal (r2, range_merge (r1, r2));
} /* range_subset */



int range_size (RangeT *r)
{
    Both lo, hi;
    int sum = 0;
    list_type l;

    if (range_empty (r))
	return 0;
    for (l = r->l; !atom (l); l = cdr (l)) {
	if (atom (car (l)))
	    return 0;		/* Illegal range! */
	lo.l = caar (l);
	hi.l = cdar (l);
	sum += (hi.i - lo.i) + 1;
    } /* for */
    return sum;
} /* range_size */
