#ifndef LIST_TYPE
#define LIST_TYPE

#include "mwm.h"		    /* need the   boolean   data type */

#define nil ((list_type) -1024)     /* Both   nil and PASTNIL   must be
				       distinct from ALL other atoms */
#define PASTNIL ((list_type) -1023)


#define assoc(x,y,z) assoc_hlp ((list_type) (x), (list_type) (y), (z))
#define atom(x) (!consp (x))
#define car(x) (!atom (_DUMMY = (x)) ? (list_type) _DUMMY -> car_elt : \
	(eprintf (_car_error, _DUMMY), PASTNIL))
#define cdr(x) (!atom (_DUMMY = (x)) ? (list_type) _DUMMY -> cdr_elt : \
	(eprintf (_cdr_error, _DUMMY), PASTNIL))
#define cons(x,y) cons_hlp ((int *) (x), (int *) (y))
#define lst_delete(val,lis,fcn) delete_help ((list_type) (val), &(lis), (fcn))
#define insert(val,lis,fcn) insert_help ((list_type) (val), &(lis), (fcn))
#define listp(x) (null (_DUMMY = (x)) || consp (_DUMMY))
#define null(x) (((_DUMMY = (x)) == nil) || (_DUMMY == PASTNIL))
#define printf_list print_list		/* I've made the mistake too often */
#define CAR(x) ((list_type)(((cell_type *)(x))->car_elt)) /* UNSAFE car!!! */
#define CDR(x) ((list_type)(((cell_type *)(x))->cdr_elt)) /* UNSAFE cdr!!! */
#define CAAR(x) CAR (CAR (x))
#define CDAR(x) CDR (CAR (x))

#define caar(x) car (car (x))
#define cadr(x) car (cdr (x))
#define cdar(x) cdr (car (x))
#define cddr(x) cdr (cdr (x))
#define caaar(x) car (car (car (x)))
#define caadr(x) car (car (cdr (x)))
#define cadar(x) car (cdr (car (x)))
#define caddr(x) car (cdr (cdr (x)))
#define cdaar(x) cdr (car (car (x)))
#define cdadr(x) cdr (car (cdr (x)))
#define cddar(x) cdr (cdr (car (x)))
#define cdddr(x) cdr (cdr (cdr (x)))
#define caaaar(x) car (car (car (car (x))))
#define caaadr(x) car (car (car (cdr (x))))
#define caadar(x) car (car (cdr (car (x))))
#define caaddr(x) car (car (cdr (cdr (x))))
#define cadaar(x) car (cdr (car (car (x))))
#define cadadr(x) car (cdr (car (cdr (x))))
#define caddar(x) car (cdr (cdr (car (x))))
#define cadddr(x) car (cdr (cdr (cdr (x))))
#define cdaaar(x) cdr (car (car (car (x))))
#define cdaadr(x) cdr (car (car (cdr (x))))
#define cdadar(x) cdr (car (cdr (car (x))))
#define cdaddr(x) cdr (car (cdr (cdr (x))))
#define cddaar(x) cdr (cdr (car (car (x))))
#define cddadr(x) cdr (cdr (car (cdr (x))))
#define cdddar(x) cdr (cdr (cdr (car (x))))
#define cddddr(x) cdr (cdr (cdr (cdr (x))))

typedef struct {
    int *car_elt, *cdr_elt;
} cell_type;

typedef cell_type *list_type;

#ifdef __STDC__
#ifndef FILE
#include <stdio.h>
#endif
#define prm(x) x
#else
#define prm(x) ()
#endif

#ifdef __cplusplus
    extern "C" {
#endif

void print_list prm((FILE *fp, list_type lis, char *(*atomprint) (list_type \
	lis, int is_head, int list_depth, int item_num, FILE *fp)));
void free_cons prm((list_type cell));
void free_list prm((list_type lis));
boolean consp prm((list_type lis));
int length prm((list_type lis));
list_type append prm((list_type left, list_type right));
list_type unsafe_append prm((list_type left, list_type right));
list_type assoc_hlp prm((list_type elt, list_type lis, boolean (*eq) (list_type, list_type)));
list_type caxr prm((list_type lis, int n));	/* NOT YET AVAILABLE */
list_type cdxr prm((list_type lis, int n));	/* NOT YET AVAILABLE */
list_type cons_hlp prm((int *left, int *right));
void delete_help prm((list_type elt, list_type *lis, boolean (*eq) (void))); /* NYA */
list_type flatten prm((list_type lis));	/* NYA */
void insert_help prm((list_type elt, list_type *lis, boolean (*eq) (void))); /* NYA */
list_type last prm((list_type lis));
list_type list_copy prm((list_type lis));
list_type member prm((list_type elt, list_type lis,
		      boolean (*eq) (list_type l1, list_type l2)));
/* list_type cdddr_hlp prm((list_type lis));*/
list_type nconc prm((list_type left, list_type right));
list_type nreverse prm((list_type lis));
list_type pairlis prm((list_type l1, list_type l2));
list_type reverse prm((list_type lis));
list_type rplaca prm((list_type cell, list_type newcar));
list_type rplacd prm((list_type cell, list_type newcdr));
list_type subst prm((list_type new_exp, list_type old_atom, list_type expr,
	boolean (*eq) (list_type l1, list_type l2)));
int list_equal prm((list_type l1, list_type l2));

int fp2list (FILE *fp, list_type *result, char **strbuf, char *white_space,
	     char *open_chars, char *close_chars, FILE *diagnostic_output);
list_type parse_atom (FILE *fp, char **strbuf, char *white_space,
		      char *open_chars, char *close_chars);
int parse_list (FILE *fp, list_type *result, char **strbuf,
		char *white_space, char *open_chars, char *close_chars,
		FILE *diagnostic_output, char end_char);


#ifdef __cplusplus
}
#endif



static list_type _DUMMY;
static const char *_car_error = "car:  tried to take   car   of atom '%d'\n";
static const char *_cdr_error = "cdr:  tried to take   cdr   of atom '%d'\n";

#undef prm
#endif /* LIST_TYPE */
