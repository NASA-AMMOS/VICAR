#ifndef MWM_INCLUDE
#   define MWM_INCLUDE

/*

FIND CURRENT DIRECTORY
	-- loads the current directory's full pathname into global variable
	   cwd; there will be *no* trailing slash ('/') in the pathname.
	-- initialize the variable by calling   init_cwd(),   with no
	   parameters.

CURRENT DATE
	-- char *whats_today()   returns a readable static string with the
	   current date and time.

HOSTNAME
	-- char *what_host ()   returns the name of the machine on which
	   the program is being run

GUARANTEED MEMORY
	-- char *must_malloc (bytes, desc)   will either successfully
	   allocate   bytes   bytes of memory, or issue an error containing
	   desc   and exit(1).
	-- void BufferAlloc(ptr, eltsize, oldlenptr, newlen, what)   will
	   ensure that the   ptr   buffer has length   newlen, freeing up
	   any already-must_malloced memory if necessary.

STACK MEMORY ADT
	-- Sometimes it's useful to keep extra information around in a
	   depth-first search.  For instance, when scanning a filesystem
	   you might want to remember the names of all files or
	   directories in an ancestor's directory.  This Abstract Data
	   Type provides an easy way to remember lots of extra information
	   in a stack.  It's also quite simple to free up space; just free
	   up the single element nearest the new top of stack (where you
	   want to be after popping old, useless data), and all of the
	   memory in between will automatically free up.
	-- StackMemoryT *st_init (block_len)   -- initializes an object of
	   StackMemoryT.  You simply specify the longest block length that
	   you ever expect to allocate (but you're not bound to it, the
	   routines can increase this limit if usage demands it).  Returns
	   NULL on failure.
	-- char *st_alloc (memptr, bytes)   -- what you call instead of
	   malloc.  memptr is the address of a valid StackMemoryT pointer;
	   the value of that pointer will change if another block of
	   memory gets allocated.  Returns a pointer to a word-aligned
	   free section of memory if successful, NULL on failure.
	-- int st_free (memptr, loc)   -- free up all stack frames above
	   the given memory   loc.   No memory is actually deallocated, we
	   just adjust some pointers (this helps prevent thrashing on
	   malloc calls).  Returns 1 if successful, 0 otherwise.
	-- int st_in (memptr, loc)   -- see if   loc   lies somewhere on
	   the stack.  Returns 0 on error or if   loc   is not in the stack,
	   1 otherwise.

PERMANENT STRING STORAGE
	-- char *new_string (str) -- allocate a copy of   str   in
	   permanent storage (i.e. storage which need never be reclaimed).
	   Doesn't return if no memory is available.

WRITE C STRING
	-- int fwrstr (fp, str), fwrstrn (fp, str, n), wrstr (str), wrstrn
	   (str, n), char *swrstr (s, str), char *swrstrn (s, str, n) --
	   These functions write a copy of their input string, modified to
	   conform to the syntax of C strings.  E.g.,
		wrstr ("\042\115\141\162\153\042\010") ==> "\"Mark\"\n"
*/


#ifndef boolean
#  define boolean int

#  ifndef TRUE
#    define TRUE 1
#  endif

#  ifndef FALSE
#    define FALSE 0
#  endif
#endif

#define NOT_IN_VECTOR -1	/* cannot be a valid array index */

/* macros for the   parse_args   routine */

#define P_STRING 1		/* Macros for the result_type attribute */
#define P_STR P_STRING
#define P_CHAR 2
#define P_SHORT 3
#define P_LONG 4
#define P_INT P_LONG
#ifdef P_FILE
#undef P_FILE
#endif
#define P_FILE 5
#define P_OLD_FILE 6
#define P_NEW_FILE 7
#define P_FLOAT 8
#define P_DOUBLE 9
#define P_INT_RANGE 10		/* Integer range, from -max to max */
#define P_POSINT_RANGE 11	/* Positive integer range, from 0 to max */

#define P_CASE_INSENSITIVE 01	/* Macros for the   flags   attribute */
#define P_REQUIRED_PREFIX 02
#define P_WAS_CHANGED 04

#define P_NO_ARGS 0		/* Macros for the   arg_count   attribute */
#define P_ONE_ARG 1
#define P_INFINITE_ARGS 2
#define P_MANY_ARGS P_INFINITE_ARGS
#define P_FUNCTION_ARG 3

#define p_entry(pref,swit,flag,count,type,store,size) \
  { (pref), (swit), (flag), (count), (type), (ArgInfoResultT) (store), (size) }

#define EDIT_INSERT 1
#define EDIT_DELETE 2
#define EDIT_CHANGE 4
#define EDIT_SWAP 8
#define EDIT_NOCHANGE 16

typedef char *ArgInfoResultT;
typedef struct {
    const char *prefix;
    const char *string;
    int flags;
    int count;
    int result_type;
    ArgInfoResultT result_ptr;	/* Must hold largest type, for alignment */
    int table_size;		/* table size for MANY_ARGS, new value for
				   NO_ARGS, max int for INT_RANGE */
} arg_info;

/* StackMemoryT definition. */

typedef struct SMT {
    struct SMT *prev;	/* pointer to previous memory block */
    struct SMT *next;	/* pointer to next (already allocated) block */
    int block_len;	/* length (in bytes) of each block for strings */
    char *start;	/* pointer to start of actual storage */
    char *top;		/* pointer to next available position */
} StackMemoryT;


/* in_range -- returns TRUE iff

   Algebraic:   min (bound1, bound2) <= num <= max (bound1, bound2)

   English:     num   lies between   bound1 and bound2
*/

#define in_range(num,bound1,bound2) ((_r_b1 = (bound1)) > (_r_b2 = (bound2)) ?\
	((_r_n = (num)) > _r_b1 ? FALSE : (_r_n < _r_b2 ? FALSE : TRUE)) : \
	((_r_n = (num)) < _r_b1 ? FALSE : (_r_n > _r_b2 ? FALSE : TRUE)))

#ifndef NO_MWM_VARS
#  ifndef mklow
#    ifdef isupper
#      define mklow(c) (_2lo=(c), ((isupper (_2lo)) ? (tolower (_2lo)) : (_2lo)))
#    else
#    define mklow(c) (((_2lo = (c)) >= 'A' && _2lo <= 'Z') ? _2lo + 32 : _2lo)
#    endif

     static char _2lo;
#  endif
#  ifndef mkupp
#    define mkupp(c) (((_2up = (c)) >= 'a' && _2up <= 'z') ? _2up - 32 : _2up)

     static char _2up;
#  endif 

#  ifndef int_min
#    define int_min(a,b) (_int_m_a = (a), _int_m_b = (b), _int_m_a < \
	_int_m_b ? _int_m_a : _int_m_b)
#    define int_max(a,b) (_int_m_a = (a), _int_m_b = (b), \
	(_int_m_a < _int_m_b) ? _int_m_b : _int_m_a)

     static int _int_m_a, _int_m_b;
#  endif /* int_min */

#  ifndef isvowel
#    define isvowel(c) (_is_v = mklow (c), _is_v == 'a' || _is_v == 'e' || \
	_is_v == 'i' || _is_v == 'o' || _is_v == 'u')

     static char _is_v;
#  endif

#  ifndef ordinal_suffix
#    define ordinal_suffix(x) \
		(_o_s_x = (x), _o_s_x = int_max (_o_s_x, -_o_s_x), \
		((_o_s_x % 10 == 1 && _o_s_x != 11) ? "st" : \
		((_o_s_x % 10 == 2 && _o_s_x != 12) ? "nd" : \
		((_o_s_x % 10 == 3 && _o_s_x != 13) ? "rd" : "th"))))
     static int _o_s_x;
#  endif
#  define st_in(m,l) (((_st_tm = (m)) != _st_last) ? st_in_hlp (_st_tm, (l)) \
	: ((_st_bot <= (_st_tl = (l)) && _st_tl < _st_top) ? 1 : \
	st_in_hlp (_st_tm, _st_tl)))

    static StackMemoryT **_st_tm;
    static char *_st_tl;

    /*NOTUSED*/
    static void mwm_I_know_this_is_not_used_dummy (void) {
      _2lo = _2up = _is_v = 0;
      _int_m_a = _int_m_b = _o_s_x = 0;
      _st_tl = 0;
      _st_tm = 0;
    }
#else
#  define st_in st_in_hlp
#endif

#define wrstr(s)		fwrstr (stdout, (s))
#define wrstrn(s,n)		fwrstrn (stdout, (s), (n))

#ifndef ibmrt
   extern char cwd[];
#endif

#ifdef __STDC__
#define prm(x) x
#ifndef FILE
#include <stdio.h>
#endif
#else
#define prm(x) ()
#endif

#ifdef __cplusplus
    extern "C" {
#endif

void fmultout prm((FILE *fp, int number, char ch));
char *c2s prm((char ch, int ws2text, char *buff));
int fmt2c prm((char *str, char *cp));
char *fmt2s prm((char *str, char *buf));
char *s2fmt prm((char *str, char *buf));
char *itoa prm((int num, char *buff));
char *safe_strncpy prm((char *dest, char *source, int max_length));
char *safe_bcopy prm((char *dest, char *souce, long count));
boolean in_vector prm((char *str, char *vector[]));
char *svec2s prm((char **str, char *buf));
boolean parse_args prm((int argc, char **argv, arg_info table[],
	int table_size, char **others, int others_count));
int entry_was_changed (arg_info table[], int table_size,
		       ArgInfoResultT storage);
void arg_clear_changed_flags (arg_info table[], int table_size);
char *build_usage prm((arg_info table[], int table_size, char *result,
	int result_len, const char *rest_of_params));
char *find_basename prm((char *pathname));
void set_costs prm((int ins, int del, int change, int swap));
void view_costs prm((int *ins, int *del, int *ch, int *swap));
int edit_dist prm((char *from, char *to));
int edit_dist_matrix prm((char *from, char *to, int *matrix, int *rows, int *cols));
int edit_distn prm((char *from, long int flen, char *to, long int tlen,
		    int *matrix, int *rows, int *cols));
void print_edit_matrix prm((FILE *fp, char *from, char *to,
			    int *matrix, int rows, int cols));
int cistrcmp prm((char *s1, char *s2));
int cistrncmp prm((char *s1, char *s2, int n));
char *sindex prm((char *big, char *small));
char *fhilite prm((char *str, FILE *fp));	/* Load with -ltermcap */
char *funder prm((char *str, FILE *fp));	/* Load with -ltermcap */
char *shilite prm((char *str, char *result));	/* Load with -ltermcap */
char *sunder prm((char *str, char *result));	/* Load with -ltermcap */
void hilite_term prm((char *term_name));	/* Load with -ltermcap */

int init_cwd prm((void));
char *squish prm((char *pathname));
char *whats_today prm((void));
char *what_host prm((void));
char *must_malloc prm((int bytes, char *desc));
void BufferLAlloc (char **ptr, int elt_size, long *old_len, long len,
		   char *what);
void BufferAlloc prm((char **ptr, int eltsize, int *olen, int len, char
	   *what));
void BufferRealloc (char **ptr, int elt_size, int *old_len, int len,
		    char *what);
StackMemoryT *st_init prm((int block_len));
char *st_alloc prm((StackMemoryT **mem, int bytes));
int st_in_hlp prm((StackMemoryT **mem, char *loc));
int st_free prm((StackMemoryT **memptr, char *loc));
char *new_string prm((const char *str));

int fwrstr prm((FILE *fp, char *str));
int fwrstrn prm((FILE *fp, char *str, int n));
char *swrstr prm((char *s, char *str));
char *swrstrn prm((char *s, char *str, int n));

int is_directory prm((char *pathname, FILE *mesgfp));
int is_file prm((char *pathname, FILE *mesgfp));

#ifdef __cplusplus
	       }
#endif

#endif
