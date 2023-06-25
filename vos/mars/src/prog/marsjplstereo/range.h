#ifndef MWM_RANGE
#define MWM_RANGE
#include "list_type.h"

#define dflt_index2s itoa

#ifdef __STDC__
#define prm(x) x
#else
#define prm(x) ()
#endif

typedef struct {
    list_type l, enumerate;
    union {
	list_type l;
	int i;
    } prev;
} RangeT;

#ifdef __cplusplus
    extern "C" {
#endif

int dflt_char_s2index prm((char *str, int low, int hi, int *result));
int dflt_s2index prm((char *str, int low, int hi, int *result));
RangeT *mk_range prm ((int i1, int i2));
char *range2s prm((RangeT *r, char *buf, char *(*index2s) (int, char *)));
RangeT *range_merge prm((RangeT *r1, RangeT *r2));
RangeT *range_intersect prm((RangeT *r1, RangeT *r2));
RangeT *range_diff prm((RangeT *r1, RangeT *r2));
int free_range prm((RangeT *r));
int s2range prm((char *str, int (*s2index) (char *, int, int, int *), int low, int hi, RangeT **result));
int range_low prm((RangeT *r));
int range_high prm((RangeT *r));
int range_equal prm((RangeT *r1, RangeT *r2));
int range_subset prm((RangeT *r1, RangeT *r2));
int range_size prm((RangeT *r));

#ifdef __cplusplus
}
#endif


#define range_empty(r) (t1_rng = (r), (t1_rng == NULL || atom (t1_rng->l)))

/* Iterate in increasing order through the range elements */

#define RangeEnumInit(r) (t1_rng = (r), t1_rng ? (t1_rng->enumerate = \
	t1_rng->l, t1_rng->prev.l = (!atom (t1_rng->l) ? caar (t1_rng->l) : \
	nil)):nil)
#define RangeEnumTest(r) (r && !atom ((r)->enumerate))
#define RangeEnumInc(r) (t1_rng = (r), t1_u.l = cdar (t1_rng->enumerate), \
	(t1_rng->prev.i >= t1_u.i ? (int) \
	(t1_rng->enumerate = cdr (t1_rng->enumerate), t1_rng->prev.l = \
	(t1_rng->enumerate ? caar(t1_rng->enumerate) : nil)) : \
	t1_rng->prev.i++))
#define RangeEnumLOOP(r) for (RangeEnumInit(r); RangeEnumTest(r); RangeEnumInc(r))
#define RangeEnumVal(r) (t1_rng = (r), t1_rng ? t1_rng->prev.i : 0)

/* Iterate in decreasing order */

#define RangeEnumDecInit(r) (t1_rng = (r), t1_rng ? (t1_rng->enumerate = \
	nreverse (list_copy (t1_rng->l)), t1_rng->prev.l = (!atom (t1_rng->l) \
	? cdar (t1_rng->enumerate) : nil)):nil)
#define RangeEnumDecTest(r) RangeEnumTest(r)
#define RangeEnumDec(r) (t1_rng = (r), t1_u.l = caar (t1_rng->enumerate), \
	(t1_rng->prev.i <= t1_u.i ? (int) \
	(t1_rng->enumerate = cdr (t1_rng->enumerate), t1_rng->prev.l = \
	(t1_rng->enumerate ? cdar(t1_rng->enumerate) : nil)) : \
	t1_rng->prev.i--))
#define RangeEnumDecDec(r) RangeEnumDec(r)
#define RangeEnumDecLOOP(r) for (RangeEnumDecInit(r); RangeEnumDecTest(r); RangeEnumDecDec(r))
#define RangeEnumDecVal(r) RangeEnumVal(r)

/* Enumerate the pairs of contiguous ranges */

#define RangePairLOOP(r) for (RangePairInit(r); RangePairTest(r); RangePairInc(r))
#define RangePairInit(r) (t1_rng = (r), t1_rng ? (t1_rng->enumerate = t1_rng->l) : nil)
#define RangePairTest(r) ((t1_rng = (r)) && !null (t1_rng->enumerate))
#define RangePairInc(r) (t1_rng = (r), t1_rng->enumerate = cdr (t1_rng->enumerate))
#define RangePairVal(r,lo,hi) (t1_rng = (r) ? (t1_u.l = caar (t1_rng->enumerate), (lo ? (*lo = t1_u.i) : 0), t1_u.l = cdar (t1_rng->enumerate), (hi ? (*hi = t1_u.i) : 0), 1) : 0)

static RangeT *t1_rng;
static union {
    list_type l;
    int i;
} t1_u;

#endif
