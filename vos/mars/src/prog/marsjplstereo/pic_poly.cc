#include <math.h>
#include "nav_memory.h"

#include "JPLPic.h"

typedef struct {
  long low_vertex_row;		// Larger scanline row of the two vertices
  float col_intercept;
  long rows_remaining;
  float dcol_drow;
} PolygonEdgeT;



// POLYGON SCAN CONVERSION
// These routines impelement a scan-convertion polygon renderer, based
// on the pseudocode presented in Hearn & Baker, Computer Graphics,
// Prentice Hall, 1986, pp 83-90.

#ifdef DEBUG
static void PolyShowEdge (char *prefix, PolygonEdgeT p)
{
  if (prefix == NULL)
    prefix = "";

  printf ("%s ROW top %ld remaining %ld  COL int %g dc/dr %g\n",
	  prefix, p.low_vertex_row, p.rows_remaining,
	  p.col_intercept, p.dcol_drow);
}
#endif

// Take two vertices known to be on different scanlines, and insert their
// parameters into the   sides   list, which is sorted based on the
// ROW NUMBER OF THE LOWER VERTEX.
// 
static void PolyInsertByLowerVertex (PolygonEdgeT *sides,
				     long sides_allocated, long sides_entered,
				     long r1, long c1, long r2, long c2,
				     long next_row)
{
  long lower_vertex_row;
  long side_index = sides_entered - 1;
  float new_col2, new_dcol_drow;

  if (!(r1 != r2 && side_index < sides_allocated))
    fprintf (stderr, "PolyInsertByLowerVertex:  r1=%ld, r2=%ld, "
	     "side_index=%ld, sides_allocated=%ld\n", r1, r2, side_index,
	     sides_allocated);

  believev (r1 != r2 && side_index < sides_allocated);

  new_dcol_drow = ((float) (c2 - c1)) / ((float) (r2 - r1));
  new_col2 = (float) c2;

  // Consider the two input vertices, on the edge 1-2.
  // Stop short of the second vertex if the one that follows it is in
  // the same direction.  We do this because the (r2,c2) vertex will
  // be plotted on the r2 scanline at the start of the 2-3 edge in
  // the list anyway.

  if (r2 > r1 && r2 < next_row) {
    r2--;
    new_col2 -= new_dcol_drow;
  } else if (r2 < r1 && r2 > next_row) {
    r2++;
    new_col2 += new_dcol_drow;
  }

  // Find the lower vertex of the two; rows start at 0 on top and increase
  // as you go down the image

  lower_vertex_row = MAX (r1, r2);

  // Insert into list sorted ian by increasing lower vertex row.  Uses
  // memory-intensive shifting to perform the insert, but oh well.
  // Starts with side_index == maximum index already initialized.

  while (side_index > 0 &&
	 lower_vertex_row > sides[side_index-1].low_vertex_row) {
    sides[side_index] = sides[side_index-1];
    side_index--;
  }


  sides[side_index].low_vertex_row = lower_vertex_row;
  sides[side_index].rows_remaining = ABS(r2 - r1) + 1;
  sides[side_index].col_intercept = (r1 > r2) ? c1 : new_col2;
  sides[side_index].dcol_drow = new_dcol_drow;
    
} // PolyInsertByLowerVertex



// Returns the row of the next vertex whose row != pr[point_index]
// Note that the point list defines a ring, so the list will be
// searched in a circle.

static long PolyNextRow (long point_index, long *pr, long *pc, long count)
{
  long i;
  believe (pr != NULL && pc != NULL);

  // Search from here to the end
  for (i = point_index + 1; i < count; i++) {
    if (pr[i] != pr[point_index])
      return pr[i];
  }

  // Didn't find anything, start over at the beginning

  for (i = 0; i < point_index; i++) {
    if (pr[i] != pr[point_index])
      return pr[i];
  }

  fprintf (stderr, "PolyNextRow: %ld points, starting with %ld\n",
	   count, point_index);
  believe_ret (0 && "PolyNextRow failed!!!", -1);

  //  return -1;
} // PolyNextRow


// Create the list of polygon edge structures used for scan conversion.
// There will be one structure for each scanline that contains a vertex.
// ASSUMES AT LEAST ONE POINT in pr, pc.
// This list will be sorted so that the BOTTOM VERTEX of each edge is
// in increasing row order, using PolyInsertByLowerVertex.
// Also returns the MINIMUM ROW in the the point list, i.e. the higheset
// row in the result, which will define the termination condition since
// rows are rendered from bottom up.

static void PolySortByDecreasingLowerVertex (PolygonEdgeT *sides, long count,
					     long *pr, long *pc,
					     long *side_entries,
					     long *min_scanrow, JPLPic *pic,
					     int wrap, unsigned char byte_val,
					     int just_shift)
{
  long k, last_r1, last_c1;

  believev (sides != NULL && side_entries != NULL && min_scanrow != NULL &&
	    pr != NULL && pc != NULL && pic != NULL);

  *side_entries = 0;

  // Polygons are closed. So start (last_r1,last_c1) with the "last" point in the
  // list, in case we need to connect it to the first point.

  last_r1 = pr[count-1];
  last_c1 = pc[count-1];

  // Initialize min_scanrow, which will hold the row from the
  // uppermost vertex

  *min_scanrow = last_r1;

  for (k = 0; k < count; k++) {
    if (last_r1 != pr[k]) {

      // Put nonhorizontal edges into the sides vector

      (*side_entries)++;

      // Pass old point, current point, and row of next nonhoriz point

      PolyInsertByLowerVertex (sides, count, *side_entries,
			       last_r1, last_c1, pr[k], pc[k],
			       PolyNextRow (k, pr, pc, count));
    } else {

      // Horizontal lines get drawn directly

      pic->Line (last_r1, last_c1, last_r1, pc[k], wrap, byte_val, just_shift);
    }
    if (pr[k] < *min_scanrow)
      *min_scanrow = pr[k];

    // Save for next side

    last_r1 = pr[k];
    last_c1 = pc[k];
  }

#ifdef DEBUG
  for (k = 0; k < *side_entries; k++) {
    char buf[100];
    snprintf (buf, 100, "DLV Side %ld: ", k);
    PolyShowEdge (buf, sides[k]);
  }  
#endif
} // PolySortByDecreasingLowerVertex


// The   sides   list of interesting edges grows and shrinks as
// vertices come and go on each scanline.  This determines the bounds
// needed for the current scanline.  We assume the sides list is already
// sorted in decreasing order of the lower vertex.

static void PolyUpdateFirstAndLast (PolygonEdgeT *sides, long sides_allocated,
				    long scan_row,
				    long *first_side, long *last_side)
{
  believev (sides != NULL && first_side != NULL && last_side != NULL);

  // Add new edges when their LOWER VERTICES enter the active region

  while ((*last_side < sides_allocated-1) &&
	 sides[*last_side + 1].low_vertex_row >= scan_row)
    (*last_side)++;

  // Remove old edges when ALL ROWS HAVE BEEN PROCESSED

  while ((*first_side < sides_allocated) &&
	 sides[*first_side].rows_remaining == 0) {
    (*first_side)++;
  }

} // PolyUpdateFirstAndLast




// Part of PolyProcessColIntersections, implements the inner loop of
// bubble sort

static void PolyInsertWithIncreasingCol (PolygonEdgeT *sides,
					 long sides_allocated, long sort_me,
					 long first_side)
{
  believev (sides != NULL);

  while (sort_me > first_side &&
	 LT (sides[sort_me].col_intercept, sides[sort_me-1].col_intercept)) {
    PolygonEdgeT temp;

    temp = sides[sort_me];
    sides[sort_me] = sides[sort_me-1];
    sides[sort_me-1] = temp;
    sort_me--;
  }
} // PolyInsertWithIncreasingCol



// Re-sort the entries in the polygon edge list in order of increasing
// column intercept, which has been previously recomputed.  But only
// bother with those that have some number of rows remaining to be
// processed.  Uses bubble sort.  Also returns the number of active
// edges (those with rows remaining).

static void PolyProcessColIntersections (PolygonEdgeT *sides, long sides_allocated,
					 long scan_row, long first_side, long last_side,
					 long *col_int_count)
{
  long k;

  believev (sides != NULL && col_int_count != NULL);

  *col_int_count = 0;
  for (k = first_side; k <= last_side; k++) {
    if (sides[k].rows_remaining > 0) {
      (*col_int_count)++;
      PolyInsertWithIncreasingCol (sides, sides_allocated, k, first_side);
    }
  }
#ifdef DEBUG
  for (k = first_side; k <= last_side; k++) {
    if (sides[k].rows_remaining > 0) {
      char buf[100];
      snprintf (buf, 100, "PCI Side %ld: ", k);
      PolyShowEdge (buf, sides[k]);
    }
  }
#endif
} // PolyProcessColIntersections


// Draw the line segments that appear on this scanline.
// Their information is encoded in   sides.   col_int_count   has the
// number of side entries that have some rows remaining to be
// processed.   index   is the first side entry in the active list;
// this active list does not wrap around.

static void PolyDrawLines (PolygonEdgeT *sides, long sides_allocated,
			   long scan_row,
			   long col_int_count, long indx, JPLPic *pic,
			   int wrap, unsigned char byte_val, int just_shift)
{
  long k, c1, c2;

  believev (sides != NULL && pic != NULL);

  for (k = 0; k < (col_int_count + 1) >> 1; k++) {
    while (indx < sides_allocated-1 && sides[indx].rows_remaining == 0)
      indx++;

    c1 = (long) floor (sides[indx].col_intercept + 0.5);
    indx++;
    while (indx < sides_allocated-1 && sides[indx].rows_remaining == 0)
      indx++;
    c2 = (long) floor (sides[indx].col_intercept + 0.5);
    pic->Line (scan_row, c1, scan_row, c2, wrap, byte_val, just_shift);
    indx++;
  }
} // PolyDrawLines



static void PolyUpdateIntercepts (PolygonEdgeT *sides, long sides_allocated,
				 long first_side, long last_side)
{
  long k;

  for (k = first_side; k <= last_side; k++) {
    if (sides[k].rows_remaining > 0) {
      sides[k].rows_remaining--;
      sides[k].col_intercept -= sides[k].dcol_drow;
    }
  }
} // PolyUpdateIntercepts

void JPLPic::PolygonOutline (long *pr, long *pc, long count,
			     int wrap, unsigned char byte_val, int just_shift)
{
  int i;

  for (i = 0; i < count-1; i++) {
    Line (pr[i], pc[i], pr[i+1], pc[i+1], wrap, byte_val, just_shift);
    // If we're wrapping intensities, we will need to draw each
    // endpoint an odd number of times.  So we add SetPixel here to
    // make it happen 3 times for each pixel
    SetPixel (pr[i], pc[i], wrap, byte_val, just_shift);
  }
  if (count > 1) {
    Line (pr[count-1], pc[count-1], pr[0], pc[0], wrap, byte_val, just_shift);
    SetPixel (pr[i], pc[i], wrap, byte_val, just_shift);
  }
} // JPLPic::PolygonOutline



// Performs SCAN CONVERSION to render a closed polygon in an image.
// Starts at the BOTTOM of the polygon, sorts the edges by their
// lowest vertex, then connects them row-by-row.

void JPLPic::Polygon (long *pr, long *pc, long count,
		      int wrap, unsigned char byte_val, int just_shift)
{
  PolygonEdgeT *sides = NEW(mm, "Polygon Edge List")
    PolygonEdgeT[count];
  long side_entries=0, first_side, last_side, scan_row, min_scanrow=0;
  long col_int_count;

  if (count < 2 || pr == NULL || pc == NULL)
    return;

  if (count == 2) {
    Line (pr[0], pc[0], pr[1], pc[1], wrap, byte_val, just_shift);
    return;
  }

  PolySortByDecreasingLowerVertex (sides, count, pr, pc,
				   &side_entries, &min_scanrow,
				   this, wrap, byte_val, just_shift);

  // Initialize pointers into sorted list

  first_side = 0;
  last_side = 0;

  for (scan_row = sides[0].low_vertex_row; scan_row >= min_scanrow; scan_row--) {
    PolyUpdateFirstAndLast (sides, side_entries, scan_row,
			    &first_side, &last_side);
    PolyProcessColIntersections (sides, side_entries, scan_row,
				 first_side, last_side, &col_int_count);
    PolyDrawLines (sides, side_entries, scan_row, col_int_count, first_side,
		   this, wrap, byte_val, just_shift);
    PolyUpdateIntercepts (sides, side_entries, first_side, last_side);
  }
} // JPLPic::Polygon


