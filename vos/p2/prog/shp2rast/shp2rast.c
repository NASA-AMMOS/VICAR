#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>

#include "defines.h"
#include "vicmain_c.h"
#include "zifmessage.h"
#include "zmabend.h"
#include "shapefil.h"
#include "zvprintf.h"
#include "gems.h"
#include "concave.h"

static char *cell = NULL;
static int pixPerDeg = 0;
static int margin = 0;

#define halfPixelWidthInDegrees (1.0 / (2.0 * pixPerDeg))

static int rasterColor = 0;
void drawProc(int y, int xl, int xr) {
  memset (cell + y * (pixPerDeg + margin) + xl, rasterColor, sizeof (char) * (xr - xl + 1));
}

static void dotproc(int x, int y) {
  if (x >=0 && y >= 0 && x < (pixPerDeg + margin) && y < (pixPerDeg + margin))
    cell[y * (pixPerDeg + margin) + x] = rasterColor;
}

void drawLine(int nvert,		/* number of vertices */
	      Point2 *points) {		/* vertices of polygon */
  int vert, x1, x2, y1, y2;

  for (vert = 0; vert < nvert - 1; ++vert) {
    x1 = points[vert].x;
    y1 = points[vert].y;
    x2 = points[vert + 1].x;
    y2 = points[vert + 1].y;
    digline(x1, y1, x2, y2, dotproc);
  }
}

void main44(void)
{
  int parmct, parmdf;
  char inpfilename [99];
  char outfileprefix [99];
  char outfilesuffix [99];
  int vunit;

  SHPHandle shpHandle;
  int nEntities, nShapetype;
  double xyzmMin [4], xyzmMax [4];

  char dataPresent [360 * 181];

  int entity;
  int vertex;
  int i, j;

  SHPObject * shpObject;
  int maxPoints = 0;		/* max number of points in a polygon for the dataset */

  int minLat, maxLat, minLon, maxLon;
  int fg, bg;
  int nofill;

  zifmessage ("shp2rast version 2019-07-18");

  /* fetch params */
  zvparm ("shp", inpfilename, &parmct, &parmdf, 1, 99);
  zvparm ("prefix", outfileprefix, &parmct, &parmdf, 1, 99);
  zvparm ("suffix", outfilesuffix, &parmct, &parmdf, 1, 99);
  zvp ("minLat", & minLat, & parmct);
  zvp ("maxLat", & maxLat, & parmct);
  zvp ("minLon", & minLon, & parmct);
  zvp ("maxLon", & maxLon, & parmct);
  zvp ("fg", & fg, & parmct);
  zvp ("bg", & bg, & parmct);
  zvp ("pixPerDeg", & pixPerDeg, & parmct);
  margin = zvptst("margin");
  nofill = zvptst("nofill");

  cell = (char *) malloc(sizeof(char) * (pixPerDeg + margin) * (pixPerDeg + margin));

/*    printf ("n %d e %d w %d s %d\n", maxLat, maxLon, minLon, minLat); */

  if (minLat < -90 || minLat > 90 ||
      maxLat < -90 || maxLat > 90 ||
      minLat >= maxLat ||
      minLon < -180 || minLon > 180 ||
      maxLon < -180 || maxLon > 180 ||
      minLon >= maxLon)
    zmabend ("lats must be in range -90 to 90; lons must be in range -180 to 180; mins must be less than maxs");      
  minLat += 90;
  maxLat += 90;			/* 89, because it will be used as an index */
  minLon += 180;
  maxLon += 180;		/* ditto */

  /* open shape input */
  if (! (shpHandle = SHPOpen (inpfilename, "rb")))
    zvnabend(1000, "error opening %s for input", inpfilename);
  
  SHPGetInfo (shpHandle, & nEntities, & nShapetype, xyzmMin, xyzmMax);
  zvnprintf (1000, "shp2rast: nEntities==%d", nEntities);

  /* check shape type */
  switch (nShapetype) {
  case SHPT_NULL:
    zvnabend (1000, "%s contains an unsupported shape entity type: NULL", inpfilename);
  case SHPT_POINT:
    zvnabend (1000, "%s contains an unsupported shape entity type: POINT", inpfilename);
  case SHPT_ARC:
  case SHPT_POLYGON:
    break;
  case SHPT_MULTIPOINT:
    zvnabend (1000, "%s contains an unsupported shape entity type: MULTIPOINT", inpfilename);
  case SHPT_POINTZ:
    zvnabend (1000, "%s contains an unsupported shape entity type: POINTZ", inpfilename);
  case SHPT_ARCZ:
    zvnabend (1000, "%s contains an unsupported shape entity type: ARCZ", inpfilename);
  case SHPT_POLYGONZ:
    zvnabend (1000, "%s contains an unsupported shape entity type: POLYGONZ", inpfilename);
  case SHPT_MULTIPOINTZ:
    zvnabend (1000, "%s contains an unsupported shape entity type: MULTIPOINTZ", inpfilename);
  case SHPT_POINTM:
    zvnabend (1000, "%s contains an unsupported shape entity type: POINTM", inpfilename);
  case SHPT_ARCM:
    zvnabend (1000, "%s contains an unsupported shape entity type: ARCM", inpfilename);
  case SHPT_POLYGONM:
    zvnabend (1000, "%s contains an unsupported shape entity type: POLYGONM", inpfilename);
  case SHPT_MULTIPOINTM:
    zvnabend (1000, "%s contains an unsupported shape entity type: MULTIPOINTM", inpfilename);
  case SHPT_MULTIPATCH:
    zvnabend (1000, "%s contains an unsupported shape entity type: MULTIPATCH", inpfilename);
  default:
    zvnabend (1000, "%s contains an unsupported shape entity type: UNKNOWN", inpfilename);
  }

  if (nShapetype != SHPT_POLYGON && nShapetype != SHPT_ARC)
    zmabend ("Input must contain either POLYGONs or ARCs");

  /* determine which 1x1 degree cells have data */
  memset (dataPresent, 0, sizeof (char) * 360 * 181);
      
  /* for each polygon ... */
  for (entity = 0; entity < nEntities; entity ++) {
    shpObject = SHPReadObject (shpHandle, entity);

    if (! shpObject)
      zmabend ("SHPReadObject failed");

    /* keep track of largest polygon size for later */
    maxPoints = (maxPoints < shpObject -> nVertices) ? shpObject -> nVertices : maxPoints;

    /* for each vertex ... */
    for (vertex = 0; vertex < shpObject -> nVertices; vertex ++) {
      double tmp;

      /* calculate the vertex's dataPresent x coordinate */
      tmp = shpObject -> padfX [vertex]
	/* The cell pixel size is 3 seconds square. Need to
		    move vector points from pixel is point to pixel is
		    area by moving NW a half pixel */
	- halfPixelWidthInDegrees
	/* Need to shift values to a positive coordinate
                    space for rasterizing algorithm*/
	+ 180.0;

      /* conversion to int truncates, but we want rounding */
      /* No, we don't! We're calculating which cells are touched here. */
      /*        if (tmp > 0.0) */
      /*  	tmp += 0.5; */
      /*        else if (tmp < 0.0) */
      /*  	tmp -= 0.5; */

      i = (int) tmp;

      if (i == 360) {
	/* printf ("wrapping right\n"); */
	i = 0;
      }

      if (i == -1) {
	/* printf ("wrapping left\n"); */
	i = 359;
      }

      if (i < -1 || i > 360)
	zvnabend (1000, "index %d computed from lon %f\n", i, shpObject -> padfX [vertex]);
	     
      /* calculate the vertex's dataPresent y coordinate */
      tmp = shpObject -> padfY [vertex]
	+ halfPixelWidthInDegrees
	+ 90.0;

      /* No, we don't! We're calculating which cells are touched here. */
      /*        if (tmp > 0.0) */
      /*  	tmp += 0.5; */
      /*        else if (tmp < 0.0) */
      /*  	tmp -= 0.5; */

      j = (int) tmp;

      if (j < 0 || j > 180)
	zvnabend (1000, "index %d computed from lat %f\n", j, shpObject -> padfY [vertex]);
	     
      if (! dataPresent [i + j * 360])
	/* printf ("entity %d vertex %d touching cell i:%d j:%d first at x:%lf y:%lf\n", entity, vertex, i - 180, j - 90, shpObject -> padfX [vertex], shpObject -> padfY [vertex]);*/

      dataPresent [i + j * 360] = 1;
    }

    SHPDestroyObject (shpObject);
  }

  /* find smallest rectangle containing all dataPresent and count occupied cells */
  {
    int minI = 361;
    int maxI = -1;
    int minJ = 181;
    int maxJ = -1;
    int cellCount = 0;

    for (i = 0; i < 360; i ++)
      for (j = 0; j < 180; j ++)
	if (dataPresent [i + j * 360]) {
	  minI = (minI > i ? i : minI);
	  maxI = (maxI < i ? i : maxI);
	  minJ = (minJ > j ? j : minJ);
	  maxJ = (maxJ < j ? j : maxJ);
	  cellCount ++;
	}

    if (! cellCount)
      zvnabend (1000, "%s is completely outside the specified area", inpfilename);

    zvnprintf (1000, "shp2rast: %s touches %d cells with lon range %d:%d, lat range %d:%d", inpfilename, cellCount, minI - 180, maxI - 179, minJ - 90, maxJ - 89);
  }

  {
    char outFileName [1000];
/*      printf ("mallocing space for %d points\n", maxPoints); */
    Point2 * points = (Point2 *) malloc (sizeof (Point2) * maxPoints);
/*      printf ("%d\n", points); */
      
    /* rasterize shape data into each cell image */
    for (i = minLon; i < maxLon; i ++)
      for (j = minLat; j < maxLat; j ++)
	if (dataPresent [i + j * 360]) {
	  /* erase the rasterizing cell */
	  memset (cell, bg, sizeof (char) * (pixPerDeg + margin) * (pixPerDeg + margin));

	  /* compute file name */
	  snprintf (outFileName, 1000, "%s%c%02d%c%03d%s", outfileprefix,
		   (j - 90) < 0 ? 's' : 'n', abs (j - 90),
		   (i - 180) < 0 ? 'w' : 'e', abs (i - 180),
		   outfilesuffix);

	  /* for each polygon, if it touches this cell, rasterize it */
	  for (entity = 0; entity < nEntities; entity ++) {
	    shpObject = SHPReadObject (shpHandle, entity);

	    {
	      int vi, vj;
	      double tmp;

	      for (vertex = 0; vertex < shpObject -> nVertices; vertex ++) {
		/* calculate the vertex's dataPresent x coordinate */
		tmp = shpObject -> padfX [vertex]
		  /* The cell pixel size is 3 seconds square. Need to
		    move vector points from pixel is point to pixel is
		    area by moving NW a half pixel */
		  - halfPixelWidthInDegrees
		  /* Need to shift values to a positive coordinate
                    space for rasterizing algorithm*/
		  + 180.0;

		/* conversion to int truncates, but we want rounding */
		/* No, we don't! We're calculating which cells are touched here. */
		/*  		if (tmp > 0.0) */
		/*  		  tmp += 0.5; */
		/*  		else if (tmp < 0.0) */
		/*  		  tmp -= 0.5; */

		vi = (int) tmp;

		if (vi == 360)
		  vi = 0;

		if (vi == -1)
		  vi = 359;
	     
		/* calculate the vertex's dataPresent y coordinate */
		tmp = shpObject -> padfY [vertex]
		  + halfPixelWidthInDegrees
		  + 90.0;

		/* No, we don't! We're calculating which cells are touched here. */
		/*  		if (tmp > 0.0) */
		/*  		  tmp += 0.5; */
		/*  		else if (tmp < 0.0) */
		/*  		  tmp -= 0.5; */

		vj = (int) tmp;

		if (vi == i && vj == j)
		  break;
	      }

	      /* if we left the loop early, then we found a vertex in the cell, so rasterize it */
	      if (vertex < shpObject -> nVertices) {
		int numPoints = 0;
		int part;
		Window cellBoundaries = {0, 0, pixPerDeg - 1 + margin, pixPerDeg - 1 + margin};

		/* collect points into cell space coordinates for concave function */
		for (vertex = 0, part = 0; vertex < shpObject -> nVertices; vertex ++) {
		  if (part < shpObject -> nParts && shpObject -> panPartStart [part] == vertex) {
		    if (part > 0) { /* this is the first vertex of a hole */
		      if (part == 1) /* rasterize polygon (the previous part) */
			rasterColor = fg;
		      else	/* rasterize hole */
			rasterColor = bg;
		      
		      /* start next part */
		      numPoints = 0;
		    }

		    part ++;
		  }

		  points [numPoints] . x = (shpObject -> padfX [vertex]
					    /* The cell pixel size varies. Need to move vector points
					       from pixel is point to pixel is area by moving NW a half pixel */
					    - halfPixelWidthInDegrees
					    /* Need to shift values to a positive coordinate space */
					    + 180.0
					    /* subtract left edge of 1 degree cell; this should leave >= 0, < 1 degree */
					    - i)
		    * pixPerDeg;

		  points [numPoints] . y = ((shpObject -> padfY [vertex] + halfPixelWidthInDegrees + 90.0) - j)
		    * pixPerDeg + margin;

		  /* if this is the first point, or it is a point different from the last point, increment numPoints */
		  if (! numPoints ||
		      points [numPoints] . x != points [numPoints - 1] . x ||
		      points [numPoints] . y != points [numPoints - 1] . y) {
		    numPoints ++;
		  }
		}

		/* rasterize the points */
		if (part > 1)
		  rasterColor = bg;
		else
		  rasterColor = fg;

		/* the concave function should but does not draw the enclosing line, just the fill, so always draw the line */
		drawLine (numPoints, points);
		if (! nofill)
		  concave (numPoints, points, & cellBoundaries, drawProc);
	      }

	      SHPDestroyObject (shpObject);
	    }
	  }

	  zvnprintf (1000, "shp2rast: creating %s", outFileName);

	  /* create output image */
	  if (zvunit (& vunit, "U_NAME", 1, "U_NAME", outFileName, NULL) != 1)
	    zvnabend (1000, "zvunit failed on %s", outFileName);

	  if (zvopen (vunit, "U_NL", pixPerDeg + margin, "U_NS", pixPerDeg + margin, "OP", "WRITE", "OPEN_ACT", "SA", NULL) != 1)
	    zvnabend (1000, "zvopen failed on %s", outFileName);

	  {
	    int line;

	    for (line = (pixPerDeg + margin) - 1; line >= 0; line --) /* flip vertically */
	      /* write one image line to VICAR cell output */
	      zvwrit (vunit, cell + line * (pixPerDeg + margin), "LINE", (pixPerDeg + margin) - line, "SAMP", 1, "NSAMPS", pixPerDeg + margin, NULL);
	  }

	  /* done with VICAR cell image */
	  zvclose (vunit, NULL);
	}
  }

  SHPClose (shpHandle);

  return;
}
