#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "vicmain_c"
#include "zifmessage.h"
#include "zvprintf.h"

void main44(void)
{
  int iunit, ounit;
  float *inBuf, *outBuf;
  int nl, ns, line, samp;
  int count, def, cnt;
  /* These variables are defined in the Ancillary Geographic Product (AGP) ATBD sections 3.5.6 - 3.5.8 */
  float deltaX, deltaY, a_rad, h0, dif, sum;
  double wX, wY;
  float tanS, cosArad, sinArad;
  int winX, winY;
  float hm, hi;
  int debLine=0, debSamp=0;
  char buf0[200], buf[1000];

  zifmessage("SDSEMS version 2020-12-21");

  assert(zvunit(&iunit, "INP", 1, NULL) == 1);
  assert(zvunit(&ounit, "OUT", 1, NULL) == 1);

  assert(zvopen(iunit, "OPEN_ACT", "SA", "IO_ACT", "SA", "U_FORMAT", "REAL", NULL) == 1);
  assert(zvopen(ounit, "OPEN_ACT", "SA", "IO_ACT", "SA", "OP", "WRITE", "O_FORMAT", "REAL", NULL) == 1);

  assert(zvget(iunit, "NL", &nl, "NS", &ns, NULL) == 1);
  assert(zvparmd("WX", &wX, &count, &def, 0, 0) == 1);
  assert(zvparmd("WY", &wY, &count, &def, 0, 0) == 1);
  assert(zvp("DEBLINE", &debLine, &cnt) == 1);
  assert(zvp("DEBSAMP", &debSamp, &cnt) == 1);

  if (! debLine)
    assert(! debSamp);
  if (! debSamp)
    assert(! debLine);

  if (debLine)
    assert(debLine > 0 && debLine <= nl &&
	   debSamp > 0 && debSamp <= ns);

  if (debLine) {
    zvnprintf(100, "using WX %lf, WY %lf, DEBLINE %d, DEBSAMP %d", wX, wY, debLine, debSamp);
  }

  assert((inBuf = (float*) malloc(sizeof(float) * nl * ns)));
  assert((outBuf = (float*) malloc(sizeof(float) * nl * ns)));

  for (line = 0; line < nl; ++line)
    assert(zvread(iunit, inBuf + line * ns, "LINE", line + 1, NULL) == 1);

  assert(zvclose(iunit, NULL) == 1);

#define PI 3.14159265359
#define INPIX(LINE,SAMP)  (inBuf[(SAMP) + (LINE)*ns])
#define OUTPIX(LINE,SAMP) (outBuf[(SAMP) + (LINE)*ns])
  /* A through I are defined in AGP ATBD section 3.5.8 */
#define A(LINE,SAMP) (INPIX((LINE-1),(SAMP-1)))
#define B(LINE,SAMP) (INPIX((LINE-1),(SAMP)))
#define C(LINE,SAMP) (INPIX((LINE-1),(SAMP+1)))
#define D(LINE,SAMP) (INPIX((LINE),(SAMP-1)))
#define E(LINE,SAMP) (INPIX((LINE),(SAMP)))
#define F(LINE,SAMP) (INPIX((LINE),(SAMP+1)))
#define G(LINE,SAMP) (INPIX((LINE+1),(SAMP-1)))
#define H(LINE,SAMP) (INPIX((LINE+1),(SAMP)))
#define I(LINE,SAMP) (INPIX((LINE+1),(SAMP+1)))

  if (debLine) {
    zvnprintf(100, "\n              SAMP %6d SAMP %6d SAMP %6d", debSamp - 1, debSamp, debSamp + 1);
    for (line=debLine - 2; line < debLine + 1; ++line) {
      snprintf(buf, 1000, "LINE %6d: ", line + 1);
      for (samp=debSamp - 2; samp < debSamp + 1; ++samp) {
	snprintf(buf0, 200, "%12f", INPIX(line, samp));
	strncat(buf, buf0, 1000);
      }
      zifmessage(buf);
    }
  }

  /* calc non-edge cases */
  for (line = 1; line < nl - 1; ++line)
    for (samp = 1; samp < ns - 1; ++samp) {
      /* From AGP ATBD equation (3), section 3.5.6.1 */
      deltaX = ((A(line, samp) + 2*D(line, samp) + G(line, samp)) - (C(line, samp) + 2*F(line, samp) + I(line, samp)))/(8*wX);
      deltaY = ((A(line, samp) + 2*B(line, samp) + C(line, samp)) - (G(line, samp) + 2*H(line, samp) + I(line, samp)))/(8*wY);
      /* From AGP ATBD equation (9), section 3.5.8.1 */
      a_rad = PI - atan2f(deltaX, deltaY); /* note x/y reversed here */
      /* From AGP ATBD equation (4), section 3.5.6.1 */
      /* Not actually used, because Equation uses the tan(s), or the tan(atan(sqrt(...))), and there is no point calcing tan(atan(...)) */
      /* s_rad = atanf(sqrtf(deltaX * deltaX + deltaY * deltaY)); */
      tanS = sqrtf(deltaX * deltaX + deltaY * deltaY);
      
      /* From AGP ATBD equation (6), section 3.5.7.1 */
      cosArad = cosf(a_rad);
      sinArad = sinf(a_rad);
      h0 = A(line,samp);
      sum = 0.0;

      if (debLine && line == debLine - 1 && samp == debSamp - 1) {
	zvnprintf(1000, "\ndeltaX %f\ndeltaY %f\na_rad %f (%f in degrees)\ntanS %f (s in degrees %f)\ncosArad %f\nsinArad %f\nh0 %f\n",
		  deltaX, deltaY, a_rad, a_rad * 180.0 / PI, tanS, atanf(tanS) * 180.0 / PI, cosArad, sinArad, h0);
      }	

      for (winX = 0; winX < 3; ++winX)
	for (winY = 0; winY < 3; ++winY) {
	  /* hm and hi, respectively are "hmeas,i" and "hi" from AGP ATBD equation (7), section 3.5.7.1 */
	  /* hi formula is AGP ATBD equation (6), section 3.5.7.1 */
	  /* the w here converts window pixel offsets winX and winY to distance in units of h0 */
	  hi = h0 - tanS * (wX * winX * cosArad + wY * winY * sinArad);
	  hm = INPIX(line - 1 + winX, samp - 1 + winY);
	  dif = hm - hi;
	  sum += dif * dif;
	  if (debLine && line == debLine - 1 && samp == debSamp - 1) {
	    zvnprintf(1000, "LINE %d SAMP %d hi %f hm %f dif %f dif^2 %f (wX * winX * cosArad + wY * winY * sinArad) %f",
		      line + winX, samp + winY, hi, hm, dif, dif*dif, wX * winX * cosArad + wY * winY * sinArad);
	  }
	}

      if (debLine && line == debLine - 1 && samp == debSamp - 1) {
	zvnprintf(1000, "\nsum %f\nsqrt(sum/8.0) %f", sum, sqrtf(sum/8.0));
      }

      OUTPIX(line, samp) = sqrtf(sum/8.0);
    }
      
  /* copy top/bottom edges from neighbors, excluding corners */
  for (samp = 1; samp < ns - 1; ++samp) {
    OUTPIX(0, samp) = OUTPIX(1, samp);
    OUTPIX(nl - 2, samp) = OUTPIX(nl - 1, samp);
  }

  /* copy left/right edges from neighbors, excluding corners */
  for (line = 1; line < nl - 1; ++line) {
    OUTPIX(line, 0) = OUTPIX(line, 1);
    OUTPIX(line, ns - 2) = OUTPIX(line, ns - 1);
  }

  /* copy corners */
  OUTPIX(0, 0) = OUTPIX(1, 1);
  OUTPIX(0,ns - 1) = OUTPIX(1, ns - 2);
  OUTPIX(nl - 1, ns - 1) = OUTPIX(nl - 2, ns - 2);
  OUTPIX(nl - 1, 0) = OUTPIX(nl - 2, 1);


  for (line = 0; line < nl; ++line)
    assert(zvwrit(ounit, outBuf + line * ns, "LINE", line + 1, NULL) == 1);

  assert(zvclose(ounit, NULL) == 1);
}
