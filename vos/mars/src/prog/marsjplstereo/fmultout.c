#include <stdio.h>
#include "mwm.h"

/* fmultout -- outputs a single character   number   times to stream   fp   */

void fmultout (FILE *fp, int number, char ch)
{
    if (fp == (FILE *) NULL)
	return;

    while (number-- > 0)
	(void) putc (ch, fp);
} /* fmultout */
