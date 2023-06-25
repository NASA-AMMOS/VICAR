/******************************************************************************
*                                                                             *
*                       C M O D _ E R R O R _ S T D I O                       *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 28 Jul 2009                  *
*                                       Updated:  7 Dec 2009                  *
*                                                                             *
*                                       Copyright (C) 2009                    *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains an error-reporting function. It is intended for
	use by the CMOD implementation and not as a public function. This
	version reports to standard I/O. It is split out into a separate
	file so that custom implementations can be used in place of this
	one for target environments that need different behavior. */


#include "cmod.h"
#include "cmod_error.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>


/******************************************************************************
********************************   CMOD_ERROR   *******************************
*******************************************************************************

    This function reports information on internal errors. A distinction is
    made in the format of the printout between internal errors and messages
    that are intended to inform users of operational problems (such as an
    input file missing). The latter are identified by the first letter of
    str1 being capitalized, in which case the source-code filename and line
    numbers are not displayed. */

void cmod_error(
    const char *filename,	/* input file name of error */
    const char *funcname,	/* input function name of error */
    cmod_int_t lineno,		/* input line number of error */
    cmod_bool_t fatal,	  	/* input if a fatal error */
    const char *str1,		/* input leading error text */
    const char *str2)		/* input trailing error text, or NULL */
{
    /* Printout for the user: suppress filename and line number */
    if (isupper((int)str1[0]))
	fprintf(stderr, "%s%s\n", str1, (str2 == NULL ? "" : str2));

    /* Printout of internal error */
    else
	fprintf(stderr, "%s(): %s%s (%s, %d)\n",
		funcname, str1, (str2 == NULL ? "" : str2), filename, lineno);

    /* Fatal */
    if (fatal) {
	fprintf(stderr, "Fatal error\n");
	abort();
	}
    }
