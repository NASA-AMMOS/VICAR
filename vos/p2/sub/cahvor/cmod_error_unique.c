/******************************************************************************
*                                                                             *
*                       C M O D _ E R R O R _ U N I Q U E                     *
*                                                                             *
*                                       Bob Deen                              *
*                                       Written: 14 Jan 2021                  *
*                                       Updated: 14 Jan 2021                  *
*                                                                             *
*                                       Copyright (C) 2009                    *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains an error-reporting function. It is intended for
	use by the CMOD implementation and not as a public function. This
	version reports to standard I/O - except that it reports each unique
        message only once, to prevent endless e.g. "> EPSILON" errors from the
        low-level code. It is split out into a separate
	file so that custom implementations can be used in place of this
	one for target environments that need different behavior. */


#include "cmod.h"
#include "cmod_error.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define CMOD_MAX_UNIQUE_MSGS 100
static char *cmod_unique_msgs[CMOD_MAX_UNIQUE_MSGS];
static int cmod_unique_msg_count = 0;
static int cmod_unique_msg_printed = 0;

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
    char string[4095];

    /* Printout for the user: suppress filename and line number */
    if (isupper((int)str1[0]))
	sprintf(string, "%s%s\n", str1, (str2 == NULL ? "" : str2));

    /* Printout of internal error */
    else
	sprintf(string, "%s(): %s%s (%s, %d)\n",
		funcname, str1, (str2 == NULL ? "" : str2), filename, lineno);

    /* Check the database of messages to see if we have already printed this */
    /* Cribbed from PigModelBase::printUniqueStaticMsg() */

    int found = 0;
    int i;
    for (i=0; i < cmod_unique_msg_count; i++) {
        if (strcmp(string, cmod_unique_msgs[i]) == 0) {
            found++;
            break;
        }
    }
    if (cmod_unique_msg_count == 0 || found == 0) {

        fprintf(stderr, "%s", string);

        if (cmod_unique_msg_count < CMOD_MAX_UNIQUE_MSGS) {
            cmod_unique_msgs[cmod_unique_msg_count] = strdup(string);
            if (cmod_unique_msgs[cmod_unique_msg_count] == NULL) {
                fprintf(stderr, "Allocation error in cmod_error\n");
            }
            cmod_unique_msg_count++;
        }
    }
    else {
        if (!cmod_unique_msg_printed) {
            fprintf(stderr, "Non-unique cmod messages suppressed...\n");
            cmod_unique_msg_printed++;
        }
    }

    /* Fatal */
    if (fatal) {
	fprintf(stderr, "Fatal error\n");
	abort();
	}
    }
