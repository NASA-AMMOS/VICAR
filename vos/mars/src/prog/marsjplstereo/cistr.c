/* cistr -- Case Insensitive STRing operations */


#include <stdio.h>
#include "mwm.h"

#ifdef __cplusplus
extern "C" {
#endif

int cistrcmp (char *s1, char *s2)
{

    if (s1 == NULL || s2 == NULL) {
	fprintf (stderr, "cistrcmp: Can't compare NULL string!\n");
	return -1;
    } /* if NULL */

    while (*s1 && mklow (*s1) == mklow (*s2))
	s1++, s2++;

    return (mklow (*s1) < mklow (*s2)) ? -1 : ((mklow (*s1) == mklow (*s2)) ?
	    0 : 1);
} /* cistrcmp */



int cistrncmp (char *s1, char *s2, int n)
{
    if (n == 0)
	return 0;

    if (s1 == NULL || s2 == NULL) {
	fprintf (stderr, "cistrncmp: Can't compare NULL string!\n");
	return -1;
    } /* if NULL */

    while (--n && *s1 && mklow (*s1) == mklow (*s2))
	s1++, s2++;

    return (mklow (*s1) < mklow (*s2)) ? -1 : ((mklow (*s1) == mklow (*s2)) ?
	    0 : 1);
} /* cistrncmp */

#ifdef __cplusplus
}
#endif

