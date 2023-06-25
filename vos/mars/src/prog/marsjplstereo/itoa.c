#include <string.h>
#include "mwm.h"

/* itoa -- int to ascii string

	The input integer will be converted to a printable representation
   and stored in   buffer.
*/

#ifndef NULL
# define NULL ((char *) 0)
#endif

void itoa_rev (char *str);

char *itoa (int num, char *buffer)
{
    int i, sign;
    static char my_buf[50];

/* Use a static buffer if none was given */

    if (buffer == NULL)
	buffer = my_buf;

    sign = num;		/* record sign */

    if (num < 0) num = -num;
    i = 0;

/* Generate the digits in reverse order */

    do {
        buffer[i++] = num % 10 + '0';	/* Store the next digit */
    } while ((num /= 10) != 0);		/* Remove the digit from   num   */

    if (sign < 0)
	buffer[i++] = '-';

    buffer[i] = '\0';
    itoa_rev (buffer);

    return buffer;
} /* itoa */

void itoa_rev (char *str)
{
    int temp, i, j;

    for (i = 0, j = strlen (str) - 1; i < j; i++, j--) {
	temp = str[i];
	str[i] = str[j];
	str[j] = temp;
    } /* for */
}/* itoa_rev */
