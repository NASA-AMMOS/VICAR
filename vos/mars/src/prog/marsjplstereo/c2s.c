/* c2s -- character to string

	The input character is converted to a printable representation and
   written to the buffer   string.   Newlines and spaces are unchanged.

   EXAMPLES:
decimal ASCII code	string		decimal ASCII code	string
	0		^@			128		M-^@
	1		^A			129		M-^A
	9		TAB			137		M-TAB
	10		<newline>		138		M-\n
	13		RETURN			141		M-RETURN
	26		^Z			154		M-^Z
	32		<space>			160		M-<space>
	65		A			193		M-A
	97		a			225		M-a
	127		DEL			255		M-DEL
*/
#include <stdio.h>
#include <ctype.h>		/* for isprint() */
#include "mwm.h"		/* for must_malloc */
#ifdef __unix__
#include <strings.h>		/* for strcpy(3), strchr() */
#include <stdlib.h>
#else /* not unix */
#include <string.h>
#include <stdlib.h>
#endif
#include <string.h>

char *sl2fmt (char *str, long len, char *buff);

char *c2s (char ch, int ws2text, char *buff)
			/* white space -> text? y/n */
{
    static char store[10], *retval;
    int force_print = ws2text;

    if (!buff)
	buff = store;	 	/* If NULL, use a static buffer area */

    retval = buff;
    if (((unsigned) ch) & 128) {/* The eighth bit is set, char is
				   non-ASCII */
	*buff++ = 'M';
	*buff++ = '-';
	force_print = 1;
	ch &= 127;
    } /* if ch > 127 || ch < 0 */

    if (force_print && ch < 32)
	switch (ch) {
	    case '\n':
		(void) strcpy (buff, "\\n");
		break;
	    case '\r':
		(void) strcpy (buff, "RETURN");
		break;
	    case '\t':
		(void) strcpy (buff, "TAB");
		break;
	    default:
		(void) sprintf (buff, "^%c", ch + 64);
		break;
	} /* switch */
    else if (ch < 32)
	switch (ch) {
	    case '\n':
	    case '\r':
	    case '\t':
	        buff[0] = ch; buff[1] = '\0';
	        break;
	    default:
	        (void) sprintf (buff, "^%c", ch + 64);
	        break;
	} /* switch */
    else if (ch < 127)
	(void) sprintf (buff, "%c", ch);
    else if (ch == 127)
	(void) strcpy (buff, "DEL");
    else {
	fprintf (stderr, "c2s:  Impossible case\n");
	*buff = '\0';
    } /* else */

    return retval;
} /* c2s */



#define isoctal(x) ((x) >= '0' && (x) <= '7')

int fmt2c (char *str, char *cp)
{
    char c;
    int len = 0;

    if (str == NULL) return 0;
    if (*str != '\\') {
	c = *str;
	len = 1;
    } else {
	len = 2;
	switch (*++str) {
	    case 'n': c = '\n'; break;
	    case 'r': c = '\r'; break;
	    case 'f': c = '\f'; break;
	    case 't': c = '\t'; break;
	    case 'b': c = '\b'; break;
	    case '0': if (!isoctal (str[1]) || !isoctal (str[2])) {
		    c = '\0'; break;
		} /* if */
		/* fall through */
	    case '1':
	    case '2':
	    case '3':
		if (!isoctal (str[1]) || !isoctal (str[2])) {
		    c = *str;
		} else {
		    c = (str[0] - '0') * 64 + (str[1] - '0') * 8 +
			    (str[2] - '0');
		    len = 4;
		} /* else */
		break;
	    default:
		c = *str;
		break;
	} /* switch */
    } /* else */

    if (cp) *cp = c;
    return len;
} /* fmt2c */


char *fmt2s (char *str, char *buf)
{
    static char *store = NULL;
    static int len = 0;
    int thislen;
    char *result;

    if (str == NULL) return NULL;

    if (buf == NULL) {
	thislen = strlen (str);
	if (thislen >= len) {
	    if (store) free (store);
	    len = thislen + 1;
	    store = must_malloc (len, "fmt2s buffer");
	} /* if */
	buf = store;
    } /* if */
    result = buf;

    while (*str) {
	str += fmt2c (str, buf++);
    } /* while *str */
    *buf = '\0';
    return result;
} /* fmt2s */


static unsigned char special_chars[256];	/* Initializes to zero!! */

static void init_special_chars (void)
{
    unsigned char *pat = (unsigned char *) "\n\r\f\t\b\\";
    unsigned char *act = (unsigned char *) "nrftb\\";

    while (*pat)
	special_chars[*pat++] = *act++;
} /* init_special_chars */


char *sl2fmt (char *str, long len, char *buff)
{
    static char *store = NULL;
    static long storelen = 0;
    char *save, *end;
#ifdef ExpensiveUseOfStrchr
    char *p;
#endif

    if (str == NULL || len == 0L) return "";

    if (special_chars['\n'] == 0) init_special_chars ();

    if (buff == NULL) {
	BufferLAlloc (&store, 1, &storelen, 4L * len, "sl2fmt static buffer");
	buff = store;
    } /* if buff == NULL */
    save = buff;
    end = str + len;

    for (; str < end; str++)
#ifdef ExpensiveUseOfStrchr
	if (p = strchr (pat, *str)) {
	    *buff++ = '\\';
	    *buff++ = act[p - pat];
#else
	if (special_chars[(unsigned char) *str]) {
	    *buff++ = '\\';
	    *buff++ = special_chars[(unsigned char) *str];
#endif
	} else if (isprint ((int) *str) && (*str & 0x80) == 0)
	    *buff++ = *str;
	else {
	    sprintf (buff, "\\%03o", (unsigned) (*str & 0xff));
	    buff += 4;
	} /* else */

    *buff = '\0';
    return save;
} /* sl2fmt */




char *s2fmt (char *str, char *buff)
{
    int len;

    if (str == NULL) return "";
    len = strlen (str);
    return sl2fmt (str, (long) len, buff);
} /* s2fmt */


