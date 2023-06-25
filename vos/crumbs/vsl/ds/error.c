/*
 * Error handling routines.
 *
 * The functions in this file are independent of any application
 * variables, and may be used with any C program.
 * Either of the names CLIENT or SERVER may be defined when compiling
 * this function.  If neither are defined, we assume CLIENT.
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<errno.h>

#include	"ds/systype.h"

#ifdef	CLIENT
#ifdef	SERVER
cant define both CLIENT and SERVER
#endif
#endif

#ifndef	CLIENT
#ifndef	SERVER
#define	CLIENT	1		/* default to client */
#endif
#endif

#ifndef	NULL
#define	NULL	((void *) 0)
#endif

char	*pname;

#ifdef	CLIENT			/* these all output to stderr */

/*
 * Print the UNIX errno value.
 */
void my_perror()
{
	char	*sys_err_str();
	fprintf(stderr, " %s\n", sys_err_str());
}

/*
 * Fatal error.  Print a message and terminate.
 * Don't dump core and don't print the system's errno value.
 *
 */

void err_quit(char *str)
{
	if (pname != NULL)
		fprintf(stderr, "%s: ", pname);
	fprintf(stderr, "%s\n", str);

	exit(1);
}

/*
 * Fatal error related to a system call.  Print a message and terminate.
 * Don't dump core, but do print the system's errno value and its
 * associated message.
 *
 */

void err_sys(char *str)
{
	if (pname != NULL)
		fprintf(stderr, "%s: ", pname);
	fprintf(stderr, "%s\n", str);

	my_perror();

	exit(1);
}

/*
 * Recoverable error.  Print a message, and return to caller.
 *
 */

void err_ret(char *str)
{
	if (pname != NULL)
		fprintf(stderr, "%s: ", pname);
	fprintf(stderr, "%s\n", str);

	my_perror();

	fflush(stdout);
	fflush(stderr);
}

/*
 * Fatal error.  Print a message, dump core (for debugging) and terminate.
 *
 */

void err_dump(char *str)
{
	if (pname != NULL)
		fprintf(stderr, "%s: ", pname);
	fprintf(stderr, "%s\n", str);

	my_perror();

	fflush(stdout);		/* abort doesn't flush stdio buffers */
	fflush(stderr);

	abort();		/* dump core and terminate */
	exit(1);		/* shouldn't get here */
}

#endif	/* CLIENT */

#ifdef	SERVER

#ifdef	BSD
/*
 * Under BSD, these server routines use the syslog(3) facility.
 * They don't append a newline, for example.
 */

#include	<syslog.h>

#else	/* not BSD */
/*
 * There really ought to be a better way to handle server logging
 * under System V.
 */

#define	syslog(a,b)	fprintf(stderr, "%s\n", (b))
#define	openlog(a,b,c)	fprintf(stderr, "%s\n", (a))

#endif	/* BSD */

char	emesgstr[255] = {0};	/* used by all server routines */

/*
 * Identify ourself, for syslog() messages.
 *
 * LOG_PID is an option that says prepend each message with our pid.
 * LOG_CONS is an option that says write to console if unable to send
 * the message to syslogd.
 * LOG_DAEMON is our facility.
 */

err_init(ident)
char	*ident;
{
	openlog(ident, (LOG_PID | LOG_CONS), LOG_DAEMON);
}

/*
 * Fatal error.  Print a message and terminate.
 * Don't print the system's errno value.
 *
 */

void err_quit(char *str)
{
	syslog(LOG_ERR, str);
	exit(1);
}

/*
 * Fatal error related to a system call.  Print a message and terminate.
 * Don't dump core, but do print the system's errno value and its
 * associated message.
 *
 */

void err_sys(char *str)
{
	my_perror();
	syslog(LOG_ERR, str);
	exit(1);
}

/*
 * Recoverable error.  Print a message, and return to caller.
 *
 */

void err_ret(char *str)
{
	my_perror();
	syslog(LOG_ERR, str);
}

/*
 * Fatal error.  Print a message, dump core (for debugging) and terminate.
 *
 */

void err_dump(char *str)
{
	my_perror();
	syslog(LOG_ERR, str);

	abort();		/* dump core and terminate */
	exit(1);		/* shouldn't get here */
}

/*
 * Print the UNIX errno value.
 * We just append it to the end of the emesgstr[] array.
 */

my_perror()
{
	register int	len;
	char		*sys_err_str();

	len = strlen(emesgstr);
	sprintf(emesgstr + len, " %s", sys_err_str());
}

#endif	/* SERVER */

			/* remainder is for both CLIENT and SERVER */
extern const int	sys_nerr;	/* # of error message strings in sys table */
/* extern char	*sys_errlist[];*/	/* the system error message table */

#ifdef	SYS5
int	t_errno;	/* in case caller is using TLI, these are "tentative
			   definitions"; else they're "definitions" */
int	t_nerr;
char	*t_errlist[1];
#endif


/*
 * Return a string containing some additional operating-system
 * dependent information.
 * Note that different versions of UNIX assign different meanings
 * to the same value of "errno" (compare errno's starting with 35
 * between System V and BSD, for example).  This means that if an error
 * condition is being sent to another UNIX system, we must interpret
 * the errno value on the system that generated the error, and not
 * just send the decimal value of errno to the other system.
 */

char *
sys_err_str()
{
	static char	msgstr[200];

	if (errno != 0) {
/*
		if (errno > 0 && errno < sys_nerr)
			sprintf(msgstr, "(%s)", sys_errlist[errno]);
		else
			sprintf(msgstr, "(errno = %d)", errno);
*/
		sprintf(msgstr, "(%s)", strerror(errno));
	} else {
		msgstr[0] = '\0';
	}

#ifdef	SYS5
	if (t_errno != 0) {
		char	tmsgstr[100];

		if (t_errno > 0 && t_errno < t_nerr)
			sprintf(tmsgstr, " (%s)", t_errlist[t_errno]);
		else
			sprintf(tmsgstr, ", (t_errno = %d)", t_errno);

		strcat(msgstr, tmsgstr);	/* catenate strings */
	}
#endif

	return(msgstr);
}
