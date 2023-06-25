/*
 * This is a 4.3BSD SIGCLD signal handler that can be used by a
 * server that's not interested in its child's exit status, but needs to
 * wait for them, to avoid clogging up the system with zombies.
 *
 * Beware that the calling process may get an interrupted system call
 * when we return, so they had better handle that.
 */

#include	"ds/systype.h"

#include	<sys/wait.h>
#include	<signal.h>

void sig_child()
{
#ifdef	BSD
	/*
	 * Use the wait3() system call with the WNOHANG option.
	 */

	int		pid;
	union wait	status;

	while ( (pid = wait3(&status, WNOHANG, (struct rusage *) 0)) > 0)
		;
#endif
}
