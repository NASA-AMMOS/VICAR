/*
 * Initialize a daemon process.
 */

#include	<stdio.h>
#include	<signal.h>
#include	<sys/param.h>
#include	<errno.h>
extern int	errno;

/* is this a BSD or SYSV system? */
#ifdef __SVR4		/* sunpro compiler */
#define SYSV
#else
#ifdef _SYSTYPE_SVR4	/* SGI compiler */
#define SYSV
#else
#ifdef linux
#define SYSV
#else
#define BSD
#endif
#endif
#endif

#ifdef BSD
#include	<sys/file.h>
#include	<sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stropts.h>
#endif

/*
 * Detach a daemon process from login session context.
 */

void daemonStart(ignsigcld)
int	ignsigcld;	/* nonzero -> handle SIGCLDs so zombies don't clog */
{
	register int	childpid, fd;

	/*
	 * If we were started by init (process 1) from the /etc/inittab file
	 * there's no need to detach.
	 * This test is unreliable due to an unavoidable ambiguity
 	 * if the process is started by some other process and orphaned
	 * (i.e., if the parent process terminates before we are started).
  	 */

	if (getppid() == 1)
  		goto out;

	/*
	 * Ignore the terminal stop signals (BSD).
	 */

#ifdef SIGTTOU
	signal(SIGTTOU, SIG_IGN);
#endif
#ifdef SIGTTIN
	signal(SIGTTIN, SIG_IGN);
#endif
#ifdef SIGTSTP
	signal(SIGTSTP, SIG_IGN);
#endif

	/*
	 * If we were not started in the background, fork and
	 * let the parent exit.  This also guarantees the first child
	 * is not a process group leader.
	 */

	if ( (childpid = fork()) < 0)
		err_sys("can't fork first child");
	else if (childpid > 0)
		exit(0);	/* parent */

	/*
	 * First child process.
	 *
	 * Disassociate from controlling terminal and process group.
	 * Ensure the process can't reacquire a new controlling terminal.
	 */

#ifdef BSD

	if (BSDsetpgrp(0, getpid()) == -1)
		err_sys("can't change process group");

	if ( (fd = open("/dev/tty", O_RDWR)) >= 0) {
		ioctl(fd, TIOCNOTTY, (char *)NULL); /* lose controlling tty */
		close(fd);
	}

#else	/* System V */

	if (setpgrp() == -1)
		err_sys("can't change process group");

	signal(SIGHUP, SIG_IGN);	/* immune from pgrp leader death */

	if ( (childpid = fork()) < 0)
		err_sys("can't fork second child");
	else if (childpid > 0)
		exit(0);	/* first child */

	/* second child */
#endif

out:
	/*
	 * Close any open files descriptors.
	 */

	for (fd = 0; fd < NOFILE; fd++)
		close(fd);

	errno = 0;		/* probably got set to EBADF from a close */

	/*
	 * Move the current directory to root, to make sure we
	 * aren't on a mounted filesystem.
	 */

	chdir("/");

	/*
	 * Clear any inherited file mode creation mask.
	 */

	umask(0);

	/*
	 * See if the caller isn't interested in the exit status of its
	 * children, and doesn't want to have them become zombies and
	 * clog up the system.
	 * With System V all we need do is ignore the signal.
	 * With BSD, however, we have to catch each signal
	 * and execute the wait3() system call.
	 */

	if (ignsigcld) {
#ifdef BSD
		int	sig_child();

		signal(SIGCLD, sig_child);	/* BSD */
#else
		signal(SIGCLD, SIG_IGN);	/* System V */
#endif
	}
}
