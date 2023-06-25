/*
 * Definitions for TCP and UDP client/server programs.
 */

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include        <sys/un.h>
#include	<netinet/in.h>
#include	<arpa/inet.h>

/*
#define	SERV_UDP_PORT	6000
#define	SERV_TCP_PORT	6000
*/

#define	SERV_TCP_PORT	5001
#define	SERV_HOST_ADDR	"128.149.90.18"	/* host addr for server */

extern char	*pname;
