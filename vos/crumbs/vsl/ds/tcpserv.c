/*
 * Server using TCP protocol.
 */

#include	"ds/inet.h"
#include	"ds/ds.h"

#include <fcntl.h>
#include <sys/time.h>
#include <string.h>
#include <stdlib.h>

static char datasetPath[256];
static DataSetInfo dataInfo;

static char buffer[128*128*3];
static int sockfd, newsockfd;
/* static char * pname; */

        int                     clilen, childpid, servlen;
        struct sockaddr_un      cli_addr, serv_addr;

        fd_set readfds;
        struct timeval timeout;
        TsTileId requestedTileId[150];
        IssHeader requestedTileHeader;
        int totalNumRequests, readBytes;
        int i, val;

	int runLock;

extern int readn(int, char *, int);
extern int writen(int, char *, int);

static int verbose = 0;

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************

                 Subroutine to read data from disk.

*************************************************************************/

void process_request(TsTileId * requested_tile_id)
{
 fd_set readfds;
 char filename[256];
 int            i, j;
 int            r,g,b;
 int filedes;

/*
 * Data structure that is sent to the ISS for each tile requested.
 *
 *
 * typedef struct TsTileId
 *  {
 *  long x;       X coordinate of tile
 *  long y;       Y coordinate of tile
 *  char type;    Type of tile from TsTileType: TsDemType or TsOiType
 *  char res;     Resolution number of the tile
 *  char setId;   Set Id                (not used by local isr)
 *  char sid;     Session ID            (not used by local isr)
 *  } TsTileId;
 *
 *
 * Data structure for tile data.
 *
 *
 *  typedef struct TileData
 *    {
 *    TsTileId id;
 *    int      check_sum;                not used by local isr 
 *    struct timeval tv[TIMECOUNT];      not used by local isr 
 *    char data[BLKSIZE];                128 x 128 x 3 
 *    } TileData;
 */

if (requested_tile_id->type == REQUEST_HEADER)
  {
  if (verbose)
  fprintf(stderr, "SERVER: process_request(): %d %d %d %d %d %d %d %d\n",
	dataInfo.Nbands, dataInfo.Nlines, dataInfo.Nsamples,
	dataInfo.typeSize, dataInfo.type, dataInfo.Nlevels,
	dataInfo.hiResLevel, dataInfo.tileSize);

  if (verbose)
  fprintf(stderr, "SERVER: Serving header information\n");

  *((int *) &(buffer[0])) = dataInfo.Nbands;
  *((int *) &(buffer[4])) = dataInfo.Nlines;
  *((int *) &(buffer[8])) = dataInfo.Nsamples;
  *((int *) &(buffer[12])) = dataInfo.typeSize;
  *((int *) &(buffer[16])) = dataInfo.type;
  *((int *) &(buffer[20])) = dataInfo.Nlevels;
  *((int *) &(buffer[24])) = dataInfo.tileSize;

  if (writen(newsockfd, buffer, 4 * 7) != 4 * 7)
  	   perror("Error writing on socket.");

  }
else	{
	sprintf(filename, "%s/%d/tiles/p%dp%d.tile", datasetPath,
		requested_tile_id->res + dataInfo.hiResLevel,
		(int) requested_tile_id->x, (int) requested_tile_id->y);

	if (verbose)
	fprintf(stderr, "SERVER: Daemon: reading %s ...\n", filename);

	if ( (filedes = open( filename, O_RDONLY )) == -1 )
	  {
	  memset(buffer, 0,
		dataInfo.tileSize * dataInfo.tileSize * dataInfo.typeSize); 
/*
          fprintf(stderr, "SERVER: process_request(): failed open for reading\n");
          perror("SERVER: ProcessRequest: Failed open\n");
          return;
*/
          }
	else {
	/*  Read tile
	***********************************************************/
	read(filedes, buffer,
	dataInfo.tileSize * dataInfo.tileSize * dataInfo.typeSize);
	close(filedes);
	}

	if (verbose)
	fprintf(stderr,
		"SERVER: Daemon: First 3 bytes: %x %x %x\n",
		buffer[0], buffer[1], buffer[2]);

        if (writen(newsockfd, buffer,
	  dataInfo.tileSize * dataInfo.tileSize * dataInfo.typeSize) !=
	  dataInfo.tileSize * dataInfo.tileSize * dataInfo.typeSize)
          {
          perror("SERVER: Error writing on socket.");
          }
	} /* end else */

}

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

void serverTCP()
{
/*	int			sockfd, newsockfd, clilen, childpid;*/
	int			clilen, childpid;
	struct sockaddr_in	cli_addr, serv_addr;
	char filename[256];
	FILE * dataInfoPtr;

	/*	pname[0] = 'B'; pname[1] = '\0';*/

	/*
	 * Open a TCP socket (an Internet stream socket).
	 */

	if (verbose)
	fprintf(stderr, "SERVER: Opening socket...\n");

	if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
		{
		fprintf(stderr, "SERVER: can't open stream socket\n");
		err_dump("SERVER: can't open stream socket");
		}

	/*
	 * Bind our local address so that the client can send to us.
	 */

	if (verbose)
	fprintf(stderr, "SERVER: Binding socket...\n");

	bzero((char *) &serv_addr, sizeof(serv_addr));
	serv_addr.sin_family      = AF_INET;
	serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	serv_addr.sin_port        = htons(SERV_TCP_PORT);

	if (bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0)
		err_dump("SERVER: can't bind local address");

	listen(sockfd, 5);

	for ( ; ; ) {
		/*
		 * Wait for a connection from a client process.
		 * This is an example of a concurrent server.
		 */

		if (verbose)
		fprintf(stderr, "SERVER: Waiting for connection...\n");

		clilen = sizeof(cli_addr);
		newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr,
								     &clilen);
		if (newsockfd < 0)
			err_dump("SERVER: accept error");

		if ( (childpid = fork()) < 0)
			err_dump("SERVER: fork error");

		else if (childpid == 0) {	/* child process */
			close(sockfd);		/* close original socket */

/* Read dataset path from client.  Blocking read. */
if (readn(newsockfd, datasetPath, 256) != 256)
	{
        perror("SERVER: Error reading dataset path on socket.");
        exit(1);
	}

if (verbose)
fprintf(stderr, "SERVER: Received datasetPath = %s\n", datasetPath);

/* Read in data set information. */
sprintf(filename, "%s/dataSet.info", datasetPath);
dataInfoPtr = fopen(filename, "r");

if (dataInfoPtr == NULL)
  fprintf(stderr, "SERVER: Can't read %s\n", filename);
fscanf(dataInfoPtr, "%d %d %d %d %d %d %d %d",
&(dataInfo.Nbands), &(dataInfo.Nlines), &(dataInfo.Nsamples),
&(dataInfo.typeSize), &(dataInfo.type), &(dataInfo.Nlevels),
&(dataInfo.hiResLevel),&(dataInfo.tileSize));
fclose(dataInfoPtr);

if (verbose)
fprintf(stderr, "SERVER: serverTCP: %d %d %d %d %d %d %d %d\n",
dataInfo.Nbands, dataInfo.Nlines,
dataInfo.Nsamples, dataInfo.typeSize, dataInfo.type, dataInfo.Nlevels,
dataInfo.hiResLevel,dataInfo.tileSize);

if (verbose)
fprintf(stderr, "SERVER: Going into request read loop...\n");

	runLock = 1;
	while (runLock) {

		if (verbose)
		fprintf(stderr, "SERVER: Blocking read...\n");

	  /* Try to read an IssHeader on socket. Blocking read. */
	  if ((readBytes = readn(newsockfd, (char *) &requestedTileHeader,
	       sizeof(IssHeader))) != sizeof(IssHeader))
	    {
            fprintf(stderr,"SERVER: Only read %d bytes of header."
                           " Expected %d bytes.\n", 
                           readBytes, sizeof(IssHeader));
            perror("DATA SERVER\n");
            exit(1);
            }

          totalNumRequests =
            requestedTileHeader.numType0Requests +
            requestedTileHeader.numType1Requests +
            requestedTileHeader.numType2Requests;

		if (verbose)
		fprintf(stderr, "SERVER: Number of requests is %d\n",
							totalNumRequests);

          /* Read the request list on socket. Blocking read. */
          if ((readBytes = readn(newsockfd, (char *) &(requestedTileId[0]),
               totalNumRequests * sizeof(TsTileId))) !=
               totalNumRequests * sizeof(TsTileId))
            {
            fprintf(stderr,"DATA SERVER: Only read %d bytes of list."
                             " Expected %d bytes.\n", 
                             readBytes, totalNumRequests * sizeof(TsTileId));
            perror("DATA SERVER");
            exit(1);
            }

          totalNumRequests =
            requestedTileHeader.numType0Requests +
            requestedTileHeader.numType1Requests;

          for (i=0; i<totalNumRequests; i++)
             {
             process_request(&(requestedTileId[i]));
             }

          } /* end while(runLock) */

	if (verbose)
	fprintf(stderr, "SERVER: Exit child process\n");
	exit(0);
	}  /* end if (childpid == 0) */

close(newsockfd);	/* parent process */
if (verbose)
fprintf(stderr, "SERVER: Exit parent process\n");
}

}
