/*
 * Client using TCP protocol.
 */

#include	<string.h>
#include	"ds/ds.h"
#include	"ds/inet.h"

/* For gethostent */
     #include <sys/types.h>
     #include <sys/socket.h>
     #undef NO_DATA
     #include <netdb.h>

#include "ds/client.h"

#include <strings.h>
#include <unistd.h>

static int verbose = 0;

int black[10] = {0,0,0,0,0,0,0,0,0,0};

/***********************************************************************
 ***********************************************************************
 ***********************************************************************
 ***********************************************************************
 ***********************************************************************
 ***********************************************************************/

#define CACHE_SIZE        64

static struct {
   char data[128][128][3];
   long line, sample;
   int level;
   } TileCache[CACHE_SIZE];

static int CacheIndex;

static IssHeader issHeader;
static TsTileId issTile;
/* static TileData tileData; */


char datasetPath[256];
static DataSetInfo dataInfo;

/*---------------------------------------------------------------------

   RequestHeader(server descriptor, int *Nbands, *Nlines, *Nsamples,
		*type, *Nlevels)

   Returns -1 on failure.

---------------------------------------------------------------------*/
/* type == "band" */
int requestHeader(int sockfd, int *Nbands, int *Nlines, int *Nsamples,
		int *typeSize, int *type, int *Nlevels, int *tileSize)
{
int numRequests;
int headerData[128];

/*   BASIC STATEGEY
     ---------------
        write to server's socket.
        wait for response (Synchronous!).
        returns response.
*/

if (sockfd < 0)
  return -1;

/* Request information. */

/* Write header. */

if (verbose) {
fprintf(stderr, "CLIENT: Sending header ...\n");
fprintf(stderr, "CLIENT: Size is %d\n", sizeof(IssHeader));
}

if (writen(sockfd, (char *) &issHeader, sizeof(IssHeader)) != sizeof(IssHeader))
  {
  perror("CLIENT: requestData(): Error when writing out the head of a tile request.");
  return -1;
  }

if (verbose)
fprintf(stderr, "CLIENT: Sent header ...\n");

/* Write the REQUEST_HEADER value into the tile request record. */
issTile.type = REQUEST_HEADER;

numRequests = 1;
if (writen(sockfd, (char *) &issTile, sizeof(TsTileId) * numRequests) !=
                                                sizeof(TsTileId) * numRequests)
  {
  perror("CLIENT: requestHeader(): Error when writing out the tile requests.");
  return -1;
  }

if (readn(sockfd, (char *) headerData, 7*4) != 7*4)
  { 
  perror("CLIENT: requestHeader(): Error reading header.");
  return -1;
  }

/*fprintf(stderr, "CLIENT: header samples: %x %x %x %x\n",
headerData[5],headerData[6],headerData[7],headerData[8]);*/

*Nbands = headerData[0];
*Nlines = headerData[1];
*Nsamples = headerData[2];
*typeSize = headerData[3];
*type = headerData[4];
*Nlevels = headerData[5];
*tileSize = headerData[6];

/* requestHeader must be made before after dsOpen,
	BEFORE any other data is requested. */

dataInfo.Nbands = headerData[0];
dataInfo.Nlines = headerData[1];
dataInfo.Nsamples = headerData[2];
dataInfo.typeSize = headerData[3];
dataInfo.Nlevels = headerData[5];
dataInfo.tileSize = headerData[6];

if (verbose)
fprintf(stderr, "CLIENT: In request_header(): header = %d %d %d %d %d %d %d\n",
	headerData[0], headerData[1], headerData[2],
	headerData[3], headerData[4], headerData[5],
	headerData[6]);

return(1);
}

/*---------------------------------------------------------------------

   requestValue(server descriptor, band, level, line, sample, *data)
   level is resolution of requested data: 0 is highest resolution.
   line and sample are pixel positions.
   Returns -1 on failure.

---------------------------------------------------------------------*/
int requestValue(int sockfd, int band, int level, long line, long sample,
			void **data)
{
int r;

if ((r=requestPixel(sockfd, level, line, sample, data)) == -1)
  {
  fprintf(stderr, "CLIENT: requestValue: Error in requestPixel");
  return r;
  }

*data = (void *) &(((char *) *data)[band]);

if (verbose)
fprintf(stderr, "Single value extracted is %x\n", *((char *) *data));
return r;
}

/*---------------------------------------------------------------------

   RequestPixel(server descriptor, level, line, sample, *data)
   level is resolution of requested data: 0 is highest resolution.
   line and sample are pixel positions.
   Returns -1 on failure.

---------------------------------------------------------------------*/
int requestPixel(int sockfd, int level, long line, long sample, void **data)
{
int i, numRequests, local;
long lineTile, sampleTile;

/*   BASIC STATEGEY
     ---------------
     if not in cache
        write to server's socket.
        wait for response (Synchronous!).
        returns response.
*/

if (sockfd < 0)
  return -1;

if (verbose)
fprintf(stderr, "CLIENT: socket OK\n");

if (level!=-1)
if ((level < 0) || (level>=dataInfo.Nlevels) || (line<0) || (sample<0) ||
    (line>dataInfo.Nlines/(level+1)) ||
    (sample>dataInfo.Nsamples/(level+1)))
  {
  *data = (void *) black;
  return 1;
  }

/* Map pixel (line, sample) to tile space (tile number). */
  lineTile = line/dataInfo.tileSize;
  sampleTile = sample/dataInfo.tileSize;

if (verbose)
fprintf(stderr, "CLIENT: lineTile = %d, sampleTile = %d\n",
	lineTile, sampleTile);

/* Check whether in cache. */
local = -1;
for (i=0; i<CACHE_SIZE; i++)
   if (TileCache[i].line   == lineTile &&
       TileCache[i].sample == sampleTile &&
       TileCache[i].level == level )
     {
     if (verbose) fprintf(stderr, "Found in cache : %d\n", i);
     local = i;
     break;
     }

if (verbose)
fprintf(stderr, "Local = %d\n", local);

if (local<0)
{
/* Request tile. */

/* Write header. */

if (verbose) {
fprintf(stderr, "CLIENT: Sending header ...\n");
fprintf(stderr, "CLIENT: Size is %d\n", sizeof(IssHeader));
}

if (writen(sockfd, (char *) &issHeader, sizeof(IssHeader)) != sizeof(IssHeader))
  {
  perror("CLIENT: requestData(): Error when writing out the head of a tile request.");
  return -1;
  }

if (verbose)
fprintf(stderr, "CLIENT: Sent header ...\n");

/* Write tile requests. */
issTile.x = sampleTile;
issTile.y = lineTile;
/*issTile.type = type;*/	issTile.type = REQUEST_DATA;
issTile.res = level;

numRequests = 1;
if (writen(sockfd, (char *) &issTile, sizeof(TsTileId) * numRequests) !=
                                                sizeof(TsTileId) * numRequests)
  {
  perror("CLIENT: requestData(): Error when writing out the tile requests.");
  return -1;
  }

/* Read data into cache. Block. */
local = (CacheIndex++) % CACHE_SIZE;

if (verbose)
fprintf(stderr, "CLIENT: Not found in cache. Put at : %d\n", local);

TileCache[local].line = lineTile;
TileCache[local].sample = sampleTile;
TileCache[local].level = level;
if (readn(sockfd, (char *) &(TileCache[local].data),
		dataInfo.tileSize * dataInfo.tileSize * dataInfo.typeSize) !=
		dataInfo.tileSize * dataInfo.tileSize * dataInfo.typeSize)
  {
  perror("CLIENT: requestData(): Error reading data.");
  return -1;
  }

} /* end if (local) */

if (verbose)
fprintf(stderr, "CLIENT: ds: First three bytes: Cache value = %x %x %x\n",
	TileCache[local].data[0][0][0], TileCache[local].data[0][0][1],
	TileCache[local].data[0][0][2]);

/* Extract pixel. */
*data = (void *) &(TileCache[local].data[line%dataInfo.tileSize]
					   [sample%dataInfo.tileSize]);
return 1;
}

/*---------------------------------------------------------------------

   dsOpen establishs a connection to the data server.

---------------------------------------------------------------------*/
int dsOpen(char *url)
{
char hostname[256] /*, hostaddr[256] */;
struct hostent *hostdata;

int i;

	int			sockfd;
	struct sockaddr_in	serv_addr;

/* url should be in ds://ip/path format */

if ((url[0]!='d')||(url[1]!='s')||(url[2]!=':')||(url[3]!='/')||(url[4]!='/'))
  {
  fprintf(stderr, "CLIENT: Bad protocol prefix on url\n");
  return -1;
  }

i=0;
while (i<255 && url[i+5]!='/')
  {
  hostname[i] = url[i+5];
  i++;
  }
hostname[i]=(char)0;

if (verbose)
fprintf(stderr, "CLIENT: hostname = %sXXX\n", hostname);

strcpy(datasetPath, (char *) &(url[i+5]));

if (verbose)
fprintf(stderr, "CLIENT: datasetPath = %sXXX\n", datasetPath);

hostdata = gethostbyname(hostname);

if(!hostdata)
  {
  fprintf(stderr, "CLIENT: Bad host name in url\n");
  return -1;
  }

	/*
	 * Fill in the structure "serv_addr" with the address of the
	 * server that we want to connect with.
	 */

	bzero((char *) &serv_addr, sizeof(serv_addr));
	serv_addr.sin_family      = AF_INET;
/*	serv_addr.sin_addr.s_addr = inet_addr(SERV_HOST_ADDR);*/
/*	serv_addr.sin_addr.s_addr = inet_addr(hostaddr);*/
	bcopy(hostdata->h_addr, (char *) &serv_addr.sin_addr,
		hostdata->h_length);
	serv_addr.sin_port        = htons(SERV_TCP_PORT);

	/*
	 * Open a TCP socket (an Internet stream socket).
	 */

	if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
		err_sys("CLIENT: can't open stream socket");

	/*
	 * Connect to the server.
	 */

	if (connect(sockfd, (struct sockaddr *) &serv_addr,
							sizeof(serv_addr)) < 0)
		err_sys("CLIENT: can't connect to server");

/*	str_cli(stdin, sockfd);	*/	/* do it all */
/* INSERT CLIENT CODE HERE */

/* Initialize cache. */
for (i=0; i<CACHE_SIZE; i++)
   {
   TileCache[i].line = -1;
   TileCache[i].sample = -1;
   }

CacheIndex = 0;

issHeader.numType0Requests = 1;
issHeader.numType1Requests = 0;
issHeader.numType2Requests = 0;

/* Send dataset path to server */
if (verbose)
fprintf(stderr, "CLIENT: Sending datasetPath = %s\n", datasetPath);

if (writen(sockfd, datasetPath, 256) != 256)
        {
        perror("CLIENT: tcpcli.c: Error writing dataset path on socket.");
        }

return(sockfd);
}

/*---------------------------------------------------------------------

	Pass in server descriptor.
	Returns -1 on failure.

---------------------------------------------------------------------*/

int dsClose(int sockfd)
{
return(close(sockfd));
}





