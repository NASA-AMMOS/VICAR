/* From /ware/proj/development/include/image */

#define NO_DATA         0                       // data types
#define BIT_DATA        1
#define CHAR_DATA       2
#define UCHAR_DATA      3
#define SHORT_DATA      4
#define USHORT_DATA     5
#define LONG_DATA       6
#define ULONG_DATA      7
#define FLOAT_DATA      8
#define DOUBLE_DATA     9
#define COMPLEX_DATA    10

#define REQUEST_HEADER  0
#define REQUEST_DATA     1

typedef struct
  {
  int Nbands, Nlines, Nsamples, typeSize, type, Nlevels, hiResLevel, tileSize;
  } DataSetInfo;


/*
 *  The Server expects a IssHeader to appear on the socket. Following
 *  the header, are IssTile structures, one for each tile requested.
 *  The number of requests following the header can be calculated
 *  from the Header by summing the numTypeNRequests fields in 
 *  the IssHeader. 
 *
 *  There is no padding between the last IssTile and the next IssHeader.
 */

/* Data structure that is sent to the ISS the precedes every list of tiles */
typedef struct
{
    int numType0Requests;   /* Number of "send right away" tiles */
    int numType1Requests;   /* Number of "send if you have time" tiles */
    int numType2Requests;   /* Number of "move to your local cache" tiles */
    unsigned long tv_sec;   /* Timestamp of request (secs) */
    unsigned long tv_usec;  /* Timestamp of request (usecs) */
}IssHeader;

/*
 * Data structure that is sent to the ISS for each tile requested.
 *
 */

typedef struct
  {
  long x;       /* X coordinate of tile */
  long y;       /* Y coordinate of tile */
  char type;    /* Type of tile */
  char res;     /* Resolution number of the tile */
  char setId;   /* Set Id */			/* not used by local isr */
  char sid;     /* Session ID */ 		/* not used by local isr */
  } TsTileId;

/*
 * Data structure for tile data.	Gone.
 *
 */
