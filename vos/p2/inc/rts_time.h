#ifndef  RTS_TIME_INCLUDED
#define  RTS_TIME_INCLUDED	1

/**  Copyright (c) 2000, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#ifdef __cplusplus
extern "C" {
#endif


typedef	struct	{
		char	Buffer[6];
		long	Days;		/* True SFOC time uses only 16-bits */
		long	MilliSeconds;
		} SfocTime_typ;

/***  Function Prototypes  ***/
char	*rts_utc_time();
int	CompareSfocTime( SfocTime_typ *, SfocTime_typ * );
int	ExtractSfocTimeBuffer( SfocTime_typ * );
void	MaxSfocTime( SfocTime_typ *, SfocTime_typ * );
void	MinSfocTime( SfocTime_typ *, SfocTime_typ * );
int	PackSfocTimeBuffer( SfocTime_typ * );
char	*SfocTimeToAscii( SfocTime_typ *, int );
SfocTime_typ	*AsciiToSfocTime( char *, int );

#ifdef __cplusplus
}
#endif

#endif
