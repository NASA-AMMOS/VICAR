#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "rts_typedefs.h"
#include "rts_time.h"
#include "return_status.h"

/* SFOC Epoch: 1-1-58 @ 00:00:00.000 */
#define  SFOC_EPOCH_YEAR	1958
#define  MS_PER_DAY		86400000
#define  INVLD_DATE_TIME	"1900-01-01T00:00:00.000"

/*****************************************************************************
 *                              RTS_UTC_TIME
 *
 *
 *	Determines the UTC time for the current host.  This relies on the 
 * time capability of the host.  A failure returns "1900-01-01T00:00:00
 ****************************************************************************/

char	*rts_utc_time(void)
{ time_t        Now;
  struct tm     *UtcNow;
  static char	UtcTime[256];

  strcpy(UtcTime,INVLD_DATE_TIME);

  if (time(&Now) == -1) return (UtcTime);

  UtcNow = gmtime(&Now);
  if (UtcNow == NULL) return (UtcTime);

  strftime(UtcTime,sizeof(UtcTime),"%Y-%m-%dT%H:%M:%S.000",UtcNow);

  return (UtcTime);
}

/*****************************************************************************
 *				CompareSfocTime	
 *
 *	Compares two SFOC times and returns -1, 0 or 1 for less than, equal,
 *  or greater than.
 ****************************************************************************/
int	CompareSfocTime(
  SfocTime_typ	*Upper,
  SfocTime_typ	*Test)
{
  if (Upper->Days > Test->Days) return 1;
  if (Upper->Days < Test->Days) return -1;
  if (Upper->MilliSeconds > Test->MilliSeconds) return 1;
  if (Upper->MilliSeconds < Test->MilliSeconds) return -1;

  return 0;
}

/*****************************************************************************
 *				ExtractSfocTimeBuffer	
 *
 *	Extracts the data from a 6-byte buffer and places the values into
 *  integer fields for processing.
 ****************************************************************************/
int	ExtractSfocTimeBuffer(
  SfocTime_typ	*Time)
{

  Time->Days = (Time->Buffer[0] * 256) + Time->Buffer[1];
  Time->MilliSeconds = (long)(Time->Buffer[2] * 16777216) +
                       (long)(Time->Buffer[3] * 65536) +
                       (long)(Time->Buffer[4] * 256) + (long)Time->Buffer[5];

  if (Time->Days < 0 || Time->MilliSeconds < 0 ||
      Time->MilliSeconds >= MS_PER_DAY)
     return RTN_INVLD_ARG_IGNORED;

  return RTN_NORMAL;
}

/*****************************************************************************
 *				PackSfocTimeBuffer	
 *
 *	Packs the integer values of the Sfoc Time into a 6-byte buffer for
 *  compatibility with SFOC usage.
 ****************************************************************************/
int	PackSfocTimeBuffer(
  SfocTime_typ	*Time)
{ Uword	TempShort;

  if (Time->Days > 0xFFFF)
  { memset(Time->Buffer,0,6);
    return RTN_INVLD_ARG;
  }

  TempShort = Time->Days;
  memcpy(Time->Buffer,&TempShort,2);
  memcpy(&Time->Buffer[2],&Time->MilliSeconds,4);

  return RTN_NORMAL;
}

/*****************************************************************************
 *				MaxSfocTime	
 *
 *	Determines which is the maximum SFOC time value and places the value
 *  in the first parameter/
 ****************************************************************************/
void	MaxSfocTime(
  SfocTime_typ	*Upper,
  SfocTime_typ	*Test)
{

  if (Upper->Days > Test->Days) return;
  if (Upper->Days == Test->Days &&
      Upper->MilliSeconds >= Test->MilliSeconds) return;

  memcpy(Upper,Test,sizeof(SfocTime_typ));

  return;
}

/*****************************************************************************
 *				MinSfocTime	
 *
 *	Determines which is the minimum SFOC time value and places the value
 *  in the first parameter.
 ****************************************************************************/
void	MinSfocTime(
  SfocTime_typ	*Lower,
  SfocTime_typ	*Test)
{

  if (Lower->Days < Test->Days) return;
  if (Lower->Days == Test->Days &&
      Lower->MilliSeconds <= Test->MilliSeconds) return;

  memcpy(Lower,Test,sizeof(SfocTime_typ));

  return;
}

/*****************************************************************************
 *				SfocTimeToAscii	
 *
 *	Converts a SFOC time to a PDS complient time string
 * History:
 * 6-15-1998	T. Nguyen	For Y2K task, modified to use zchk_leap().
 * 1-15-2000	A. Runkle	Removed zchk_leap ... not correct for Year 2100
 ****************************************************************************/
char	*SfocTimeToAscii(
  SfocTime_typ	*Time,
  int		Julian)
{ int	LeapYear,	/* Leap Year flag */
	LeapDays,	/* Number of Leap Days from Epoch to current year */
	Year,
	Month,
	Day,		/* Day of year/month (as required) */
	TDays = Time->Days,	/* Total Days (from SFOC construct) */
	Hour,
	Minute,
	Second,
	Milli;
  Byte	dom[12] = {31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30,  31};

  static char	Buffer[64];

  Year = TDays / 365 - 1;	/* '-1' needed in calculating 'LeapDays' */
  LeapDays = (Year+2) / 4 -	/* Added 2; 1958 is the middle of leap cycle */
             (((Year+SFOC_EPOCH_YEAR)/100) -        /* 15 non-leap centuries */
              ((Year+SFOC_EPOCH_YEAR)/400) - 15);   /* until Epoch of 1958   */
  Day = (TDays % 365) - LeapDays;
  Year += SFOC_EPOCH_YEAR + 1;
  while (Day < 0)
  { Year--;
    if (((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0)))
       Day+=366;
    else Day += 365;
  }

  Day++;        /* Added 1 to Day because it is a zero-offset number used */
                /* in a one-offset calculation */

  Hour = Time->MilliSeconds / (1000 * 60 * 60);
  Minute = (Time->MilliSeconds / (1000 * 60)) % 60;
  Second = (Time->MilliSeconds / 1000) % 60;
  Milli = Time->MilliSeconds % 1000;

  LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));

  if (Julian)
     sprintf(Buffer,"%04d-%03dT%02d:%02d:%02d.%03d",
             Year,Day,Hour,Minute,Second,Milli);
  else
  { if (LeapYear) dom[1]++;		/*  Add day in Feb. for leap year  */
    for (Month = 0; Month<12 && Day>dom[Month]; Month++) Day -= dom[Month];
    Month++;
    sprintf(Buffer,"%04d-%02d-%02dT%02d:%02d:%02d.%03d",
            Year,Month,Day,Hour,Minute,Second,Milli);
  }

  return (Buffer);
}


/*****************************************************************************
 *				AsciiToSfocTime
 *
 *	Converts a PDS complient time string to a SFOC time
 * History:
 * 6-15-1998	T. Nguyen	For Y2K task, modified to use zchk_leap().
 * 1-15-2000	A. Runkle	Removed zchk_leap ... not correct for Year 2100
 ****************************************************************************/
SfocTime_typ	*AsciiToSfocTime(
  char	*Time,
  int	Julian)
{ int	LeapYear,
	LeapDays,
	Year = SFOC_EPOCH_YEAR,
	Month = 1,
	Day = 0,
	Hour = 0,
	Minute = 0,
	Second = 0,
	Milli = 0;
  int	dts[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
  static SfocTime_typ	SfocTime = {0,0};

  if (Julian)
  { sscanf(Time,"%4d-%3dT%2d:%2d:%2d.%3d",
           &Year,&Day,&Hour,&Minute,&Second,&Milli);
  } else
  { sscanf(Time,"%4d-%2d-%2dT%2d:%2d:%2d.%3d",
           &Year,&Month,&Day,&Hour,&Minute,&Second,&Milli);
  }
  if (Year < 1958) return (&SfocTime);

  LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));

  Year--;				/*  Everything is zero-based here  */
  Month--;				/*  Everything is zero-based here  */
  Day--;				/*  Everything is zero-based here  */

  Day += dts[Month];
  if (LeapYear && Month > 1) Day++;

  LeapDays = (Year-SFOC_EPOCH_YEAR+2) / 4 - ((Year/100) - (Year/400) - 15);
  Year -= (SFOC_EPOCH_YEAR - 1);
  Day += (Year * 365) + LeapDays;
  Milli += (((Hour * 60 + Minute) * 60) + Second) * 1000;

  /* printf ("Time in days: %d\n", Day); */

  SfocTime.Days = Day;
  SfocTime.MilliSeconds = Milli;
  PackSfocTimeBuffer( &SfocTime );

  return (&SfocTime);
}
