/******************************************************************************
*                                                                             *
*		JPL Stereo Vision code					      *
*									      *
*		Developed by the Machine Vision and Tracking Sensors Group    *
*		at the Jet Propulsion Laboratory			      *
*								  	      *
*									      *
*	For information contact:					      *
*									      *
*			Larry Matthies	lhm@robotics.jpl.nasa.gov	      *
*                      	Todd Litwin     litwin@robotics.jpl.nasa.gov          *
*			Mark Maimone	mark.maimone@jpl.nasa.gov	      *
*			Yalin Xiong	yx@robotics.jpl.nasa.gov	      *
*								  	      *
*                                       Updated: 16 Nov 1998                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
******************************************************************************/

#ifndef __ERR_HANDLE__
#define __ERR_HANDLE__
/* MSP/MUSES-CN Rovers, Jet Propulsion Laboratory
   $Id: ErrHandle.h,v 1.17 2003/04/18 08:41:39 mwm Exp $
   Error-handling definitions used in vision/navigation subsystem only */

/* Return values from vision/navigation functions */
#define NO_ERR			1
#define FILE_ERR		2
#define MEM_ERR			3
#define PARAM_ERR		4
#define INIT_ERR		5
#define INTERNAL_ERR		6
#define TYPE_MATCH_ERR	 	7
#define CONTENT_ERR		8

#ifdef DEBUG_MEM
#define BOUND(x, low, up)     do {if (x < low || x > up) { \
             FatalErr("memory out of bound\n"); \
             return MEM_ERR; }} while (0)
#else
#define BOUND(x, low, up)     
#endif

#ifndef true
#define true 1
#endif

#ifndef false
#define false 0
#endif

/* eliminate spurious Green Hills compiler warning about "if (1)" */
#ifdef _GREEN_TOOL
#pragma ghs nowarning 236 /* "controlling expression is constant" */
#endif

/* Handle Athena rover flight software errors */

#if defined(MSP)

#include "rover.h"
#include <stdio.h>

/* Map the vision/nav error functions Warning/Message/FatalErr into
   MSR functions */
#define Warning(args...) (printf (args), error(ERR(0), ERR_MINOR, ERF_COMMAND, ## args))
#define Message(args) (DBG((args)), 0)
#define FatalErr(args...) (printf (args), error(ERR(0), ERR_FATAL, ERF_COMMAND, ## args))
#else /* not defined(MSP) */

/* COPY OF err.h INCLUDED HERE FOR NON-MSP USE ONLY!!

MSP/MUSES-CN Rovers, Jet Propulsion Laboratory
Error-handling component public definitions */

/* error severity levels */
enum ErrSeverity {
 	ERR_INFO 	= 0,	/* reporting interesting event */
	ERR_MINOR	= 1,	/* recoverable problem */
	ERR_MAJOR	= 2,	/* system error without recovery */
	ERR_FATAL	= 3	/* software critical failure */
};


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

int Warning (char *c);
int Message (char *c);
int FatalErr (char *c);

void err_print(int code, enum ErrSeverity severity, const char *srcloc,
		int srcline, ...);

#ifdef __cplusplus
}
#endif /* __cplusplus */


/* report anomaly (debug, flight)
   example usage:
   error(ERR(3), ERR_MINOR, ERF_COMMAND, "Block %d not defined", blockID);
   */

#define ERR(x) (x)
#ifndef DBG
#define DBG(x) printf x
#endif
#ifndef VDBG
#define VDBG(b,x) DBG (x)
#endif
#ifndef BIT00
#define BIT00 1
#endif
#ifndef BIT01
#define BIT01 2
#endif
#ifndef BIT02
#define BIT02 4
#endif
#ifndef BIT03
#define BIT03 8
#endif
#ifndef BIT04
#define BIT04 16
#endif
#define BIT_INIT BIT00

#if defined(__GNUC__) && !defined(LINUX)
#define error(code,sev,flags,format,args...) \
		(err_print(code, sev, \
			 __FILE__, __LINE__, format , ## args))
#else
     /* HACK HACK HACK -- need to clean this up */
#define ERF_COMMAND 0

#ifdef __cplusplus
extern "C" {
#endif
void error(int code, enum ErrSeverity severity, int flags, ...);
#ifdef __cplusplus
}
#endif

#endif



#endif /* defined(MSP) */

#if 0

#ifndef MM_FILENAME
#ifdef __ghs__
#define MM_FILENAME __BASE__
#else /* __ghs__ */
#define MM_FILENAME __BASE_FILE__
#endif /* __ghs__ */
#endif /* MM_FILENAME */

#ifdef MER

#define ECMD		EVR_COMMAND
#define EINFO		EVR_INFO
#define EWARNING	EVR_WARNING
#define EFAULT		EVR_FAULT
#define EFATAL		EVR_FATAL

#define EPRINT0(lev,verb,vlev,tag,msg) \
	do { if ((verb) >= (vlev)) \
	   EVR_REPORT_EVENT_0 (lev, tag, MM_FILENAME, msg) } while (0)
#define EPRINT1(lev,verb,vlev,tag,msg,a1) \
	do { if ((verb) >= (vlev)) \
	   EVR_REPORT_EVENT_1 (lev, tag, MM_FILENAME, msg,a1) } while (0)
#define EPRINT2(lev,verb,vlev,tag,msg,a1,a2) \
	do { if ((verb) >= (vlev)) \
	   EVR_REPORT_EVENT_2 (lev, tag, MM_FILENAME, msg,a1,a2) } while (0)
#define EPRINT3(lev,verb,vlev,tag,msg,a1,a2,a3) \
	do { if ((verb) >= (vlev)) \
	   EVR_REPORT_EVENT_3 (lev, tag, MM_FILENAME, msg,a1,a2,a3) } while (0)
#define EPRINT4(lev,verb,vlev,tag,msg,a1,a2,a3,a4) \
	do { if ((verb) >= (vlev)) \
	   EVR_REPORT_EVENT_4 (lev, tag, MM_FILENAME, msg,a1,a2,a3,a4) } while (0)
#define EPRINT5(lev,verb,vlev,tag,msg,a1,a2,a3,a4,a5) \
	do { if ((verb) >= (vlev)) \
	   EVR_REPORT_EVENT_5 (lev, tag, MM_FILENAME, msg,a1,a2,a3,a4,a5) } while (0)
#define EPRINT6(lev,verb,vlev,tag,msg,a1,a2,a3,a4,a5,a6) \
	do { if ((verb) >= (vlev)) \
	   EVR_REPORT_EVENT_6 (lev, tag, MM_FILENAME, msg,a1,a2,a3,a4,a5,a6) } while (0)
#else

#define ECMD		"Cmd "
#define EINFO		"Info"
#define EWARNING	"Warn"
#define EFAULT		"Falt"
#define EFATAL		"Ftal"

#define EPRINT0(lev,verb,vlev,tag,msg) \
	do { if ((verb) >= (vlev)) \
	   printf (lev " " MM_FILENAME ": " msg); } while (0)
#define EPRINT1(lev,verb,vlev,tag,msg,a1) \
	do { if ((verb) >= (vlev)) \
	   printf (lev " " MM_FILENAME ": " msg,a1); } while (0)
#define EPRINT2(lev,verb,vlev,tag,msg,a1,a2) \
	do { if ((verb) >= (vlev)) \
	   printf (lev " " MM_FILENAME ": " msg,a1,a2); } while (0)
#define EPRINT3(lev,verb,vlev,tag,msg,a1,a2,a3) \
	do { if ((verb) >= (vlev)) \
	   printf (lev " " MM_FILENAME ": " msg,a1,a2,a3); } while (0)
#define EPRINT4(lev,verb,vlev,tag,msg,a1,a2,a3,a4) \
	do { if ((verb) >= (vlev)) \
	   printf (lev " " MM_FILENAME ": " msg,a1,a2,a3,a4); } while (0)
#define EPRINT5(lev,verb,vlev,tag,msg,a1,a2,a3,a4,a5) \
	do { if ((verb) >= (vlev)) \
	   printf (lev " " MM_FILENAME ": " msg,a1,a2,a3,a4,a5); } while (0)
#define EPRINT6(lev,verb,vlev,tag,msg,a1,a2,a3,a4,a5,a6) \
	do { if ((verb) >= (vlev)) \
	   printf (lev " " MM_FILENAME ": " msg,a1,a2,a3,a4,a5,a6); } while (0)

#endif
#endif /* 0*/

#endif /*__ERR_HANDLE__ */
