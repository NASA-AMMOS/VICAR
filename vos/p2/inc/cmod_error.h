/******************************************************************************
*                                                                             *
*                             C M O D _ E R R O R                             *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 28 Jul 2009                  *
*                                       Updated: 24 May 2016                  *
*                                                                             *
*                                       Copyright (C) 2009, 2010, 2012, 2016  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains a declaration for an error-reporting function.
	*/


#ifndef CMOD_ERROR_H
#define CMOD_ERROR_H

#include "cmod.h"
#include <stdio.h>

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (0)
#endif

/* Assertion */
#ifndef CMOD_ASSERT
#define CMOD_ASSERT(funcname, test) \
    ((test) ? (void)0 : \
	     (void)cmod_error(__FILE__, funcname, __LINE__, TRUE, #test, NULL))
#endif

/* Assertion with one integer argument */
#ifndef CMOD_ASSERT_1
#define CMOD_ASSERT_1(funcname, test, arg) do { \
    if (!(test)) { \
	char str[128]; \
	snprintf(str, sizeof str, "; %ld", (long)(arg)); \
	cmod_error(__FILE__, funcname, __LINE__, TRUE, #test, str); \
	} \
    } while (0)
#endif

/* Assertion with two integer arguments */
#ifndef CMOD_ASSERT_2
#define CMOD_ASSERT_2(funcname, test, arg1, arg2) do { \
    if (!(test)) { \
	char str[32]; \
	snprintf(str, sizeof str, "; %ld, %ld", (long)(arg1), (long)(arg2)); \
	cmod_error(__FILE__, funcname, __LINE__, TRUE, #test, str); \
	} \
    } while (0)
#endif

/* Error reports that may be overridden */

/* Error message */
#ifndef CMOD_ERROR
#define CMOD_ERROR(funcname, str) \
    cmod_error(__FILE__, funcname, __LINE__, FALSE, str, NULL)
#endif

/* Error message with one extra integer argument */
#ifndef CMOD_ERROR_I
#define CMOD_ERROR_I(funcname, str1, arg) do { \
    char str2[32]; \
    snprintf(str2, sizeof str2, ": %ld", (long)arg); \
    cmod_error(__FILE__, funcname, __LINE__, FALSE, str1, str2); \
    } while (0)
#endif

/* Error message with two extra integer arguments */
#ifndef CMOD_ERROR_II
#define CMOD_ERROR_II(funcname, str1, arg1, arg2) do { \
    char str2[32]; \
    snprintf(str2, sizeof str2, ": %ld, %ld", (long)arg1, (long)arg2); \
    cmod_error(__FILE__, funcname, __LINE__, FALSE, str1, str2); \
    } while (0)
#endif

/* Error message with one extra string argument */
#ifndef CMOD_ERROR_S
#define CMOD_ERROR_S(funcname, str, arg) \
    cmod_error(__FILE__, funcname, __LINE__, FALSE, str, arg)
#endif

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef __STDC__

/* Error function */
void cmod_error(
    const char *filename,	/* input file name of error */
    const char *funcname,	/* input function name of error */
    cmod_int_t lineno,		/* input line number of error */
    cmod_bool_t fatal,	  	/* input if a fatal error */
    const char *str1,		/* input leading error text */
    const char *str2);		/* input trailing error text, or NULL */

#else

void cmod_error();

#endif

#ifdef	__cplusplus
}
#endif

#endif
