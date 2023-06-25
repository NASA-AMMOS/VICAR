#ifndef _AEJERROR_H
#define _AEJERROR_H
// aejError.h 1.3 02/07/10 12:48:08
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@ri.cmu.edu)
 + DATE:    4-Feb-98
 + PURPOSE: aej error library function prototypes 
 **/

void HandleError(const char*);
void AnnounceFunctionStart(const char*);
void AnnounceFunctionEnd(const char*);
void PrintCommandLine(int, char**);
char* Index2Char(int,char*);
char* Index3Char(int,char*);

#endif



