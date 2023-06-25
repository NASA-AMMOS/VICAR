/**********************************
Program path_tests.C
Created by John Wright
Created Fri Aug 16 14:45:59 2002

**********************************/

/* Include block */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "path/linearspline.h"

/* Define block */
#ifndef TRUE
#define TRUE  1
#endif
#ifndef FALSE
#define FALSE  0
#endif

/* Usage method */
void print_usage(char *name) {
	fprintf(stderr,"\nUsage: %s [-/-h/-help]\n",name);
	fprintf(stderr,"  This tool runs unit regression tests on the various classes in the path library.\n");
	fprintf(stderr,"  As of 16 Aug 2002, only wrapped_linearspline is implemented.\n");
	return;
}

/* Main */
int main(int argc, char **argv)
{
	int	i, tstatus, status=TRUE;
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-") || !strcmp(argv[i],"-h") || !strcmp(argv[i],"-help")) {
			print_usage(argv[0]);
			exit(0);
		} else {
			fprintf(stderr,"Whoops - Unrecognized option %s\n", argv[i]);
			print_usage(argv[0]);
			exit(0);
		}
	}

	tstatus = wrapped_linearspline::self_test();
	status = status && tstatus;

	if(!status) {
		fprintf(stderr,"Whoops - Path self tests failed.\n");
	}

	exit(0);
}
