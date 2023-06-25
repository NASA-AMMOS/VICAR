// config.C 1.1 02/06/14 08:32:22
/** \file
// SUMMITT configuration setup
//
// Get configuration arguments, if any, for the specified program.
// Arguments are read from the file named in environment variable
// SUMMITT_CFG, or in $HOME/.summitt if SUMMITT_CFG is not defined.
// This file contains lines in the form
//	<progname> <arg> [<arg> ...]
// Arguments from all lines referring to the specified program are
// stored (as dynamically allocated copies) in the provided list.
// Returns the number of arguments stored (there's no indication if
// more arguments were present than could be stored).
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int summitt_config(const char *program, char *args[], int maxargs)
{
	char buf[1024];

	// determine configuration file name
	char *cfname = getenv("SUMMITT_CFG");
	if (cfname == NULL) {	// SUMMITT_CFG not set, use ~/.summitt
		cfname = getenv("HOME");
		if (cfname == NULL) {
			fprintf(stderr, 
				"summitt_config: Whoops - $HOME unknown?!\n");
			return 0;
		}
		sprintf(buf, "%s/.summitt", cfname);
		cfname = buf;
	}

	// open config file
	FILE *fp = fopen(cfname, "r");
	if (fp == NULL) {
		fprintf(stderr, "summitt_config: Whoops, can't open %s\n",
			cfname);
		return 0;
	}

	// search file for relevant lines
	int plen = strlen(program);
	int argc = 0;
	while (argc < maxargs && fgets(buf, sizeof(buf), fp)) {
		if (!strncmp(buf, program, plen)) {
			// found one, extract arguments
			// ** doesn't handle quote-delimited embedded spaces
			char *tok = strtok(buf+plen, " \t\n");
			while (tok && argc < maxargs) {
				args[argc++] = strdup(tok);
				tok = strtok(NULL, " \t\n");
			}
		}
	}
	fclose(fp);
	return argc;
}

#ifdef TEST_MAIN
int main(int argc, char **argv)
{
	if (argc != 2) {
		printf("usage: %s progname\n", argv[0]);
		return 1;
	}
	char *alist[10];
	int n = summitt_config(argv[1], alist, 10);
	printf("Result = %d args\n", n);
	for (int i=0; i<n; i++)
		printf("arg %d = [%s]\n", i, alist[i]);
}
#endif
