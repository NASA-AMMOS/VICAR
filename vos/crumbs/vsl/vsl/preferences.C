// preferences.C
//
// Written by John Wright for the MUDSS task - 12/22/97
//
// Copyright (C) 1997, California Institute of Technology
// All rights reserved.

#include <iostream>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "preferences.h"
using namespace std;
// get_preference searches the file $HOME/.vsprefs for a string
// of the form fieldname = string_value and returns a pointer
// to the associated string value
char *Preferences::get_preference(const char *fieldname)
{
	char	line_buff[1024], *str;
	char	path[1024];
	FILE	*fp;

	str = NULL;
	if(!filename) {
		cout << "Preferences filename not set" << endl;
		return 0;
	}
	fp = fopen(filename,"r");
	if(fp) {
		while(!feof(fp)) {
			fgets(line_buff, 1023, fp);
			if((str = strstr(line_buff, fieldname)) && line_buff[0] != '#') {
				str += strlen(fieldname);
				path[0] = '\0';
				sscanf(str, " = %s\n", path);
				fclose(fp);
				return(strdup(path));
			}
		}
		fclose(fp);
	}
	return(0);
}
	
// set_preference searches the file $HOME/.vsprefs for a string
// of the form fieldname = string_value and replaces string_value
// If the fieldname does not appear in the file it is added
int Preferences::set_preference(const char *fieldname, const char *string_value)
{
	int	bt;
	char	line_buff[1024];
	FILE	*rfp, *wfp;

	if(!filename) {
		cout << "Preferences filename not set" << endl;
		return -1;
	}
	rfp = fopen(filename,"r");
	if(!rfp) {
		rfp = fopen(filename,"w");
		if(!rfp) {
			return(-1);
		} else {
			fprintf(rfp,"%s = %s\n", fieldname, string_value);
			fclose(rfp);
		}
	} else {
		wfp = tmpfile();
		if(!wfp) {
			fclose(rfp);
			return(-1);
		} else {
			bt = getc(rfp);
			while(!feof(rfp)) {
				fputc(bt, wfp);
				bt = getc(rfp);
			}
			fclose(rfp);
			rewind(wfp);
			rfp = fopen(filename,"w");
			if(!rfp) {
				return(-1);
			} else {
				while(fgets(line_buff, 1023, wfp)) {
					if(!strstr(line_buff, fieldname) || line_buff[0] == '#') {
						fputs(line_buff, rfp);
					}
				}
				fprintf(rfp,"%s = %s\n", fieldname, string_value);
				fclose(rfp);
			}
			fclose(wfp);
		}
	}
	return(0);
}

int Preferences::set_file(char *name, char *path)
{
	char *def = (char *)"./";
	char *home = 0;
	if (path) home = path;
	else if (home = getenv("HOME"));
	else home = def;
	if (home) {
		struct stat buf;
		char *p = home + strlen(home)-1;
		if (*p == '/') *p = '\0';
		if(filename) free(filename);
		filename = (char *)malloc(strlen(name) + strlen(home) + 2);
		sprintf(filename, "%s/%s", home, name);
                int ret = stat(filename, &buf);
                if (ret) {
                        int ret1 = creat(filename, 0600);
                        if (ret1 < 0) {
                                perror("CREAT");
                                return -1;
                        } else return 1;
                } else return 0;
	} else {
		cout << "Unable to create preferences file " << endl;
		return -1;
	}
}
