// dataport.C 1.7 01/10/09 19:57:58
// generic interface for data input/output
// define DAM_DATAPORT and/or PVM_DATAPORT to support those options

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "dataport.h"

Dataport	*dataport_create(int type)
{
	Dataport	*tmp=NULL;

	if(type == BASE_PORT) {
		fprintf(stderr," Creating base class dataport is a bad idea...forget it.\n");
	} else if(type == FILE_PORT) {
		tmp = new FILE_Dataport();
#ifdef DAM_PORT
	} else if(type == DAM_PORT) {
		tmp = new DAM_Dataport();
#endif

#ifdef PVM_PORT
	} else if(type == PVM_PORT) {
		tmp = new PVM_Dataport();
#endif

	} else {
		fprintf(stderr," Invalid type of dataport object requested (%d)\n", type);
	}
	return(tmp);
}

static void remove_cpp_comments(Dataport *fp, char *tbuf)
{
	int	c;

	for (; *tbuf; tbuf++) {
		if(tbuf[0] == '/' && tbuf[1] == '/') {
			*tbuf = '\0';
			do {
				c = fp->pgetc();
			} while(c != '\n' && c != EOF);
			break;
		}
	}
}

int FILE_Dataport::get_next_string(char *buff)
{
	int c;

	if(!fp)return 0;
	do {		// skip leading whitespace
		c = getc(fp);
	} while (isspace(c));
	if (c == EOF)	// hit EOF
		return 0;
	do {		// get non-whitespace
		*buff++ = c;
		c = getc(fp);
	} while (!isspace(c) && c != EOF);
	*buff = 0;	// terminate string

	remove_cpp_comments(this, buff);
	return 1;
}

int FILE_Dataport::put_string(const char *buff)
{
	if (!fp) return 0;
	int n = fputs(buff, fp);
	fputc(' ', fp);
	return n+1;
}

static char * commented(char *tbuf)
{
	for (; *tbuf; tbuf++) {
		if(tbuf[0] == '/' && tbuf[1] == '*')
			return(tbuf);
	}
	return(NULL);
}

static char * end_commented(char *tbuf)
{
	for (; *tbuf; tbuf++) {
		if(tbuf[0] == '*' && tbuf[1] == '/')
			return(tbuf);
	}
	return(NULL);
}

static void remove_comment(Dataport *fp, char *buff)
{
	char	*tbuf, *tbuf2;

	tbuf = commented(buff);
	if(tbuf) {
		tbuf2 = end_commented(tbuf);
		while(!tbuf2) {
			if (!fp->get_next_string(tbuf+2))
				return;
			tbuf2 = end_commented(tbuf);
		}
		strcpy(tbuf, tbuf2+2);
	}
}

int get_next_token(Dataport *fp, char *buff)
{
	do {
		if (!fp->get_next_string(buff))
			return 0;
		while(commented(buff))
			remove_comment(fp, buff);
	} while(!*buff);
	return 1;
}

int put_token(Dataport *fp, const char *buff)
{
	return(fp->put_string(buff));
}
