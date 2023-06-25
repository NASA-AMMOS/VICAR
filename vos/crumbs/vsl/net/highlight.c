/*
	highlight.c

This is just a simple program to convert \^ in the input stream
to highlight characters in the output stream.  This is used to
process text files to create man pages with highlighted text.
It also generates underlined text between pairs of \_ marks.
*/

#include <stdio.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

void print_usage(char *progname)
{
	fprintf(stderr,"\nUsage: %s [-/-h/-help] < in_file > out_file\n", progname);
	fprintf(stderr,"  -/-h/-help  :print this message\n");
	fprintf(stderr,"\n%s filters incoming text to add underline and bold\n", progname);
	fprintf(stderr,"controls to the stream.  Text enclosed in \\_ will be\n");
	fprintf(stderr,"underlined and text enclosed in \\^ will be boldface\n");
	fprintf(stderr,"after going through ul -b as used by man.\n");
	fprintf(stderr,"Any text selected for bold and underline will be bold only.\n\n");
}

void putout(int c, int flag, int ulflag)
{
	int	o = 8, ul=95;
	if(flag) {	
		putc(c, stdout);
		putc(o, stdout);
	}
	putc(c, stdout);
	if(ulflag) {
		putc(o, stdout);
		putc(ul, stdout);
	}
}

main(int argc, char **argv)
{
	int	c, flag, ulflag, o = 8, ul=95, bs=92;

	for(c=1; c<argc; c++) {
		if(!strcmp(argv[c], "-") || !strcmp(argv[c], "-h") || !strcmp(argv[c], "-help")) {
			print_usage(argv[0]);
			exit(0);
		} else {
			fprintf(stderr,"Whoops - Unrecognized argument %s\n\n", argv[c]);
			print_usage(argv[0]);
			exit(0);
		}
	}
	c = getc(stdin);
	flag = FALSE;
	ulflag = FALSE;
	while(!feof(stdin)) {
		if(c == '\\') {
			c = getc(stdin);
			if(c == '^') {
				flag = ! flag;
				c = getc(stdin);
			} else if(c == '_') {
				ulflag = ! ulflag;
				c = getc(stdin);
			} else if(c == '\\') {
				c = getc(stdin);
				putout(bs, flag, ulflag);
			} else {
				putout(bs, flag, ulflag);
			}
		} else {
			putout(c, flag, ulflag);
			c = getc(stdin);
		}
	}
}
