#ifndef _pic_read_h_
#define _pic_read_h_

#ifdef __STDC__

int cpic_read(
	      char *filename,	/* input name of PIC file */
	      int *rows,			/* output number of rows */
	      int *cols,			/* output number of columns */
	      int *bands,         	/* output number of bands */
	      unsigned char **red,	/* output pointer to red buffer */
	      unsigned char **green,	/* output pointer to green buffer */
	      unsigned char **blue);

#else
int cpid_read();
#endif

#endif
