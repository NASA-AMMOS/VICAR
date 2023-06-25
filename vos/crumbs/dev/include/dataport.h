// dataport.h 1.8 01/10/11 13:30:45
// This class represents a port over which to transfer data.
// FILE_Dataport is always defined.
// Define symbols DAM_DATAPORT and/or PVM_DATAPORT to support those options.

#ifndef	_DATAPORT_H_
#define _DATAPORT_H_

#include <stdio.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#define	BASE_PORT	1
#define	FILE_PORT	2
#ifdef DAM_DATAPORT
//#define DAM_PORT	3	/* disabled for now */
#endif
#ifdef PVM_DATAPORT
#define	PVM_PORT	4
#endif

class Dataport {	// abstract base class

    protected:		// hidden to prevent base class instantiation
	Dataport() { }

    public:
	virtual	FILE*	open(FILE *)=0;
	virtual	FILE*	open(const char *)=0;
	virtual	FILE*	ropen(const char *)=0;
	virtual	FILE*	wopen(const char *)=0;
	virtual	FILE*	rwopen(const char *)=0;
	virtual	void	close(void)=0;
	virtual	void	flush(int=0)=0;
	virtual int	get_next_string(char *)=0;
	virtual int	put_string(const char *)=0;
	virtual	int	pgetc(void)=0;
	virtual int	write(void *, unsigned)=0;
	virtual int	read(void *, unsigned)=0;
	virtual	int	get_type(void) { return(BASE_PORT); }
};


class FILE_Dataport : public Dataport {

    private:

    protected:

	FILE	*fp;

	void	init() { fp = NULL; }

    public:

	virtual	FILE*	open(FILE *fpin) { if(fp)close(); return(fp = fpin); }
	virtual	FILE*	open(const char *ref) { if(fp)close(); return(fp = fopen(ref,"r+")); }
	virtual	FILE*	ropen(const char *ref) { if(fp)close(); return(fp = fopen(ref,"r")); }
	virtual	FILE*	wopen(const char *ref) { if(fp)close(); return(fp = fopen(ref,"w")); }
	virtual	FILE*	rwopen(const char *ref) { if(fp)close(); return(fp = fopen(ref,"r+")); }
	virtual	void	close(void) { if(fp)fclose(fp); fp = NULL; }
	virtual	void	flush(int=0) { if(fp)fflush(fp); }
	virtual int	get_next_string(char *);
	virtual int	put_string(const char *);
	virtual	int	pgetc(void) { 
		if(fp)return(getc(fp)); 
		else return(0);
	}
	virtual int	write(void *buf, unsigned nbytes) {
				if (!fp) return -1;
				return fwrite(buf, 1, nbytes, fp);
			}
	virtual int	read(void *buf, unsigned nbytes) {
				if (!fp) return -1;
				return fread(buf, 1, nbytes, fp);
			}
	virtual	int	get_type(void) { return(FILE_PORT); }

	FILE_Dataport() { 
		fp = NULL;
		init(); 
	}
	~FILE_Dataport() {
		if(fp) {
			fflush(fp);
			fclose(fp);
		}
	}
};

#ifdef DAM_DATAPORT
#include "ds/dataport.h"
#endif

#ifdef PVM_DATAPORT
#include "net/dataport.h"
#endif

// support runtime creation of subclass objects
Dataport	*dataport_create(int);

// token input/output
int get_next_token(Dataport *fp, char *buff);
int put_token(Dataport *fp, const char *buff);

#endif
