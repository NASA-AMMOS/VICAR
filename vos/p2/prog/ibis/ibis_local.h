/*
 * ibis_local.h: Local Include file for program IBIS
 */

#ifndef _ibis_local_h
#define  _ibis_local_h 1

/* INCLUDES */
#include "ibisfile.h"
#include "ibiserrs.h"
#include <ctype.h>

/* DEFINES */
#ifndef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif
#define MAX_FMT 1024
#define MAX_COLUMN 100
#define MAXGRPNAME 32
#define MAX_DATA 100
#define MAX_FILE 20
#define DBUFSIZE 1000
#define IBUFSIZE (DBUFSIZE*sizeof(double))
#define MAXCOLSIZE 80
#define ROWSIZE 250
#define MAXCOLPERROW 50

/* DECLARES */
extern int gr1dim;
char *new_format_string(/* int nc,char * deffmt,char *str */);
void new_file(void);
void modify_group(void);
void list_file(void);
int copy_file(void);
int catenate_files(void);
int write_data(int ibis);
void pre_format(int ibis);

#endif /* _ibis_local_h */

