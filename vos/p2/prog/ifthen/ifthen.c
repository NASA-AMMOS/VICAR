#include <math.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <stdlib.h>             //64-bit def of NULL

#include "vicmain_c.h"
//#include "ms_defines.h" 
#include "mve.h"
#include "applic.h" 
#include "ftnbridge.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zvprintf.h"
#include "zmabend.h"
#include "zifmessage.h"

#include "cartoMemUtils.h"      //for mz_alloc1

#define MAXTEXT         26
#define ARITHBUF        2062
#define OPBUF           3000
#define STRINGBUF       120000
#define NUMCOLS         9    /* problem with s=9 in knoth() */
#define NUMCOLS1        NUMCOLS+1
#define NUMCOLS2        NUMCOLS*2

/* prototypes  added for 64-bit */

unsigned char ct1(unsigned char *s);
short int ct2(short int *s) ;
int ct4(int *s);
float ct7(float *s);
double ct8(double *s);

void st1(unsigned char v,unsigned char *s);
void st2(short int v,short int *s);
void st4(int v,int *s);
void st7(float v,float *s);
void st8(double v,double *s);

double ffetchcd(int k,int typ,unsigned char c_data[]);
void fstorecd(int k,int typ,double val,unsigned char c_data[]);
void stsget(int *s,char *fstrng,double *dbuf,int *cnum,char *sbuf,
	    int *sptr);
void sp_knuth(char *fstrng,int *ibuf,double *dbuf,char *sbuf,int *cnum,int *sptr,
	      int functionsize);
double ms_dnum(char **num_ptr);
void insq(char *buf,int indx,int ptr);
void delq(char *buf,int ptr);
void sp_xknuth(int *ibuf,double *dbuf,char *sbuf,int *sptr,double *result,
	       int code);

/* global variables */
int fp,cp,sbop2,nbpo,idebug,debugrec1; 
/* legal ascii characters in functions */
char cvec[64] = {'0','1','2','3','4','5','6','7','8','9',
		 '.','a','l','o',
		 'g','i','n','t','s','q','r','x','d','m','b','c','e','p',
		 'f','h','j','k','u','v','w','y','z','_','A','B',
		 'C','D','E','F','G','H','I','J','K','L','M','N',
		 'O','P','Q','R','S','T','U','V','W','X','Y','Z'};

/*=========================================================*/
/*                                                  ALZ
						    ct1, ct2, ct4, ct7, ct8, st1, st2, st4, st7, st8

						    Subroutines used with tabular data set operations
						    for type conversion and storing.  The unsigned char
						    is for image handling only.

*/
unsigned char ct1(s) unsigned char *s; { return(*s); }
short int ct2(s) short int *s; { return(*s); }
int ct4(s) int *s; { return(*s); }
float ct7(s) float *s; { return(*s); }
double ct8(s) double *s; { return(*s); }
void st1(v,s) unsigned char v,*s; { *s = v; return; }
void st2(v,s) short int v,*s; { *s = v; return; }
void st4(v,s) int v,*s; { *s = v; return; }
void st7(v,s) float v,*s; { *s = v; return; }
void st8(v,s) double v,*s; { *s = v; return; }
/*================================================================*/
/*  ffetchcd - get 8-byte value from a column   */
/*             as unsigned characters           */
/* returns value as a double                    */
double ffetchcd(k,typ,c_data)
     int k,typ;
     unsigned char c_data[];
{
  switch(typ)
    {
    case 1: return((double)ct1((unsigned char *)&c_data[k]));
    case 2: return((double)ct2((short int *)&c_data[k]));
    case 4: return((double)ct4((int *)&c_data[k]));
    case 7: return((double)ct7((float *)&c_data[k]));
    case 8: return(ct8((double *)&c_data[k]));
    }
  return(0.);
}
/*================================================================*/
/* fstorecd - store a value (double) in a column    */
/*              as a series of unsigned characters  */    
void fstorecd(k,typ,val,c_data)
     int k,typ; double val;
     unsigned char c_data[];
{
  short int x2; int x4; float x7; unsigned char x1;
  switch(typ)
    {
    case 1: x1 = (unsigned char) val;
      st1(x1,&c_data[k]); return;
    case 2: x2 = (short int) val;
      st2(x2,(short int*)&c_data[k]); return;
    case 4: x4 = (int) val;
      st4(x4,(int *)&c_data[k]); return;
    case 7: x7 = (float)val;
      st7(x7,(float *)&c_data[k]); return;
    case 8: st8(val,(double *)&c_data[k]); return;
    }
  return;
}
/*================================================================*/
/* c version 1/23/00 al zobrist ... no attempt to use c constructs,
   just a straight conversion of the fortran lines
   inputs fstrng (function string - whatever is after =  sign)
   returns s (symbol value)
     
   outputs: sbuf - string buffer for string functions
   dbuf - numeric values for evaluation
   temporarily modifies fstrng for + and - values
   modifies: sptr,fp,sp,sbop2,nbpo
   fp =  number of useful chars in fstrng after removal of "(" or ")" 

*/
void stsget(s,fstrng,dbuf,cnum,sbuf,sptr)
     int *s,*cnum,*sptr;
     char *fstrng,*sbuf;
     double *dbuf;
                                                                                                                                                        
{
  double rnum,rfac=0;
  int first,atop;
  char c,cl,minus,aop[19],intg[66];
  /* char outmsg[132]; */
  /* fcv - operator */
  int fcv[148] = {1661,1662,1663,1664,1665,1666,1667,1668,1669,16610,
		  16611,16612,16613,16614,16615,16616,16617,16618,16619,16620,
		  16621,16622,16623,16624,16625,16626,16627,16628,16629,16630,
		  16631,16632,16633,16634,16635,16636,16637,16638,16639,16640,
		  16641,16642,16643,16644,16645,16646,16647,16648,16649,16650,
		  16651,16652,16653,16654,16655,16656,16657,16658,16659,16660,
		  16661,16662,16663,16664,16665,16666,16667,16668,16669,16670,
		  16671,16672,16673,16674,16675,16676,16677,16678,16679,16680,
		  16681,16682,16683,16684,16685,16686,16687,16688,16689,16690,
		  16691,16692,16693,16694,16695,16696,16697,16698,16699,166100,
		  21282,168481,
		  13686,19357,168481,12677,1234410,12344,20117,1966,2648,1826,
		  134311,134661,13452,1358,1677,2431,2466,2452,128262,12966,
		  13648,12826,24546,24004,2627,262741,29990,25990,13474796,20474796,
		  19173,346306,146306,22883376,1991476,2848,199279,1992144,1992827,
		  153397,233397,1943,153990,283990,2449990,2449943};
  int kcv[148] = {1,2,3,4,5,6,7,8,9,10,  11,12,13,14,15,16,17,18,19,20,
		  21,22,23,24,25,26,27,28,29,30,  31,32,33,34,35,36,37,38,39,40,
		  41,42,43,44,45,46,47,48,49,50,  51,52,53,54,55,56,57,58,59,60,
		  61,62,63,64,65,66,67,68,69,70,  71,72,73,74,75,76,77,78,79,80,
		  81,82,83,84,85,86,87,88,89,90,  91,92,93,94,95,96,97,98,99,100,
		  102,101,
		  101,18,20, 8, 6, 7,16,17,18,19,  20,21,22,23, 8,20,21,22,25,26,
		  27,28,39,40,41,42,43,44,45,46,  47,48,49,50,51,52,53,54,55,56,
		  57,58,59,60,61,62};
  int cop[20] = {1,2,3,4,9,10,0,14,24,11, 29,30,31,32,33,34,35,36,37,38};
  int prior[63] = {0, 4,4,5,5,6,7,7,7,0,0, 0,0,0,1,0,7,7,7,7,1,
		   1,1,7,7,1,7,7,7,3,3, 3,3,3,3,2,2,2,7,1,1,
		   1,1,1,1,1,1,1,7,7,1, 7,1,1,1,1,1,1,7,7,1,7,7};
  int bop2[63] = {0,  1,1,1,1,1,0,0,0,0,1, 1,0,0,0,0,0,0,0,0,1,
		  1,1,0,0,1,0,0,0,1,1, 1,1,1,1,1,1,1,0,1,1,
		  1,1,1,1,1,1,1,0,0,1, 0,1,1,1,1,1,1,0,0,1,0,0};
  int isavtr[6] = {11,12,13,16,17,14};
  int type,fpx,num,snum,i,isav,isl,qtype,ipow,qret=0,isv;

  minus = '-';
  strcpy(aop,"+-*/() ,;$<=!|&>^@");				/* 17 of 19 values */
  strcpy(intg,"0123456789.alogintsqrxdmbcepfhjkuvwyz_ABC");
  strcat(intg,"DEFGHIJKLMNOPQRSTUVWXYZ'");
   
  /* temporarily, column names and functions are case insensitive, later
     the column names will become case sensitive, and this routine and
     the main program have to be modified. alz 1/26/00 */

  /*printf ("stsget: fp,cp,sbop2,nbpo = %d %d %d %d\n",fp,cp,sbop2,nbpo);      */
  atop = 0;
  first = 1;
  nbpo = 1;
  num = 0;
  snum = 0;
  rnum = 0.0;
  type = 0;
  qtype = 0;
  /* parse function */
 l100: c = fstrng[fp+1];
  if (c==aop[6]) goto l8;           /* blank */
  fpx = fp+1;
  for (i=0;i<17;i++)		/* all aop values except @ */
    {
      isav = i;
      if (c==aop[i]) goto l700;      /* if legal operator */
    }
  if (c==aop[17]) goto l704;        /* if @ */
  goto l39;
  /* legal operator eval */
 l700: if (isav<10||isav>15) goto l702;
  c = fstrng[fp+2];
  for (i=10;i<16;i++)
    {
      if (c==aop[i]) goto l703;      /* if logical operator <,=,!,|,&,>,^ */
    }
 l702: if (isav==16) isav = 18;          /* ^ to aop[18] */
  if (isav==12) isav = 19;          /* ! to aop[19] */
  if (isav==14) isav = 17;          /* & to aop[17] - overwrite @ */
  fpx = fp+1;
  goto l10;
  /* logical eval */
 l703: isav = isavtr[isav-10];
  fpx = fp+2;
  goto l10;
  /* @ eval */
 l704: atop = 1;         /* flag @ operator */
  fp = fp+1;
  goto l100;        /* next char */
  /* continuei, check for legal characters in intg */
 l39: first = 0;
  for (i=0;i<65;i++)
    {
      isl = i;
      cl = (char)tolower(c);       /* make lowercase */
      if (cl==intg[i]) goto l4;
    }
  zvnprintf(132,"??E stsget: illegal symbol [%c]\n",c);
  zmabend("??E program terminating");
  return;

  /* legal char eval */
 l4:  if (isl<10) goto l7;      /* branch if numeric */
  if (isl>10) goto l5;      /* branch if alphabetic */
  /* char is a period */
  type = 1;                /* float */
  rnum = (double)num;
  rfac = 1.0;
  goto l8;
  /* alphabetic eval */
 l5:  if (isl==26) goto l55;    /* branch if e */
  if (isl==64) goto l65;    /* branch if ' */
  type = -1;                /* alphabetic */
  goto l7;
  /* e eval */
 l55: if (type!=1) goto l7;
  ipow = 0;
  for (i=0;i<10;i++)
    {
      if (fstrng[fp+3]==intg[i]) ipow = ipow+i*10;
      if (fstrng[fp+4]==intg[i]) ipow = ipow+i;
    }
  if (fstrng[fp+2]==aop[0]) rnum = rnum*pow(10.0,(double)ipow);
  if (fstrng[fp+2]==aop[1]) rnum = rnum*pow(0.10,(double)ipow);
  fp = fp+4;
  goto l100;        /* next char */
  /* ' eval */
 l65: type = 0;         /* quoted */
  qret = *sptr;
  qtype = 1;        /* single quote */
  for (i=0;i<30;i++)
    {
      if (fstrng[fp+2]==intg[64]) break;     /* closing ' */
      sbuf[*sptr] = fstrng[fp+2];
      *sptr = *sptr+1;
      fp = fp+1;
    }
  sbuf[*sptr] = (char)0;            /* append a nul */
  *sptr = *sptr+1;
  fp = fp+2;
  goto l100;        /* next char */
  /* compute real value */
 l7:  num = num*10+isl;         /* isl is in intg */
  snum = snum*39;
  if (snum>100000000) snum = snum/31;
  snum = snum+isl;
  rfac = .1*rfac;
  rnum = rnum+rfac*(double)(isl);
  /* blank eval */
 l8:  fp = fp+1;
  goto l100;        /* next char */
  /* resume logical eval */
 l10: if (first) goto l20;
  sbop2 = 0;
  if (type<0) goto l11;	/* branch if alphabetic */
  if (type==0) goto l12;    /* branch if quoted */
  if (type>0) goto l13;     /* branch if float */
 l11: if (!atop) goto l801;
  /* @ eval */
  for (i=0;i<148;i++)
    {
      isv = i;
      if (num==fcv[i]) goto l15;
    }
  zmabend("??E stsget: operator not found");
  return;

 l801: for (i=0;i<20;i++)
    {
      if (snum!=cnum[i]) continue;
      *s = i;
      return;
    }
  return;
  /* get operator in s */
 l15: *s = kcv[isv];
  if (isv<=104) return;
  sbop2 = bop2[*s];
  if (prior[*s]==1) nbpo = 0;
  return;
 l12: cp = cp+1;
  dbuf[cp] = (double)(num);
  if (qtype>0) dbuf[cp] = (double)qret;
  *s = cp;
  return;
  /* float to double */
 l13: cp = cp+1;
  dbuf[cp] = rnum;
  *s = cp;
  return;
 
 l20: fp = fpx;
  *s = cop[isav];
  if (*s==14) nbpo = 0;			/* if & */
  if (c==aop[8]) fstrng[fp] = minus;        /* restore ; to minus */
  if (c!=aop[2]) goto l21;                  /* branch if not * */
  c = fstrng[fp+1];
  if (c!=aop[2]) goto l21;                  /* branch if not * */
  *s = 5;                                   /* val is ** - set OPCODE to 5 (EXP) */
  fp = fp+1;
 l21: sbop2 = bop2[*s];
  return;
}

/*=================================================================*/
/* sp_knuth
   c version 1/23/00 al zobrist ... no attempt to use c constructs,
   just a straight conversion of the fortran lines
                                                                                                                                                          
   c  modified 1/17/90 for string functions
   c  modified 3/16/87 a. zobrist for mosx system
   c  kludged again 6/18/87 a. zobrist
   c  this routine is really getting encrusted from about
   c  five major changes 
   modified for ifthen - added functionsize as parameter 
   since in mf3.c functionsize is a global

   input:  fstrng (whatever is on the right side of = sign)
   modifies: sbop2,fp,cp
   output: ibuf, (sbuf,dbuf,sptr from stsget)
*/
/*================================================================*/
void sp_knuth(fstrng,ibuf,dbuf,sbuf,cnum,sptr,functionsize)
     char *fstrng,*sbuf;
     int *cnum,*sptr,*ibuf;
     int functionsize;
     double *dbuf;
{
  int firvar;
  int stack[50],bpostk[10];
  int prior[63] = {0, 4,4,5,5,6,7,7,7,0,0, 0,0,0,1,0,7,7,7,7,1,
		   1,1,7,7,1,7,7,7,3,3, 3,3,3,3,2,2,2,7,1,1,
		   1,1,1,1,1,1,1,7,7,1, 7,1,1,1,1,1,1,7,7,1,7,7};
  int bop3[63] =  {0, 1,1,1,1,1,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,1,
		   1,1,0,0,1,0,0,0,1,1, 1,1,1,1,1,1,1,0,1,1,
		   1,1,1,1,1,1,1,0,0,1, 0,1,1,1,1,1,1,0,0,1,0,0};
  char schar[10];
  /* char outmsg[132]; */
  int ix,itemp,bp,op,sp,s,loc,iop,s2;

  /* ptr = 0;      */
  strcpy(schar,"(,-+$; xx");

  /* have to pull out unary + and unary -, the algorithm will put back
     later */

  schar[7] = schar[0];
  for (ix=0;ix<functionsize;ix++)
    {
      schar[8] = fstrng[ix];
      if (schar[8]==schar[4]) break;			/* if $ */
      if (schar[8]==schar[6]) continue;			/* if blank */
      if (schar[7]!=schar[0]&&schar[7]!=schar[1])       /* if ( or , */ 
	{
	  schar[7] = schar[8];				/* then, x */
	  continue;
	}
      if (schar[8]==schar[2]) fstrng[ix] = schar[5];	/* if -, then ; */
      if (schar[8]==schar[3]) fstrng[ix] = schar[6];	/* if +, then blank */
      schar[7] = schar[8];
    }

  if (idebug) {
    zvnprintf(132,"fstrng: %s",fstrng);
  }


  bp = -1;
  op = 0;
  fp = -1;
  cp = 102;
  sp = -1;
  sbop2 = 0;
  s = 12; /* data value */
  itemp = 1062; /* stack ptr */
  firvar = 1;
  ibuf[1] = 15*65536;
  goto l4;
  /* loop */
 l2:  if (sbop2) goto l3;
 l4:  sp = sp+1;
  stack[sp] = s;

  /*       printf ("before - fp,cp,sbop2,nbpo = %d %d %d %d\n",fp,cp,sbop2,nbpo); */
 l5:  stsget(&s,fstrng,dbuf,cnum,sbuf,sptr);
  if (idebug) {
    zvnprintf(132,"sp_knuth: input symbol: %d:   fstrng: %s ",s,fstrng);
  }
    
  /*       printf ("after  - fp,cp,sbop2,nbpo = %d %d %d %d\n",fp,cp,sbop2,nbpo); */

  if (firvar&&(s!=9)) ibuf[0] = 13*65536+s;
  if (s!=9) firvar = 0;
  if (nbpo) goto l2;
  if (s==14) goto l21;
  bp = bp+1;
  bpostk[bp] = s;
  goto l5;
 l21: s = bpostk[bp];
  bp = bp-1;
  sbop2 = 1;
  goto l2;
  /* loop end */
 l3:  s2 = stack[sp-1];
  if (prior[s]>prior[s2]) goto l4;
  if (s2==12) goto l24;
  if (s2==9) goto l7;
  if (bop3[s2]) goto l10;
  goto l9;
 l7:  stack[sp-1] = stack[sp];
  sp = sp-1;
  goto l5;
 l9:  loc = stack[sp];
  ibuf[op] = stack[sp-1]*65536+loc;
  ibuf[op+1] = 14*65536+itemp;
  op = op+2;
  sp = sp-1;
  goto l11;
 l10: loc = stack[sp-2];
  iop = 14*65536+loc;
  ibuf[op] = iop-65536;
  /*if (ibuf[op-1]!=iop&&loc>=61) ibuf[2*loc] = 1;optimizer*/
  if (ibuf[op-1]==iop) op = op-1;
  loc = stack[sp];
  ibuf[op+1] = stack[sp-1]*65536+loc;
  ibuf[op+2] = 14*65536+itemp;
  op = op+3;
  sp = sp-2;
 l11: stack[sp] = itemp;
  /*if (loc>=61) ibuf[loc-1] = 1;used in optimizer*/
  /*dbuf[itemp] = 0.0;should never need to clear a temp*/
  itemp++;
  ibuf[op] = 15*65536;
  goto l3;
 l24: 
  /*  ptr = 61;     */
  /*for (itemp=1061;itemp<250;itemp++) don't use optimizer for now
    {                   indexes screwed
    ibuf[ptr-1] = ibuf[itemp-1];
    m = ibuf[itemp-1]/65536;
    if (m==15) return;
    n = ibuf[itemp-1]-m*65536;
    if (m!=14||ibuf[n-1]!=0) ptr = ptr+2;
    }*/
  return;
}
/*================================================================*/
/* look into char patbuf[131] starting at ptr */
void insq(buf,indx,ptr)
     char *buf;
     int indx,ptr;
{
  int ichar,i,iu=0,ict,ir=0;
      
  if (indx==0) return;
  for (i=ptr;i<131;i++)
    {
      iu = i;
      ichar = (int)buf[i];
      if (ichar==0) break;
    }
  ict = iu-ptr+1;
  for (i=1;i<=ict;i++)
    {
      ir = iu-i+1;
      buf[ir+1] = buf[ir];
    }
  buf[ir] = '\?';
  return;
}
/*================================================================*/
/* delete in char patbuf[131] starting at ptr */
void delq(buf,ptr)
     char *buf;
     int ptr;
{
  int ichar,i,iq,iu=0,iu2;
                                                                                                                                               
  if (ptr<0) return;

  for (i=ptr;i<131;i++)
    {
      iu = i;
      ichar = (int)buf[i];
      if (ichar==0) break;
    }
  iq = 0;

  for (i=ptr;i<131;i++)
    {
      ichar = (int)buf[i];
      if (ichar!=63) break;   /* ? */
      iq = iq+1;
    }
                                                                                                                                               
  iu2 = iu-iq;
      
  for (i=ptr;i<iu2;i++)
    {
      buf[i] = buf[i+iq];
    }
  return;
}
/*================================================================*/
/*    sp_xknuth - modified 3/16/87 by a. zobrist for mosx system */
/*    kludged from fortran to c 1/24/00 by a. zobrist
      ... no attempt to use c constructs, just a straight conversion
      of the fortran lines
      inputs:  ibuf,dbuf,sbuf
      modifies: sbuf,sptr
      outputs: result (only after RETN)

*/      
/*================================================================*/
void sp_xknuth(ibuf,dbuf,sbuf,sptr,result,code)
     int *ibuf,*sptr;
     int code;
     double *result,*dbuf;
     char *sbuf;
{
  double reg=0,div,t,num,sig,minut,secnd,frac,sig2;
  char patbuf[131];                  /* left at fortran indexing */
  /* char outmsg[132]; */
  char tchar;
  int starp[4],isu[4],blnct[3];      /* left at fortran indexing */

  int ptr,op,opnd,ibit,jbit,kbit,ireg,jreg,i,j,tmtch,mtch,len,imtch=0;
  int osptr,slen,pmtch,stp,cptr,isu1,is1,isu2,is2,isu3,is3,break2;
  int lrsw,ltr,btr,str,knum,itop=0,kdig,itop2,deccnt,ichar,ichxx;
  char *p,*q,mtchbuf[1000];

  char opcode_name[63][7] = {
    "x","ADD   ","SUB   ","MUL   ","DIV   ","x","LOG10 ","LOG   ","INT   ","x","x",
    "x","x","LOAD  ","STOR  ","RETN  ","SQRT  ","SIN   ","COS   ","TAN   ","MAX   ",
    "MIN   ","MOD  ","ABS   ","LCMP  ","ATAN2 ","ASIN  ","ACOS  ","ATAN  ","LT    ","LE    ",
    "EQ    ","NE    ","GE    ","GT    ","OR    ","AND   ","POW   ","NOT   ","x","x",
    "LSHF  ","RSHF  ","FSTR  ","BSTR  ","ADEL  ","SDEL  ","TRIM  ","UCASE ","LCASE ","REPL  ",
    "STRLEN","POS   ","STREQ ","STRSUB","STRPAT","LJUST ","RJUST ","NUM   ","I2STR ","F2STR ",
    "DMSSTR","DMSNUM"};

  /*      sprintf (outmsg,"sp_xknuth: debug = %d   debugrec1 = %d\n",idebug,debugrec1);
	  zvmessage(outmsg," ");
  */
  for (ptr=0;ptr<OPBUF;ptr++)
    {
      op = ibuf[ptr]>>16;                         /* OPCODE */
      opnd = ibuf[ptr]&65535;                     /* OPERAND */
      /*
	printf ("op = %d opnd = %d\n",op,opnd);
	opptr = &oopp;
	oopp.operand = (short) opnd;
	oopp.opcode = (short) op;

	printf ("oopp.opcode = %d oopp.operand = %d\n", oopp.opcode,oopp.operand);
        printf ("opptr->opcode = %d\n",opptr->opcode);
      */
      switch (op)
	{
	case 9: case 10: case 11: case 12: case 39: case 40:
	  zmabend("??E sp_xknuth: arithmetic execution error");
	  break;
	case 1:                                     /* +  (ADD) */
	  reg = reg+dbuf[opnd];
	  break;
	case 2:
	  reg = reg-dbuf[opnd];                     /* - (SUB) */
	  break;
	case 3:
	  reg = reg*dbuf[opnd];                     /* * (MUL) */
	  break;
	case 4:                                     /* / (DIV) */
	  div = dbuf[opnd];
	  if (fabs(div)>=1.0e-20)
	    {
	      reg = reg/div;
	      break;
	    }
	  if (div>=0) div = div+1.0e-20;
	  if (div<0) div = div-1.0e-20;
	  reg = reg/div;
	  break;
	case 37:    /* temporarily using ^ for exponentiation */
	  reg = pow(MAX(reg,1.0e-6),dbuf[opnd]);
	  break;
	case 6:                                     /* LOG10 */
	  reg = log10(MAX(dbuf[opnd],1.0e-6));
	  break;
	case 7:                                     /* LN  (LOG) */
	  reg = log(MAX(dbuf[opnd],1.0e-6));
	  break;
	case 8:
	  reg = (int)(dbuf[opnd]);                  /* INT */
	  break;
	case 13:                                    /* LOAD */
	  reg = dbuf[opnd];
	  break;
	case 14:                                    /* STOR */
	  dbuf[opnd] = reg;
	  break;
	case 15:                                    /* RTN */
	  *result = reg;
	  if (idebug || code) {
	    zvnprintf(132,"%s %5d    reg = %f",&opcode_name[op][0],opnd,reg);
	  }

	  return;
	case 16:                                    /* SQRT */
	  reg = sqrt(fabs(dbuf[opnd]));
	  break;
	case 17:                                    /* SIN */
	  reg = sin(dbuf[opnd]);
	  break;
	case 18:                                    /* COS */
	  reg = cos(dbuf[opnd]);
	  break;
	case 19:                                    /* TAN */
	  reg = tan(dbuf[opnd]);
	  break;
	case 20:                                    /* max (AMAX) */
	  reg = MAX(reg,dbuf[opnd]);
	  break;
	case 21:                                    /* MIN (AMIN) */
	  reg = MIN(reg,dbuf[opnd]);
	  break;
	case 22:                                    /* FMOD (MOD) */
	  div = dbuf[opnd];
	  if (fabs(div)>=1.0e-20)
	    {
	      reg = fmod(reg,div);
	      break;
	    }
	  if (div>=0) div = div+1.0e-20;
	  if (div<0) div = div-1.0e-20;
	  reg = fmod(reg,div);
	  break;
	case 23:                                    /* ABS */
	  reg = fabs(dbuf[opnd]);
	  break;
	case 24:                                    /* LCMP */
	  reg = -dbuf[opnd];
	  break;
	case 25:                                    /* ATAN2 */
	  if ((reg==0.0)&&(dbuf[opnd]==0.0))
	    {
	      reg = 0.0;
	      break;
	    }
	  reg = atan2(reg,dbuf[opnd]);
	  break;
	case 26:                                    /* ASIN */
	  reg = asin(dbuf[opnd]);
	  break;
	case 27:                                    /* ACOS */
	  reg = acos(dbuf[opnd]);
	  break;
	case 28:
	  reg = atan(dbuf[opnd]);                   /* ATAN */
	  break;
	case 29:
	  t = 0.0;
	  if (reg<dbuf[opnd]) t = 1.0;              /* < (.LT.) */
	  reg = t;
	  break;
	case 30:                                    /* <= (.LE.) */
	  t = 0.0;
	  if (reg<=dbuf[opnd]) t = 1.0;
	  reg = t;
	  break;
	case 31:                                    /* == (.EQ.) */
	  t = 0.0;
	  if (reg==dbuf[opnd]) t = 1.0;
	  reg = t;
	  break;
	case 32:                                    /* != (.NE.) */
	  t = 0.0;
	  if (reg!=dbuf[opnd]) t = 1.0;
	  reg = t;
	  break;
	case 33:                                    /* >= (.GE.) */
	  t = 0.0;
	  if (reg>=dbuf[opnd]) t = 1.0;
	  reg = t;
	  break;
	case 34:                                    /* > (.GT.) */
	  t = 0.0;
	  if (reg>dbuf[opnd]) t = 1.0;
	  reg = t;
	  break;
	case 35:                                    /* || (.OR.) */
	  ibit = (int)reg;                          /* cast - May 05, 2011 */
	  jbit = (int)dbuf[opnd];                   /* cast - May 05, 2011 */
	  kbit = ibit|jbit;
	  reg = (double)kbit;
	  break;
	case 36:                                    /* && (.AND. */
	  ibit = (int)reg;                          /* cast - May 05, 2011 */
	  jbit = (int)dbuf[opnd];                   /* cast - May 05, 2011 */
	  kbit = ibit&jbit;
	  reg = (double)kbit;
	  break;
	  /*case 37: the ^ symbol appropriated for expon
	    ibit = reg;
	    jbit = dbuf[opnd];
	    kbit = ibit^jbit;
	    reg = (double)kbit;
	    break;*/
	case 38:                                    /* ! (.NOT.) */
	  reg = 1.0-dbuf[opnd];
	  break;

	case 41:                                    /* <<    LSHF */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
                                                                                                                                               
	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      sbuf[*sptr] = (char)ichar;
	      *sptr = *sptr+1;
	      if (ichar==0) break;
	    }
	  *sptr = *sptr-1;
	  for (j=0;j<100;j++)
	    {
	      ichar = (int)sbuf[jreg+j];
	      sbuf[*sptr] = (char)ichar;
	      *sptr = *sptr+1;
	      if (ichar==0) break;
	    }
	  break;

	case 42:                    /* break */     /* >>    RSHF */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar==0) break;
	      for (j=0;j<100;j++)
		{
		  ichxx = (int)sbuf[jreg+j];
		  if (ichxx==0) break;
		  if (ichxx==ichar) break;
		}
	      if (ichxx==ichar) break;
	      sbuf[*sptr] = (char)ichar;
	      *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 43:                    /* FSTR */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  for (i=0;i<jreg;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar==0) break;
	      sbuf[*sptr] = (char)ichar;
	      *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 44:                     /* BSTR */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
                                                                                                                                               
	  for (i=0;i<60;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar==0) break;
	      if (i>=jreg) sbuf[*sptr] = (char)ichar;
	      if (i>=jreg) *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 45:                    /* ADELETE */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
                                                                                                                                               
	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar==0) break;
	      for (j=0;j<100;j++)
		{
		  ichxx = (int)sbuf[jreg+j];
		  if (ichxx==0) break;
		  if (ichxx==ichar) break;
		}
	      if (ichxx==ichar) continue;
	      sbuf[*sptr] = (char)ichar;
	      *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 46:                    /* SDELETE */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  tmtch = 0;
                                                                                                                                               
	  for (j=0;j<100;j++)
	    {
	      ichxx = (int)sbuf[jreg+j];
	      if (ichxx==0) break;
	      tmtch = tmtch+1;
	    }
	  mtch = 0;
	  i = 0;
	  while (i<100)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar==0) break;
	      ichxx = (int)sbuf[jreg+mtch];
	      if (ichxx==ichar) mtch = mtch+1;
	      else if (mtch>0)
		{
		  i = i-mtch+1;
		  *sptr = *sptr-mtch+1;
		  mtch = 0;
		  continue;
		}
	      if (mtch>=tmtch)
		{
		  *sptr = *sptr-tmtch+1;
		  mtch = 0;
		}
	      else
		{
		  sbuf[*sptr] = (char)ichar;
		  *sptr = *sptr+1;
		}
	      i++;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 47:                    /* TRIM */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  len = 0;
                                                                                                                                               
	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar==0) break;
	      sbuf[*sptr] = (char)ichar;
	      *sptr = *sptr+1;
	      len = len+1;
	    }
	  if (len>=0) for (i=0;i<len;i++)
			{
			  ichar = (int)sbuf[*sptr-1];
			  osptr = *sptr;
                                                                                                                                               
			  for (j=0;j<30;j++)
			    {
			      ichxx = (int)sbuf[jreg+j];
			      if (ichxx==0) break;
			      if (ichxx==ichar) *sptr = *sptr-1;
			    }
			  if (*sptr==osptr) break;
			}
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 48:                    /* UCASE */
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[jreg+i];
	      if (ichar==0) break;
	      sbuf[*sptr] = (char)toupper((char)ichar);
	      *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 49:                    /* LCASE */
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);

	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[jreg+i];
	      if (ichar==0) break;
	      sbuf[*sptr] = (char)tolower((char)ichar);
	      *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 50:                    /* REPLACE */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  tmtch = 0;
	  for (j=0;j<100;j++)
	    {
	      ichxx = (int)sbuf[jreg+j];
	      if (ichxx==61) break;
	      if (ichxx==0) zmabend("??E sp_xknuth: no equals sign in replacement string");
	      tmtch = tmtch+1;
	    }
	  mtch = 0;
	  i = 0;
	  while (i<100)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar==0) break;
	      ichxx = (int)sbuf[jreg+mtch];
	      if (ichxx==ichar) mtch = mtch+1;
	      else if (mtch>0)
		{
		  i = i-mtch+1;
		  *sptr = *sptr-mtch+1;
		  mtch = 0;
		  continue;
		}
	      if (mtch<tmtch)
		{
		  sbuf[*sptr] = (char)ichar;
		  *sptr = *sptr+1;
		  i++;
		  continue;
		}
	      *sptr = *sptr-tmtch+1;
	      mtch = 0;
                                                                                                                                               
	      for (j=0;j<100;j++)
		{
		  ichxx = (int)sbuf[jreg+tmtch+j+1];
		  if (ichxx==0) break;
		  sbuf[*sptr] = (char)ichxx;
		  *sptr = *sptr+1;
		}
	      i++;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 51:                    /* STRLEN */
	  jreg = (int)(dbuf[opnd]+.001);
	  len = 0;
                                                                                                                                               
	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[jreg+i];
	      if (ichar==0) break;
	      len = len+1;
	    }
	  reg = (double)len;
	  break;
                                                                                                                                               
	case 53:                    /* STREQ */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = 0.0;
	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      ichxx = (int)sbuf[jreg+i];
	      if (ichxx!=ichar) goto done53;
	      if (ichar==0||ichxx==0) break;
	    }
	  if (ichar==0&&ichxx==0) reg = 1.0;
	done53:
	  break;

	case 54:                    /* STRSUB */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = 0.0;
	  tmtch = 0;
	  for (j=0;j<100;j++)
	    {
	      ichxx = (int)sbuf[jreg+j];
	      if (ichxx==0) break;
	      tmtch = tmtch+1;
	    }
	  mtch = 0;
	  i = 0;
	  while (i<100)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar==0) break;
	      ichxx = (int)sbuf[jreg+mtch];
	      if (ichxx==ichar) mtch = mtch+1;
	      else if (mtch>0)
		{
		  i = i-mtch+1;
		  *sptr = *sptr-mtch+1;
		  mtch = 0;
		  continue;
		}
	      if (mtch>=tmtch)
		{
		  reg = 1.0;
		  break;
		}
	      i++;
	    }
	  break;

	case 52: case 55:           /*  52=POS 55=STRPAT */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = 0.0;
	  slen = 0;
	  for (j=0;j<100;j++)
	    {
	      ichxx = (int)sbuf[ireg+j];
	      if (ichxx==0) break;
	      slen = slen+1;
	    }
	  pmtch = 0;
	  stp = 0;
	  isu[1] = 1;
	  isu[2] = 1;
	  isu[3] = 1;
	  starp[1] = -999;
	  starp[2] = -999;
	  starp[3] = -999;
	  cptr = 0;
	  for (j=0;j<100;j++)
	    {
	      ichxx = (int)sbuf[jreg+j];
	      patbuf[cptr] = (char)ichxx;
	      cptr = cptr+1;
	      if (ichxx==42)
		{
		  cptr = cptr-1;
		  pmtch = pmtch-1;
		  stp = stp+1;
		  starp[stp] = cptr;
		  isu[stp] = slen;
		}
	      if (ichxx==0) break;
	      pmtch = pmtch+1;
	    }

	  isu1 = MIN(isu[1],slen-pmtch+3);
	  for (is1=0;is1<isu1;is1++)
	    {
	      insq(patbuf,is1,starp[1]);
	      isu2 = MIN(isu[2],slen-pmtch+3);
	      for (is2=0;is2<isu2;is2++)
		{
		  insq(patbuf,is2,starp[2]+is1);
		  isu3 = MIN(isu[3],slen-pmtch+3);
		  for (is3=0;is3<isu3;is3++)
		    {
		      insq(patbuf,is3,starp[3]+is2+is1);
		      tmtch = pmtch+is1+is2+is3;
		      if (tmtch>slen+2) break;
                                                                                                                                               
		      for (i=0;i<1000;i++)
			{
			  mtch = 0;
			  imtch = 0;
			  break2 = 0;
			  for (j=0;j<1000;j++)
			    {
			      ichar = (int)sbuf[ireg+i+j];
			      ichxx = (int)patbuf[mtch];
			      if (ichar!=0||ichxx!=37)
				{
				  if (ichar==0) { break2 = 1; break; }
				  ichxx = (int)patbuf[mtch];
				  if (ichxx==94)
				    {
				      if (i!=0) { break2 = 1; break; }
				      mtch = mtch+1;
				      ichxx = (int)patbuf[mtch];
				    }
				  if (ichxx!=ichar&&ichxx!=63) break;
				  mtchbuf[imtch++] = (char)ichar;
				}
			      mtch = mtch+1;
			      if (mtch<tmtch) continue;
			      reg = i+1;
			      if (op==55) reg = 0.0;
			      goto done55;
			    } /* j loop */
			  if (break2) break;
			} /* i loop */
		    } /* is3 */
		  delq(patbuf,starp[3]+is2+is1-2);
		} /* is2 */
	      delq(patbuf,starp[2]+is1-1);
	    } /* is1 */
	done55:
	  if (op==55)
	    if (op==55)
	      {
		reg = (double)(*sptr);
		for (i=0;i<imtch;i++) sbuf[(*sptr)++] = mtchbuf[i];
		sbuf[(*sptr)++] = (char)0;
	      }
	  break;

	case 56:
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  len = 0;
	  blnct[1] = 0;
	  blnct[2] = 0;
	  lrsw = 1;
	  ichxx = (int)' ';

	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar!=ichxx) lrsw = 2;
	      if (ichar!=ichxx) blnct[2] = 0;
	      if (ichar==ichxx) blnct[lrsw] = blnct[lrsw]+1;
	      if (ichar==0) break;
	      len = len+1;
	    }

	  ltr = MIN(jreg,len-blnct[1]-blnct[2]);
	  btr = jreg-ltr;
	  str = blnct[1]+1;
	  for (i=0;i<ltr;i++)
	    {
	      sbuf[*sptr] = sbuf[ireg+str+i-1];
	      *sptr = *sptr+1;
	    }
	  for (i=0;i<btr;i++)
	    {
	      sbuf[*sptr] = ' ';
	      *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 57:                    /* RJUST */
	  ireg = (int)(reg+.001);
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  len = 0;
	  blnct[1] = 0;
	  blnct[2] = 0;
	  lrsw = 1;
	  ichxx = (int)' ';

	  for (i=0;i<100;i++)
	    {
	      ichar = (int)sbuf[ireg+i];
	      if (ichar!=ichxx) lrsw = 2;
	      if (ichar!=ichxx) blnct[2] = 0;
	      if (ichar==ichxx) blnct[lrsw] = blnct[lrsw]+1;
	      if (ichar==0) break;
	      len = len+1;
	    }

	  ltr = MIN(jreg,len-blnct[1]-blnct[2]);
	  btr = jreg-ltr;
	  str = blnct[1]+1;

	  for (i=0;i<btr;i++)
	    {
	      sbuf[*sptr] = ' ';
	      *sptr = *sptr+1;
	    }
	  for (i=0;i<ltr;i++)
	    {
	      sbuf[*sptr] = sbuf[ireg+str+i-1];
	      *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)ichar;
	  *sptr = *sptr+1;
	  break;

	case 58:                    /* NUM */
	  jreg = (int)(dbuf[opnd]+.001);
	  p = &sbuf[jreg];
	  reg = ms_dnum(&p);
	  break;

	case 59:                    /* I2STR */
	  ireg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  if (ireg<0)
	    {
	      sbuf[*sptr] = '-';
	      *sptr = *sptr+1;
	      ireg = (int)(-dbuf[opnd]+.001);
	    }
	  for (i=0;i<12;i++)
	    {
	      knum = ireg/10;
	      if (ireg==0&&i!=1) { itop = i; break; }
	      kdig = ireg-knum*10;
	      ichar = kdig+48;
	      sbuf[*sptr+i] = (char)ichar;
	      ireg = knum;
	    }
	  itop2 = itop/2;
                                                                                                                                               
	  for (i=0;i<itop2;i++)
	    {
	      tchar = sbuf[*sptr+i];
	      sbuf[*sptr+i] = sbuf[*sptr+itop-1-i];
	      sbuf[*sptr+itop-1-i] = tchar;
	    }
	  *sptr = *sptr+itop+1;
	  sbuf[*sptr-1] = (char)0;
	  break;

	case 60:
	  num = reg;
	  jreg = (int)(dbuf[opnd]+.001);
	  reg = (double)(*sptr);
	  if (num<0.0)
	    {
	      sbuf[*sptr] = '-';
	      *sptr = *sptr+1;
	      num = -num;
	    }
	  num = num+0.5*pow(0.1,(double)jreg);
	  ireg = (int)num;
	  deccnt = 0;
	  for (i=0;i<50;i++)
	    {
	      if (num<10.0) break;
	      deccnt = deccnt+1;
	      num = 0.1*num;
	    }
	  itop = deccnt+jreg+1;
	  for (i=0;i<itop;i++)
	    {
	      kdig = (int)num;
	      num = fmod(num,1.0);
	      num = num*10.0;
	      ichar = kdig+48;
	      sbuf[*sptr] = (char)ichar;
	      *sptr = *sptr+1;
	      deccnt = deccnt-1;
	      if (deccnt!=(-1)) continue;
	      if (jreg==0) break;
	      sbuf[*sptr] = '.';
	      *sptr = *sptr+1;
	    }
	  sbuf[*sptr] = (char)0;
	  *sptr = *sptr+1;
	  break;

	case 61:                            /* dmsstr */
	  jreg = (int)(dbuf[opnd]+.001);
	  len = (int)strlen(&sbuf[jreg]);           /* cast - May 05, 2011 */
	  if (strchr("WSwsENen",sbuf[jreg+len-1])!=0)
	    {
	      if (strchr("WSws",sbuf[jreg+len-1])!=0) sig2 = -1.0;
	      else sig2 = 1.0;
	      len--;
	      sbuf[jreg+len] = (char)0;
	    }
	  else sig2 = 1.0;
	  if (strchr("WSwsENen",sbuf[jreg])!=0)
	    {
	      if (strchr("WSws",sbuf[jreg])!=0) sig2 = -1.0;
	      else sig2 = 1.0;
	      jreg++;
	    }
	  for (p=&sbuf[jreg],q=mtchbuf;;p++)
	    {
	      if (*p==(char)0) { *q = (char)0; break; }
	      if (strchr("0123456789.+-eE",*p)!=0) *(q++) = *p;
	      if ((*p=='d'||*p=='D')&&strchr("+-",*(p+1))!=0) *(q++) = *p;
	    }
	  p = mtchbuf;
	  num = ms_dnum(&p);
	  if (num<0.0) { sig = -1.0; num = -num;} else sig = 1.0;
	  frac = 100.0*((int)(num/100.0));
	  secnd = num-frac;
	  num -= secnd;
	  frac = 10000.0*((int)(num/10000.0));
	  minut = num-frac;
	  num -= minut;
	  reg = sig2*sig*(num/10000.0+minut/6000.0+secnd/3600.0);
	  break;

	case 62:                            /* DMSNUM */
	  num = dbuf[opnd];
	  if (num<0.0) { sig = -1.0; num = -num;} else sig = 1.0;
	  frac = 100.0*((int)(num/100.0));
	  secnd = num-frac;
	  num -= secnd;
	  frac = 10000.0*((int)(num/10000.0));
	  minut = num-frac;
	  num -= minut;
	  reg = sig*(num/10000.0+minut/6000.0+secnd/3600.0);
	  break;
	}
      if (idebug || code) {
	zvnprintf(132,"%s %5d    reg = %f",&opcode_name[op][0],opnd,reg);
	/*        zknuth_dump(opnd,op); */
      }
    }
  return;
}
/*================================================================*/
/* main program */
void main44(void)
{
  char *c_data;
  char *sbuf;
  char *iffunction,*thenfunction,*elsefunction;
  char *r1,*q2,*r2,*q3,*r3,*qlp;
  char iffuncparm[40][251],thenfuncparm[40][251],elsefuncparm[40][251];
  /* char outmsg[132]; */
  char fmtstring[10];
  char blanks[500],zeros[2];
  char c;
  char c_field[NUMCOLS][MAXTEXT];

  unsigned int diff;
  int i,j,k,m;
  int iffunctionsize,thenfunctionsize,elsefunctionsize,functionsize;
  int npar,idef,ncol,nincol,tablen,unit,ibis,status;
  int wmx,rcol,sptr,svsptr,ires,lres;
  int cnum[NUMCOLS],savvec[NUMCOLS2],datcols[NUMCOLS];
  int typ[NUMCOLS],wid[NUMCOLS],totwid[NUMCOLS1];
  int ibuf[OPBUF];
  int idebug,code;
  int strl,snum,alphc,cptr,lptr,savptr,ksv;
  int ifop,lparfound;

  double dbuf[ARITHBUF];
  double res;

  char fop[11] = {'+','-','*','/','<','>','=','!','^','&','|'}; /*,"+-/()* ,;$<=!|&>^@"); */
  char paren[2] = {'(',')'};

  zifmessage("IFTHEN version 2019-07-31");

  /*  mxddwid = 0;    */
  /********************/
  /* READ INPUT PARMS */
  /********************/
  /* if function */
  iffunctionsize = 0;
  zvparm("if",iffuncparm,&npar,&idef,40,251);
  for (i=0;i<npar;i++) {
    iffunctionsize += (int)strlen(iffuncparm[i]);       /* cast - May 05, 2011 */
  }
  mz_alloc1((unsigned char **)&iffunction,iffunctionsize+1,1);
  strcpy(iffunction,iffuncparm[0]);
  for (i=1;i<npar;i++) {
    strcat(iffunction,iffuncparm[i]);
  }
  /*mz_free2((unsigned char **)funcparm,40);*/
  zvnprintf(132,"if   string = %s",iffunction);
  /* then function */
  thenfunctionsize = 0;
  zvparm("then",thenfuncparm,&npar,&idef,40,251);
  for (i=0;i<npar;i++) {
    thenfunctionsize += (int)strlen(thenfuncparm[i]);       /* cast - May 05, 2011 */
  }
  mz_alloc1((unsigned char **)&thenfunction,thenfunctionsize+1,1);
  strcpy(thenfunction,thenfuncparm[0]);
  for (i=1;i<npar;i++) {
    strcat(thenfunction,thenfuncparm[i]);
  }
  /*mz_free2((unsigned char **)funcparm,40);*/
  zvnprintf(132,"then string = %s",thenfunction);
  /* else function */
  elsefunctionsize = 0;
  zvparm("else",elsefuncparm,&npar,&idef,40,251);
  for (i=0;i<npar;i++) {
    elsefunctionsize += (int)strlen(elsefuncparm[i]);       /* cast - May 05, 2011 */
  }
  if (elsefunctionsize < 251) elsefunctionsize = 251;
  mz_alloc1((unsigned char **)&elsefunction,elsefunctionsize+1,1);
  strcpy(elsefunction,elsefuncparm[0]);
  for (i=1;i<npar;i++) {
    strcat(elsefunction,elsefuncparm[i]);
  }
  /*mz_free2((unsigned char **)funcparm,40);*/
  zvnprintf(132,"else string = %s",elsefunction);

  idebug=0;
  zvp("debug",&idebug,&npar);

  code=0;
  zvp("code",&code,&npar);
  /********************/
  /* open the data set */
  /********************/
  status = zvunit(&unit,"inp",1,NULL);            //64-bit
  status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
  if (status!=1) IBISSignalU(unit,status,1);
  IBISFileGet(ibis,"nr",&tablen,1,1,0);
  IBISFileGet(ibis,"nc",&ncol,1,1,0);

  /*     mxddwid = 50;  */
  mz_alloc1((unsigned char **)&sbuf,STRINGBUF,1);
  totwid[0] = 0;
  for (i=0;i<500;i++) blanks[i] = ' ';			/* fill blanks */
  for (i=0;i<2;i++) zeros[i] = (char)0;

  for (i=0;i<NUMCOLS;i++) cnum[i] = -1;		/* fill -1 into col numbers */

  snum = 0; cptr = 0; lptr = 0; savptr = 0; alphc = 0;

  /********************/
  /* parse the IF FUNCTION for column numbers, @ signs, single ', */
  /********************/
  strl = (int)strlen(iffunction);             /* cast - May 05, 2011 */
  for (i=0;i<=strl;i++) {
    c = iffunction[i];
    if (c=='@')                                               /* @ */
      do c = iffunction[++i]; while (isalnum(c));
    if (c=='\'')                                              /* ' */
      do c = iffunction[++i]; while (c!='\'');
    c = (char)tolower(c);

    /* relies on fact that first char will be c */
    for (j=0;j<64;j++) if (c==cvec[j]) {               /* cvec[] contains legal characters */
	c_field[cptr][lptr] = c;                     /* c_field will contain control column id, eg. "C12" */
	if (idebug) {
	  zvnprintf(132,"if: c_field[%d][%d] = %s",cptr,lptr,&c_field[cptr][lptr]);
	}
	lptr++;
	/*	sprintf (outmsg,"snum = %d",snum);
		zvmessage(outmsg," ");
	*/
        snum *= 39;
        if (snum>100000000) snum /= 31;
        snum += j;
        if (j>10 && lptr==1) alphc = 1;
	/*	sprintf (outmsg,"j, lptr, alphc %d, %d, %d",j, lptr, alphc);
		zvmessage(outmsg," "); 
	*/
        goto nexti;
      } /* for (j=0;j<64;j++) */

    if (snum*alphc!=0) {
      if (idebug) {
	zvmessage("alph"," ");
      }
      alphc = 0;
      ksv = -1;
      for (k=0;k<cptr;k++)
	if (cnum[k]==snum) ksv = k;
      if (ksv==(-1)) {
	ksv = cptr;
	cnum[cptr] = snum;
	/*	    sprintf (outmsg,"cnum[%d] = %d, ksv = %d",cptr,cnum[cptr], ksv); 
		    zvmessage(outmsg," ");
	*/
	if (cptr==NUMCOLS) zmabend("??E too many columns in if function");
	c_field[cptr++][lptr] = (char)0;		/* append a terminating nul */
      }
      if (c=='=' && iffunction[i+1]!='=') {            /* check for == */
	savvec[savptr] = ksv;
	if (idebug) {
	  zvnprintf(132,"if: savvec[%d] = %d",savptr,savvec[savptr]);
	}
	savptr++;
      }
    } /* if (snum*alphc!=0) */
    snum = 0;
    lptr = 0;
  nexti: continue;
  }  /* for (i=0;i<=strl;i++) */

  if (idebug) {
    zvnprintf(132,"Num of if columns: cptr = %d",cptr);
  }

  /********************/
  /* parse the THEN FUNCTION */
  /********************/
  /* zvmessage("then:"," "); */
  strl = (int)strlen(thenfunction);                /* cast - May 05, 2011 */
  if (strcmp(thenfunction,"noop") == 0 || (strcmp(thenfunction,"NOOP")) == 0)  {
    zvmessage("No then operation given, exiting..."," ");
    exit(0);
  }
  for (i=0;i<=strl;i++) {
    c = thenfunction[i];
    if (c=='@')                                               /* @ */
      do c = thenfunction[++i]; while (isalnum(c));
    if (c=='\'')                                              /* \ */
      do c = thenfunction[++i]; while (c!='\'');
    c = (char)tolower(c);
    for (j=0;j<64;j++) if (c==cvec[j]) {               /* cvec[] contains legal characters */
	c_field[cptr][lptr] = c;                     /* c_field will contain control column id, eg. "C12" */
	if (idebug) {
	  zvnprintf(132,"then: c_field[%d][%d] = %s",cptr,lptr,&c_field[cptr][lptr]);
	}
	lptr++;
	/*        sprintf (outmsg,"snum = %d",snum);
		  zvmessage(outmsg," ");
	*/
	snum *= 39;
	if (snum>100000000) snum /= 31;
	snum += j;
	if (j>10 && lptr==1) alphc = 1;
	/*        sprintf (outmsg,"j, lptr, alphc %d, %d, %d",j, lptr, alphc);
		  zvmessage(outmsg," ");
	*/
	goto nextt;
      } /* for (j=0;j<64;j++) */

    if (snum*alphc!=0) {
      if (idebug) {
	zvmessage("alph"," ");
      }
      alphc = 0;
      ksv = -1;
      for (k=0;k<cptr;k++)
	if (cnum[k]==snum) ksv = k;
      if (ksv==(-1)) {
	ksv = cptr;
	cnum[cptr] = snum;
	/*            sprintf (outmsg,"cnum[%d] = %d, ksv = %d",cptr,cnum[cptr], ksv);
		      zvmessage(outmsg," ");
	*/
	if (cptr==NUMCOLS) zmabend("??E too many columns in then function");
	c_field[cptr++][lptr] = (char)0;            /* append a terminating nul */
      }
      if (c=='=' && thenfunction[i+1]!='=') {            /* check for = (not ==) */
	savvec[savptr] = ksv;			/* save output column */
	if (idebug) {
	  zvnprintf(132,"then: savvec[%d] = %d",savptr,savvec[savptr]);
	}
	savptr++;
      }
    } /* if (snum*alphc!=0) */
    snum = 0;
    lptr = 0;
  nextt:  continue;
  }  /* for (i=0;i<=strl;i++) */
  if (idebug) {
    zvnprintf(132,"Num of if/then columns: cptr = %d",cptr);
  }

  /********************/
  /* parse the ELSE FUNCTION */
  /********************/
  strl = (int)strlen(elsefunction);               /* cast - May 05, 2011 */
  if (strcmp(elsefunction,"noop") == 0 || (strcmp(elsefunction,"NOOP")) == 0)  {
    zvmessage("   OK, No else operation given"," ");
    r2 = &thenfunction[0];
    q2 = strchr(r2,'=') +1;
    diff = (unsigned int) (q2-r2-1);            /* cast - May 05, 2011 */
    /*        strncpy(elsefunction,"\0");    removed Feb 24, 2008 */
    strncpy(elsefunction,thenfunction,(long unsigned int)diff);
    strcat(elsefunction,"=");
    strncat(elsefunction,thenfunction,diff);
    strcat(elsefunction,"\0");
    zvnprintf(132,"   Use as elsefunction: %s\n",elsefunction);
    strl = (int)strlen(elsefunction);           /* cast - May 05, 2011 */
  }
  for (i=0;i<=strl;i++) {
    c = elsefunction[i];
    if (c=='@')                                               /* @ */
      do c = elsefunction[++i]; while (isalnum(c));
    if (c=='\'')                                              /* \ */
      do c = elsefunction[++i]; while (c!='\'');
    c = (char)tolower(c);

    for (j=0;j<64;j++) if (c==cvec[j]) {               /* cvec[] contains legal characters */
	c_field[cptr][lptr] = c;                     /* c_field will contain control column id, eg. "C12" */
	if (idebug) {
	  zvnprintf(132,"else: c_field[%d][%d] = %s",cptr,lptr,&c_field[cptr][lptr]);
	}
	lptr++;
	/*          sprintf (outmsg,"snum = %d",snum);
		    zvmessage(outmsg," ");
	*/
	snum *= 39;
	if (snum>100000000) snum /= 31;
	snum += j;
	if (j>10 && lptr==1) alphc = 1;
	/*          sprintf (outmsg,"j, lptr, alphc %d, %d, %d",j, lptr, alphc);
		    zvmessage(outmsg," ");
	*/
	goto nexte;
      } /* for (j=0;j<64;j++) */

    if (snum*alphc!=0) {
      if (idebug) {
	zvmessage("alph"," ");
      }
      alphc = 0;
      ksv = -1;
      for (k=0;k<cptr;k++)
	if (cnum[k]==snum) ksv = k;
      if (ksv==(-1)) {
	ksv = cptr;
	cnum[cptr] = snum;
	/*            sprintf (outmsg,"cnum[%d] = %d, ksv = %d",cptr,cnum[cptr], ksv);
		      zvmessage(outmsg," ");
	*/
	if (cptr==NUMCOLS) zmabend("??E too many columns in else function");
	c_field[cptr++][lptr] = (char)0;            /* append a terminating nul */
      }
      if (c=='=' && elsefunction[i+1]!='=') {            /* check for == */
	savvec[savptr] = ksv;
	if (idebug) {
	  zvnprintf(132,"else: savvec[%d] = %d",savptr,savvec[savptr]);
	}
	savptr++;
      }
    } /* if (snum*alphc!=0) */
    snum = 0;
    lptr = 0;
  nexte:  continue;
  }  /* for (i=0;i<=strl;i++) */

  if (idebug) {
    zvnprintf(132,"Num of if/then/else columns: cptr = %d",cptr);
  }

  /********************/
  /* prepare IF FUNCTION for code generator */
  /********************/
  savptr = 0;
  i = (int)strlen(iffunction);                /* cast - May 05, 2011 */
  if (idebug) {
    zvnprintf(132,"length of if function = %d\n",i);
  }
  iffunction[i] = '$';                                           /* append a $ */
  iffunction[i+1] = (char)0;                                     /* append a nul */
  if (idebug) {
    zvnprintf(132,"iffunction: %s\n",iffunction);
  }
  r1 = &iffunction[0];
  if (idebug) {
    zvnprintf(132,"%s\n",&r1[0]);
  }
  /********************/
  /* prepare THEN FUNCTION for code generator */
  /********************/
  i = (int)strlen(thenfunction);              /* cast - May 05, 2011 */
  if (idebug) {
    zvnprintf(132,"length of then function = %d\n",i);
  }
  thenfunction[i] = '$';                                           /* append a $ */
  thenfunction[i+1] = (char)0;                                     /* append a nul */
  if (idebug) {
    zvnprintf(132,"thenfunction: %s\n",thenfunction);
  }                                                                                                                              
  r2 = &thenfunction[0];
  if (idebug) {
    zvnprintf(132,"%s\n",&r2[0]);
  }
  q2 = strchr(r2,'=') +1;
  if (q2 == NULL) {
    zvmessage ("No = in then function"," ");
    zmabend ("??E bad statement");
  }
  if (idebug) {
    zvnprintf(132,"then>%s",&q2[0]);
  }
  lparfound = 0;          /* init paren check */
  ifop = 0;
  qlp = strchr(r2,'$') +1;
  /*    strl = strlen(q2);  */
  diff = (unsigned int)(qlp - q2);                        /* cast - May 05, 2011 */
  /* simple ( and ops checks (needs to be smarter) */
  for (m=0;m<(signed int)diff;m++) {                      /* cast - May 05, 2011 */
    if (q2[m] == paren[0]) {         /* check for ( */
      lparfound = 1;
    }
    for (j=0;j<11;j++) {            /* check for op? */
      if (q2[m] == fop[j]) {
	ifop++;
      }
    }
    if (q2[m] == 'e') {          /*decrement for exponential (2 ops) */
      --ifop;
    }
  }
  if (ifop > 1 && lparfound == 0) {
    zvmessage("parentheses required for then function"," ");
    zmabend ("??E bad statement");
  }

  /********************/
  /* prepare ELSE FUNCTION for code generator */
  /********************/
  i = (int)strlen(elsefunction);                  /* cast - May 05, 2011 */
  if (idebug) {
    zvnprintf(132,"length of else function = %d\n",i);
  }
  elsefunction[i] = '$';                                  /* append a $ */
  elsefunction[i+1] = (char)0;                            /* append a nul */
  if (idebug) {
    zvnprintf(132,"elsefunction: %s\n",elsefunction);
  }
  r3 = &elsefunction[0];
  if (idebug) {
    zvnprintf(132,"%s\n",&r3[0]);
  }
  q3 = strchr(r3,'=')+1;                          /* need to trap this */
  if (q3 == NULL) {
    zvmessage ("No = in else function"," ");
    zmabend ("??E bad statement");
  }
  if (idebug) {
    zvnprintf(132,"else>%s",&q3[0]);
  }
  lparfound = 0;          /* init paren check */
  ifop = 0;
  qlp = strchr(r3,'$') +1;
  /*    strl = strlen(q2);  */
  diff = (unsigned int)(qlp - q3);                /* cast - May 05, 2011 */
  /* simple ( and ops checks (needs to be smarter) */
  for (m=0;m<(signed int)diff;m++) {              /* cast - May 05, 2011 */
    if (q3[m] == paren[0]) {         /* check for ( */
      lparfound = 1;
    }
    for (j=0;j<11;j++) {            /* check for op? */
      if (q3[m] == fop[j]) {
	ifop++;
      }
    }
    if (q3[m] == 'e') {          /*decrement for exponential (2 ops) */
      --ifop;
    }
  }
  if (ifop > 1 && lparfound == 0) {
    zvmessage("parentheses required for else function"," ");
    zmabend ("??E bad statement");
  }
  /********************/
  /* Done parsing - READ data */
  /********************/
  nincol = cptr;

  for (i=0;i<nincol;i++) {                              /* nincol is number of columns called */
    datcols[i] = atoi(&c_field[i][1]);                /* datcols[i] is each column called */

    if (datcols[i] > ncol) {
      zvnprintf(132,"Col %d > number of cols %d",datcols[i],ncol);
      zmabend("??E illegal column number");
    }                                                                                                                                                       
    if (idebug) {
      zvnprintf(132,"cfield[%d][1] = %s  datcols[%d] = %d",i,&c_field[i][1],i,datcols[i]);
    }
    status = IBISColumnGet(ibis,"FORMAT",fmtstring,datcols[i]);
    if (status!=1) IBISSignal(ibis,status,1);
    if (idebug) {
      zvnprintf(132,"   fmtstring = %s",fmtstring);
    }
    if (fmtstring[0]=='A') {
      /*         wid[i] = ms_num(&fmtstring[1])+1; */                     /* ms_num is number of ASCII chars */
      /*         typ[i] = 0;				*/		/* ascii */
      zvnprintf(132,"Selected col %d is ASCII format %s",datcols[i],fmtstring);
      zmabend("??E bad column format");
    } else {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",datcols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnGet(ibis,"U_SIZE",&wid[i],datcols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      typ[i] = 8; 		/* double */
    }
    wmx = wid[i]*tablen;
    totwid[i+1] = totwid[i]+((wmx+7)/8)*8;



  }  /* for (i=0;i<nincol;i++) */
  /* allocate double buffer */
  mz_alloc1((unsigned char **)&c_data,totwid[nincol],1);

  /* read appropriate column into c_data array */
  for (i=1;i<=ncol;i++) {                                        /* ncol is total columns in table */
    for (j=0;j<nincol;j++) {
      if (datcols[j]==i) {
	/* printf ("IBIS i = %d  j = %d read &c_data[totwid[j]] = %d \n",i,j,&c_data[totwid[j]]); */
	status = IBISColumnRead(ibis,&c_data[totwid[j]],i,1,tablen);        /* c_data[] is slurpped in data from file */
	if (status!=1) IBISSignal(ibis,status,1);
      } /* terminates 2 for loops */
    }
  }
  /* TEMPORARY         */
  /* Convert COL data into dbuf array                  */
  for (i=0;i<tablen;i++) {
    /* assemble input buffer into double format */
    for (j=0;j<nincol;j++) {
      /*            if (typ[j]!=0) */
      dbuf[j] = ffetchcd(totwid[j]+i*wid[j],typ[j],(unsigned char*)c_data);
      /* row in following print is in ibis convention - not c */
      if (idebug || code) {
	zvnprintf(132,"<<   original value in row %d col %d = %f",i+1,datcols[j],dbuf[j]);
      }
    }      /* for (j=0;j<nincol;j++) */
  }      /* for (i=0;i<tablen;i++) */

  /********************/
  /* generate pseudo code for if function */
  /********************/
  rcol = savvec[savptr++];
  if (idebug || code) {
    zvnprintf(132,"rcol = %d\n",rcol);
  }
  sptr = 0;
  svsptr = sptr;
  for (i=0;i<tablen;i++) {
    sptr = svsptr;
    for (j=0;j<nincol;j++) {
      if (typ[j]!=0) {
	dbuf[j] = ffetchcd(totwid[j]+i*wid[j],typ[j],(unsigned char*)c_data);
	/* row in following print is in ibis convention - not c */
	if (idebug || code) {
	  zvnprintf(132,"<<   original value in row %d col %d = %f",i+1,datcols[j],dbuf[j]);
	}
      } else {
	dbuf[j] = (double)sptr;
	/*bcopy(&c_data[totwid[j]+i*wid[j]],&sbuf[sptr],wid[j]);*/
	zmve(1,wid[j],&c_data[totwid[j]+i*wid[j]],&sbuf[sptr],1,1);
	sptr += wid[j]+1;
	sbuf[sptr-1] = (char)0;
      }
    } /* for (j=0;j<nincol;j++) */
    r1--;
    do r1++; while (*r1=='$');
    if (idebug) { 
      zvnprintf(132,"pseudo: %s\n",&r1[0]);
    }
    functionsize = iffunctionsize;
    sp_knuth(r1,ibuf,dbuf,sbuf,cnum,&sptr,functionsize);
    if (idebug) {
      zvnprintf(132,"sbuf = %s\n",sbuf);
    }
    dbuf[101] = (double)(i+1);
    debugrec1 = i;                 /* debugrec1 = i==0;  */
    sp_xknuth(ibuf,dbuf,sbuf,&sptr,&res,code);
    if (idebug) {
      zvnprintf(132,"   datcols[%d] = %d    result = %f",rcol,datcols[rcol],res);
    }
    svsptr = sptr;
    /********************/
    /* got result, generate pseudo code for then or else */
    /********************/
    ires = (int)(res+.001);      /* make sure 1 or 0 */
    if (ires == 1) {
      if (idebug) {
	zvnprintf(132,"ires= %d; then>%s",ires,&q2[0]);
      }
      q2--;
      do q2++; while (*q2=='$');
      sptr = 0;   
      functionsize = thenfunctionsize;
      sp_knuth(q2,ibuf,dbuf,sbuf,cnum,&sptr,functionsize);
      if (idebug) {
	zvmessage("\n"," ");
      }
    } else {
      if (idebug) {                
	zvnprintf(132,"ires= %d else>%s",ires,&q3[0]);
      }
      q3--;
      do q3++; while (*q3=='$');
      sptr = 0;
      functionsize = elsefunctionsize;
      sp_knuth(q3,ibuf,dbuf,sbuf,cnum,&sptr,functionsize);
      if (idebug) {
	zvmessage("\n"," ");
      }
    }
    /* execute the chosen function */
    dbuf[101] = (double)(i+1);
    debugrec1 = i;                 /* debugrec1 = i==0;  */
    sp_xknuth(ibuf,dbuf,sbuf,&sptr,&res,code);
    if (idebug) {
      zvnprintf(132,"   datcols[%d] = %d    result = %f",rcol,datcols[rcol],res);
    }
    svsptr = sptr;
    /********************/
    /* convert it - typ[rcol]>0 is numeric typ[rcol]==0 is ASCII */
    /********************/
    if (typ[rcol]!=0) {
      fstorecd(totwid[rcol]+i*wid[rcol],typ[rcol],res,(unsigned char*)c_data);

    } else {
      ires = (int)(res+.001);
      lres = MIN((int)strlen(&sbuf[ires]),wid[rcol]);             /* cast - May 05, 2011 */
      /*bcopy(&sbuf[ires],&c_data[totwid[rcol]+i*wid[rcol]],lres);*/
      zmve(1,lres,&sbuf[ires],&c_data[totwid[rcol]+i*wid[rcol]],1,1);
      /*bcopy(blanks,&c_data[totwid[rcol]+i*wid[rcol]+lres],wid[rcol]-lres);*/
      zmve(1,wid[rcol]-lres-1,blanks,&c_data[totwid[rcol]+i*wid[rcol]+lres],1,1);
      zmve(1,1,zeros,&c_data[totwid[rcol]+(i+1)*wid[rcol]-1],1,1);
    }
    if (idebug || code) {
      zvnprintf(132,">>   output value in row %d col %d = %f",i+1,datcols[rcol],(float) c_data[totwid[rcol]]);
    }
  } /* for (i=0;i<tablen;i++) */

    /********************/
    /* write results to appropriate columns in output */
    /********************/
  if (idebug) {
    zvnprintf(132,"datcols[%d] = %d\n",rcol,datcols[rcol]);
  }
  for (i=0;i<=ncol;i++) {
    for (j=0;j<nincol;j++) {
      if (datcols[j]==i&&tablen>0) {
	/*            totwid[j] = 4;
		      c_data[totwid[j]] = (float) 23;
	*/
	status = IBISColumnWrite(ibis,&c_data[totwid[j]],i,1,tablen);
	if (status!=1) IBISSignal(ibis,status,1);

      } /* terminate both for loops */
    }
  }
  /* say how many records modified */
  zvnprintf(132,"%d records out\n",tablen);
  /********************/
  /* close files */
  /********************/
  status = IBISFileClose(ibis,0);
  if (status!=1) IBISSignal(ibis,status,1);
  return;
}
