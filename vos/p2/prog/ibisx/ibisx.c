#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

#include "zvprintf.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "ibishelper.h"
#include "zifmessage.h"

#ifndef MAXCOLS
#define MAXCOLS 500
#endif 

#define IBISFORMAT( str, bufsiz, fmtchar, colsize )	\
  switch (tolower(fmtchar)) {				\
  case 'b':						\
  case 'h':								\
  case 'f': snprintf( str, bufsiz, "%%%dd",  (colsize));  break;	\
  case 'r': snprintf( str, bufsiz, "%%%d.2f",  (colsize));  break;	\
  case 'd': snprintf( str, bufsiz, "%%%d.2lf", (colsize));  break;	\
  case 'c': snprintf( str, bufsiz, " (%%-%d.2f,%%%d.2f)",		\
		      ((colsize)-4)/2, colsize - (4 + ((colsize)-4)/2) ); \
    break;								\
  case 'a': snprintf( str, bufsiz, "%%%ds", (colsize));break;		\
  default:  snprintf( str, bufsiz, "%%%ds", (colsize));break;		\
  }

static char buf[MAXCOLS * 100];

void printHeader(IBISStruct *ibis)
{
   int i, j, line[MAXCOLS];
   char *formats[MAXCOLS];

   zifmessage("");
   for(i = 0; i < ibis->nc; i++)
   {
      formats[i] = (ibis->formats)[i];

      if(!strcmp("byte", formats[i]) ||
         !strcmp("half", formats[i]) ||
         !strcmp("full", formats[i])) line[i] = 13;
      else if(!strcmp("real", formats[i]) ||
              !strcmp("doub", formats[i])) line[i] = 13;
      else if(!strcmp("comp", formats[i])) line[i] = 21;
      else if(formats[i][0] == 'a') line[i] = (ibis->colLens)[i] + 1;
      else IBISHELPER_wrongFormatError(ibis, i);
   }

   strcpy(buf, " ");
   for(i = 0; i < ibis->nc; i++)
   {
      for(j = 0; j < line[i] - 1; j++)
	strcat(buf, "-");
      strcat(buf, "+");
   }
   zifmessage(buf);

   strcpy(buf, " ");
   for(i = 0; i < ibis->nc; i++)
   {
      int numlen;

      numlen = 1;
      while(i/(numlen*10) != 0) numlen++;
      for(j = 0; j < line[i] - (3 + numlen); j++)
	strcat(buf, " ");
      strcat(buf, "C:");
      snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), "%d", i);
      strcat(buf, " ");
   }
   zifmessage(buf);

   strcpy(buf, " ");
   for(i = 0; i < ibis->nc; i++)
   {
      for(j = 0; j < line[i] - 1; j++)
	strcat(buf, "-");
      strcat(buf, "+");
   }
   zifmessage(buf);
}

void printRecord(IBISStruct *ibis, int index, char fmtstr[MAXCOLS][30])
{
  int i;
   char *elem;

   strcpy(buf, "");
   for(i = 0; i < ibis->nc; i++)
   {
      //      getElement(ibis, i, index, &elem);
      elem = IBISHELPER_getBufPtr(ibis, i, index);

      /* IBIS formatting and printing was copied over from ibis_list.c */
      switch (tolower((ibis->formats)[i][0])) {
      case 'b': snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstr[i], *(char *)  (elem));  break;
      case 'h': snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstr[i], *(short *) (elem));  break;
      case 'f': snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstr[i], *(long *)  (elem));  break;
      case 'r': snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstr[i], *(float *)  (elem));  break;
      case 'd': snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstr[i], *(double *)(elem));  break;
      case 'c': snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstr[i], ((float*)elem)[0],((float*)elem)[1] );break;
      case 'a': snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstr[i], elem );break;
      default:  snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstr[i], "BAD FORMAT!" );break;
      }
   }

   zifmessage(buf);
}

void getStatistics(IBISStruct *ibis, int *printFlags, double *mean, double *stdv, int nr)
{
   int i, nElems;
   double *sum;

   sum = (double *)calloc(sizeof(double)*ibis->nc*2, sizeof(double));
   if(!sum) zabend();

   nElems = 0;
   for(i = 0; i < ibis->nr; i++)
   {
      if(printFlags[i])
      {
         int j;

         for(j = 0; j < ibis->nc; j++)
         {
            char *format;
            char *elem;

            format = (ibis->formats)[j];
            if(format[0] != 'a') elem = IBISHELPER_getBufPtr(ibis, j, i); //getElement(ibis, j, i, &elem);
            else continue;

            if(!strcmp(format, "byte")) sum[j*2] += *((char*)elem);
            if(!strcmp(format, "half")) sum[j*2] += *((short int*)elem);
            if(!strcmp(format, "full")) sum[j*2] += *((int*)elem);
            if(!strcmp(format, "real")) sum[j*2] += *((float*)elem);
            if(!strcmp(format, "doub")) sum[j*2] += *((double*)elem);
            if(!strcmp(format, "comp"))
            {
               sum[j*2] += *((float*)elem);
               sum[j*2 + 1] += *((float*)(elem + sizeof(float)));
            }
         }

         ++nElems;
      }
   }

   if(nElems > 0)
      for(i = 0; i < ibis->nc; i++) mean[i*2] = sum[i*2]/nElems;

   for(i = 0; i < ibis->nr; i++)
   {
      if(printFlags[i])
      {
         int j;

         for(j = 0; j < ibis->nc; j++)
         {
            char *format;
            char *elem;

            format = (ibis->formats)[j];
            if(format[0] != 'a') elem = IBISHELPER_getBufPtr(ibis, j, i); //getElement(ibis, j, i, &elem);
            else continue;

            if(!strcmp(format, "byte")) stdv[j*2] += pow(*((char*)elem) - mean[j*2], 2);
            if(!strcmp(format, "half")) stdv[j*2] += pow(*((short int*)elem) - mean[j*2], 2);
            if(!strcmp(format, "full")) stdv[j*2] += pow(*((int*)elem) - mean[j*2], 2);
            if(!strcmp(format, "real")) stdv[j*2] += pow(*((float*)elem) - mean[j*2], 2);
            if(!strcmp(format, "doub")) stdv[j*2] += pow(*((double*)elem) - mean[j*2], 2);
            if(!strcmp(format, "comp"))
            {
               stdv[j*2] += pow(*((float*)elem) - mean[j*2], 2);
               stdv[j*2 + 1] += pow(*((float*)(elem + sizeof(float))) - mean[j*2 + 1], 2);
            }
         }
      }
   }

   for(i = 0; i < ibis->nc; i++)
   {
      if(stdv[i*2] > 10e-10) stdv[i*2] = sqrt(stdv[i*2]/(nElems - 1));
      if(stdv[i*2 + 1] > 10e-10) stdv[i*2 + 1] = sqrt(stdv[i*2 + 1]/(nElems - 1));
   }
}

void printStatistics(IBISStruct *ibis, char fmtstrs[MAXCOLS][30], double *mean, double *stdv)
{
   int i, line[MAXCOLS];
   char doubfmt[30], compfmt[30], *formats[MAXCOLS];

   /* print separating line */
   for(i = 0; i < ibis->nc; i++)
   {
      formats[i] = (ibis->formats)[i];

      if(!strcmp("byte", formats[i]) ||
         !strcmp("half", formats[i]) ||
         !strcmp("full", formats[i])) line[i] = 13;
      else if(!strcmp("real", formats[i]) ||
              !strcmp("doub", formats[i])) line[i] = 13;
      else if(!strcmp("comp", formats[i])) line[i] = 21;
      else if(formats[i][0] == 'a') line[i] = (ibis->colLens)[i] + 1;
      else IBISHELPER_wrongFormatError(ibis, i);
   }

   strcpy(buf, "");
   for(i = 0; i < ibis->nc; i++)
   {
      int j;

      for(j = 0; j < line[i] - 1; j++)
	strcat(buf, "-");
      strcat(buf, "+");
   }
   zifmessage(buf);

   IBISFORMAT(doubfmt, 30, 'd', 13);
   IBISFORMAT(compfmt, 30, 'c', 21);

   /* print the mean */
   strcpy(buf, "MEAN:");
   for(i = 0; i < ibis->nc; i++)
   {
      char *type;

      type = (ibis->formats)[i];
      if(type[0] == 'a')
      {
	snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstrs[i]," ");
	continue;
      }
      else if(!strcmp("comp", type))
	snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), compfmt, (float)(mean[i*2]), (float)(mean[i*2 + 1]));
      else
	snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), doubfmt, mean[i*2]);
   }
   zifmessage(buf);

   /* print the standard deviation */
   strcpy(buf, "STDV:");
   for(i = 0; i < ibis->nc; i++)
   {
      char *type;

      type = (ibis->formats)[i];
      if(type[0] == 'a')
      {
	snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), fmtstrs[i]," ");
	continue;
      }
      else if(!strcmp("comp", type))
	snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), compfmt, (float)(stdv[i*2]), (float)(stdv[i*2 + 1]));
      else
	snprintf(buf + strlen(buf), MAXCOLS * 100 + strlen(buf), doubfmt, stdv[i*2]);
   }
   zifmessage(buf);
}

void main44(void)
{
   int i, skip, randflag, seed, sr, nr, parmcnt, *printFlags, cnt, status, percent;
   IBISStruct *ibis;
   double *mean, *stdv;
   char fmtstrs[MAXCOLS][30];

   zifmessage("IBISX version 2019-07-31");

   /* get user input parameters */
   status = zvp("skip", &skip, &parmcnt);
   if(status != 1) zabend();
   randflag = zvptst("random");
   seed = zvptst("seed");
   status = zvp("sr", &sr, &parmcnt);
   if(status != 1) zabend();
   status = zvp("percent", &percent, &parmcnt);
   if(status != 1) zabend();
   status = zvp("nr", &nr, &parmcnt);
   if(status != 1) zabend();

   /* open ibis file */
   ibis = IBISHELPER_openIBIS("inp", 1, "read");

   /* allocate buffers */
   mean = (double*)calloc(sizeof(double)*ibis->nc*2, sizeof(double));
   stdv = (double*)calloc(sizeof(double)*ibis->nc*2, sizeof(double));
   printFlags = (int*)calloc(sizeof(int)*ibis->nr, sizeof(int));
   if(!printFlags || !stdv || !printFlags)
   {
     zifmessage("Error while allocating buffer.");
     if(!mean)
       zifmessage("CHECK MEAN BUFFER ALLOCATION.");
     if(!stdv)
       zifmessage("CHECK STDV BUFFER ALLOCATION.");
     if(!printFlags)
       zifmessage("CHECK PRINTFLAGS BUFFER ALLOCATION.");
     zabend();
   }

   /* preprocess parameters and decide which rows to print */
   if(sr > ibis->nr)
   {
     zvnprintf(60, "---> Number of rows in ibis file: %d", ibis->nr);
     zvnprintf(60, "---> Start row specified        : %d", sr);
   }

   if(skip && randflag)
     zifmessage("---> Both skip and random chosen... Ignoring random and doing skip.");

   if(nr && percent)
     zifmessage("---> Both nr and percent specified... Ignoring nr and doing percent.");

   cnt = 0;
   if(percent > 0) nr = (percent/100.0)*ibis->nr;
   else if(nr == 0 || nr > ibis->nr - sr) nr = ibis->nr - sr;
   if((!randflag && !skip) || nr == ibis->nr - sr)
   {
      for(i = sr; i < ibis->nr; i++)
      {
         printFlags[i] = 1;
         cnt++;
         if(cnt >= nr) break;
      }
   }
   else if(skip)
   {

      for(i = sr; i < ibis->nr && cnt < nr; i+=skip)
      {
         printFlags[i] = 1;
         cnt++;
      }
   }
   else
   {
      if(seed) srand(time(0));

      while(cnt < nr)
      {
         int randIndex;

         randIndex = (rand()/((double)RAND_MAX))*ibis->nr;

         if(!printFlags[randIndex] && randIndex >= sr)
         {
            printFlags[randIndex] = 1;
            cnt++;
         }
      }
   }

   IBISHELPER_getFormats(ibis, fmtstrs);
   printHeader(ibis);
   for(i = sr; i < ibis->nr; i++)
      if(printFlags[i]) printRecord(ibis, i, fmtstrs);

   /* get the statistics */
   getStatistics(ibis, printFlags, mean, stdv, nr);

   /* print statistics */
   printStatistics(ibis, fmtstrs, mean, stdv);

   /* free data */
   free(printFlags);
   free(mean);
   free(stdv);

   /* close ibis file */
   IBISHELPER_closeIBIS(&ibis);
}
