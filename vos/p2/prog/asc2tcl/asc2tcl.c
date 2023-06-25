#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "zifmessage.h"
#include "cartoStrUtils.h"
#include "cartoTaeUtils.h"

#define MAXTEXT    25000           /*  Number of characters in entire input */
#define MAXSTRPAR  130

/*  parse ascii file to fill a TAE TCL variable   A. Zobrist  5/26/00   */

#define BUF_LEN 3000

void main44(void)
{
  int i,sequence,lineno,vtype,comma,ct,def,ptr=0,keymatched;
  int int_result,atline,atitem,whitesp,vfound,lenkey,lenword=0,met;
  int btrim,etrim,len,keycount,ikey;
  double real_result;
  char *p = NULL,*q,char_result[MAXTEXT],star_result[MAXTEXT],quotestr[2];
  char infilename[99],buf[BUF_LEN + 1],keyvec[50][100],keystar[100];
  char keyword[100],outname[4];
  char outvec[50][4] = {"val","v2","v3","v4","v5","v6","v7","v8","v9",
			"v10","v11","v12","v13","v14","v15","v16","v17","v18","v19",
			"v20","v21","v22","v23","v24","v25","v26","v27","v28","v29",
			"v30","v31","v32","v33","v34","v35","v36","v37","v38","v39",
			"v40","v41","v42","v43","v44","v45","v46","v47","v48","v49","v50"};
  FILE *infile;
   
  zifmessage("ASC2TCL version 2019-08-07");
   
  /* null-terminate some bufs */
  buf[BUF_LEN] = '\0';
  char_result[MAXTEXT - 1] = '\0';
  star_result[MAXTEXT - 1] = '\0';

  /* get the parameters */
   
  zvparm("inp",infilename,&ct,&def,1,0);
  zvp("sequence",&sequence,&ct);
  zvp("lineno",&lineno,&ct);
  if (lineno>0) lineno--;
  zvparm("keyword",keyvec,&keycount,&def,50,100);   /* up to 50 entries - each 100 chars */
  if (def==1) keycount = 1;
  zvp("vtype",&vtype,&ct);
  comma = zvptst("comma");
  met = zvptst("met");
  zvp("btrim",&btrim,&ct);
  zvp("etrim",&etrim,&ct);
   
  quotestr[0] = (char)34;      /* quotes */
  quotestr[1] = (char)0;       /* null */
   
  infile = fopen(infilename,"r");
  if (infile == 0) {
    zmabend("??E - Input file not found");
  }
  
 
  /* loop over the keywords */
   
  for (ikey=0;ikey<keycount;ikey++)
    {
      strcpy(keyword,keyvec[ikey]);        /* isolate each keyword from keyvac */
      lenkey = (int)strlen(keyword);
      strcpy(keystar,keyword);
      /* replace space with * (star) */
      for (i=0;i<lenkey;i++) if (keystar[i]==' ') keystar[i] = '*';
   
      /* fseek to start of infile at end of loop */
   
      whitesp = 1;
      atline = 0;
      atitem = 0;
      vfound = 0;
      keymatched = 0;
      /* fread(void * buffer, size_t size, size_t members, FILE * infile); */
      fread(&buf[1],1,1,infile);
      /* read in file char by char */
      for (i=0;;i++)
	{
	  buf[0] = buf[1];   /* is lr1 parser*/
	  fread(&buf[1],1,1,infile);        /* read 1 char */
	  if (feof(infile))
	    {
	      if (lenkey>0 && strncmp(keystar,star_result,(size_t)lenkey)==0 &&
		  !(met&&!((lenword==lenkey+1) &&
			   ((char_result[lenkey]==10) || (char_result[lenkey]==34)))))
		{
		  keymatched = 1;
		  if (lenkey==lenword) atitem = 0;
		  else { p = char_result+lenkey; atitem = 1; }
		}
	      if (atline>=lineno&&atitem==sequence) vfound = 1;
	      break;
	    }
	  /* if char not printable (<32)   or ,(comma)   or " (dble quote)   */
	  if (((int)(buf[0])<=32 || (comma&&buf[0]==',') || (met&&buf[0]==34)) &&
	      !(lenword>0&&lenkey>0&&strncmp(keystar,star_result,(size_t)lenword)==0))
	    {
	      /* if LF or CR  */
	      if ((int)(buf[0])==10 || (int)(buf[0])==13) 
		{
		  if (atline<lineno) atitem = 0; 
		  atline++;
		}
	      if (whitesp) continue;
	      /* end of word case here */
	      whitesp = 1;
	      char_result[ptr] = (char)0;
	      star_result[ptr++] = (char)0;
	      lenword = (int)strlen(char_result);
         
	      if (lenkey>0&&strncmp(keystar,star_result,(size_t)lenkey)==0 &&
		  !(met&&!((lenword==lenkey+1) &&
			   ((char_result[lenkey]==10) || (char_result[lenkey]==34)))))
		{
		  keymatched = 1;
		  if (lenkey==lenword) atitem = 0;
		  else { p = char_result+lenkey; atitem = 1; }
		}
	      else p = char_result;
	      if (lenkey>0&&!keymatched) continue;
	      if (atline>=lineno && atitem==sequence)
		{
		  vfound = 1;
		  break;
		}
	    }  /* if (((int)(buf[0])<=32 */
	  else
	    {
	      if (whitesp)
		{
		  /* start of word case here */
		  whitesp = 0;
		  ptr = 0;
		  atitem++;
		}
	      char_result[ptr] = buf[0];
	      if (buf[0]==' ') star_result[ptr++] = '*';
	      else  star_result[ptr++] = buf[0];
	      char_result[ptr] = (char)0;
	      star_result[ptr] = (char)0;
	      lenword = (int)strlen(char_result);
	    }  /* else if (((int)(buf[0])<=32 */
	}  /* for (i=0;;i++)   char read   */
   
      /* this finds the value for the .met case only */
   
      if (met)
	{
	  for (i=0;i<3000;i++)
	    {
	      fread(&buf[i],1,1,infile);
	      buf[i+1] = (char)0;
	      if (feof(infile)) break;
	    }
	  p = buf;
	  while (strncmp(p," VALUE",6)!=0) p++;
	  while (strncmp(p,"=",1)!=0) p++;
	  while (strncmp(p,quotestr,1)!=0) p++;
	  p++;
	  q = p;
	  while (strncmp(q,quotestr,1)!=0) q++;
	  *q = (char)0;
	}  /* end of if (met)  */
   
      /* breaking out of read loop, result in char_result, vfound */
      /* need to augment the result to next cr/lf */
   
      if (vtype>0&&!met&&buf[0]!=(char)10&&buf[0]!=(char)13)
	{
	  buf[2] = (char)0;
	  strcat(p,buf);		/* p points into either char_result or buf; both are initialized null-terminated */
	  buf[0] = buf[1];
	  buf[1] = (char)0;
	  while (buf[0]!=(char)10&&buf[0]!=(char)13)
	    {
	      fread(&buf[0],1,1,infile);
	      if (feof(infile)) break;
	      strcat(p,buf);
	    }
	}
   
      if (!vfound)
	{
	  strcpy(char_result,"-999");
	  p = char_result;
	}
   
      strcpy(outname,outvec[ikey]);
      char msg_buf[31];
      snprintf(msg_buf, 30, "vtype %d", vtype);
      zifmessage(msg_buf);
      switch (vtype)
	{
	case 0: if (vfound)
	    {
	      len = (int)strlen(p); /* p points into either char_result or buf; both are initialized null-terminated */
	      *(p+len-etrim) = (char)0;
	      p += btrim;
	    }
	  mq_out_string(outname,p,MAXSTRPAR);
	  break;
	case 2: 
	case 4: int_result = ms_num(p);
	  mq_out_int(outname,int_result);
	  break;
	case 7: 
	case 8: real_result = ms_dnum(&p);
	  mq_out_real(outname,real_result);
	  break;
	} /* end switch */

      /* end of the keyword loop */
   
      fseek(infile,0,0);
    }   /* for (ikey=0;ikey<keycount;ikey++)  */
   
  /* close and exit */
   
  fclose(infile);
  return;
}
