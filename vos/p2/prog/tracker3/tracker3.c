/* TRACKER3 */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "vicmain_c"
#include "kqkkor.h"

#define MAX_LEFT_AREA 50 
#define MAX_RIGHT_AREA 150 

void main44(void)
{
 int unit1,unit2,unit3,unit4,status,nlw,nsw,nlw2,nsw2,count,def,grid;
 int nsout,ptcount,output_line,nl1,ns1,nl,ns,left_line,left_samp;
 int i,ind,right_line,right_samp,mode,limits,pmode,zero;
 int nids,input_line,inpt_count,nl4,ns4,extend,ascii;
 int tmpint; 
 float inbuf[32][4],obuf[32][4],line1,samp1,line2,samp2,percent;
 float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
 float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
 float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
 float line_offset,samp_offset;
 float line_coef[3],samp_coef[3];
 float line_coef_limits[3][2],samp_coef_limits[3][2];
 float line_temp[3],samp_temp[3],quality,line_range,samp_range;
 float coefrange[4],quality_limit,dn_left,dn_right;
 char correl_scheme[8],printit[8],zeromode[8],extendmode[8];
 char msg[80],asciifile[80];
 char header[] = "linel sampl liner sampr  dnl    dnr     X     Y     Z    Dr"; 
 FILE *fd;

 /* Nessesary variables for the KQKKOR correlation algorithm */
 int kq_nlw, kq_nsw, kq_nlw2, kq_nsw2;  /* New sizes for template and
					   search windows */
 KORPAR info; /* correlation parameters */
 short int made_good_guess; /* Control variable for KQKKOR guess loop */

 printf("*** tracker3 version 2017-08-11 ***\n");

/* get parameters */
 status=zvparm("NLW",&nlw,&count,&def,1,0);
 status=zvparm("NSW",&nsw,&count,&def,1,0);
 status=zvparm("NLAREA",&nlw2,&count,&def,1,0);
 status=zvparm("NSAREA",&nsw2,&count,&def,1,0);
 status=zvparm("GRID",&grid,&count,&def,1,0);
 status=zvparm("PERCENT",&percent,&count,&def,1,0);
 status=zvparm("LIMIT",&limits,&count,&def,1,0);
 status=zvparm("QUALITY",&quality_limit,&count,&def,1,0);
 status=zvparm("CORREL",correl_scheme,&count,&def,1,0);
 if(strcmp(correl_scheme,"LINEAR") == 0) mode=0;
 if(strcmp(correl_scheme,"ANNEAL") == 0) mode=1;
 if(strcmp(correl_scheme,"AMOEBA") == 0) mode=2;
 if(strcmp(correl_scheme,"LINAMO") == 0) mode=3;
 if(strcmp(correl_scheme,"ANNAMO") == 0) mode=4;

 if(strcmp(correl_scheme,"KQKKOR") == 0) mode=5;

 status=zvparm("COEFRANG",coefrange,&count,&def,4,0);
 pmode=0;
 status=zvparm("PRINT",printit,&count,&def,1,0);
 if(strcmp(printit,"PRINT") == 0) pmode=1;
 zero=0;
 status=zvparm("ZERO",zeromode,&count,&def,1,0);
 if(strcmp(zeromode,"ZERO") == 0) zero=1;
 extend=0;
 status=zvparm("EXTEND",extendmode,&count,&def,1,0);
 if(strcmp(extendmode,"EXTEND") == 0) extend=1;
 status=zvparm("ASCIIFILE",asciifile,&count,&def,1,0);
 ascii=1;
 if(strcmp(asciifile,"NONE") == 0) ascii=0;

/* check oddness */
 if(nlw == (nlw/2)*2) nlw += 1;
 if(nsw == (nsw/2)*2) nsw += 1;
 if(nlw2 == (nlw2/2)*2) nlw2 += 1;
 if(nsw2 == (nsw2/2)*2) nsw2 += 1;

/* check overflows */
 if(nlw > MAX_LEFT_AREA){
   sprintf(msg,"nlw exceeds limit of %d",MAX_LEFT_AREA);
   zvmessage(msg," ");
   zabend();
 }
 if(nsw > MAX_LEFT_AREA){
   sprintf(msg,"nsw exceeds limit of %d",MAX_LEFT_AREA);
   zvmessage(msg," ");
   zabend();
 }
 if(nlw2 > MAX_RIGHT_AREA){
   sprintf(msg,"nlarea exceeds limit of %d",MAX_RIGHT_AREA);
   zvmessage(msg," ");
   zabend();
 }
 if(nsw2 > MAX_RIGHT_AREA){
   sprintf(msg,"nsarea exceeds limit of %d",MAX_RIGHT_AREA);
   zvmessage(msg," ");
   zabend();
 }


/* open files */
 status=zvpcnt("INP",&nids);
 nsout=128;
 status=zvunit(&unit1,"INP",1, NULL);
 status=zvopen(unit1,"OP","READ","U_FORMAT","REAL", NULL);
 status=zvunit(&unit2,"INP",2, NULL);
 status=zvopen(unit2,"OP","READ","U_FORMAT","REAL", NULL);
 status=zvunit(&unit3,"OUT",1, NULL);
 status=zvopen(unit3,"OP","WRITE","U_FORMAT","REAL","U_NS",nsout,
               "U_NL",100,"O_FORMAT","REAL", NULL);
 if(nids > 2){
   status=zvunit(&unit4,"INP",3, NULL);
   status=zvopen(unit4,"OP","READ","U_FORMAT","REAL", NULL);
 }
 if(ascii == 1){   /* open ascii file "asciifile" */
   if ((fd = fopen(asciifile, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s\n", asciifile);
      zabend();
   }
 }

/* fill initial records in the ascii file */
 if(ascii == 1){
   fprintf(fd,"%s\n","Tracker3 ascii tiepoints file");
   status=zvpone("INP",msg,1,80); /* first input filename */
   fprintf(fd,"%s\n",msg);
   status=zvpone("INP",msg,2,80); /* second input filename */
   fprintf(fd,"%s\n",msg);
   fprintf(fd,"%s\n","Placeholder");
   fprintf(fd,"%s\n","Placeholder");
   fprintf(fd,"%s\n",header);
 }

/* get input image dimensions */
 status=zvget(unit1,"NL",&nl1,"NS",&ns1, NULL);
 status=zvget(unit2,"NL",&nl,"NS",&ns, NULL);
 if(nl > nl1) nl=nl1;
 if(ns > ns1) ns=ns1;
 if(nids > 2){
    status=zvget(unit4,"NL",&nl4,"NS",&ns4, NULL);
 }   

/* initialize constants */
 inpt_count=32;
 input_line=0;
 ptcount=0;
 output_line=0;
 left_samp=(nsw2/2)+1-grid;
 left_line=(nlw2/2)+1 ;
 line_range=((float)nlw2-(float)nlw)/2.0;
 samp_range=((float)nsw2-(float)nsw)/2.0;

/* set coefficient boundary limits */
 line_coef_limits[0][0]=coefrange[2];
 line_coef_limits[0][1]=coefrange[3];
 line_coef_limits[1][0]=coefrange[0];
 line_coef_limits[1][1]=coefrange[1];
 line_coef_limits[2][0]= -line_range;
 line_coef_limits[2][1]= line_range;
 samp_coef_limits[0][0]=coefrange[0];
 samp_coef_limits[0][1]=coefrange[1];
 samp_coef_limits[1][0]=coefrange[2];
 samp_coef_limits[1][1]=coefrange[3];
 samp_coef_limits[2][0]= -samp_range;
 samp_coef_limits[2][1]= samp_range;

/* set initial coefficient temperature values */
 line_temp[0]=(line_coef_limits[0][1]-line_coef_limits[0][0])/12.0;
 line_temp[1]=(line_coef_limits[1][1]-line_coef_limits[1][0])/12.0;
 line_temp[2]=line_range/12.;
 samp_temp[0]=(samp_coef_limits[0][1]-samp_coef_limits[0][0])/12.0;
 samp_temp[1]=(samp_coef_limits[1][1]-samp_coef_limits[1][0])/12.0;
 samp_temp[2]=samp_range/12.;


 
/* acquire a new tiepoint */
 new_point:

/* determine initial tiepoint image location */
 if(nids == 2){
   if(left_samp+grid > ns-(nsw2/2)){
     left_samp=(nsw2/2)+1;
     left_line += grid;
     if(left_line > nl-(nlw2/2)) goto end_points;}
   else{
     left_samp += grid;
   }
   line1=left_line;
   samp1=left_samp;
   line2=line1;
   samp2=samp1;
 }
 else{
   if(inpt_count == 32){
     inpt_count=0;
     input_line += 1;
     if(input_line > nl4) goto end_points;
     status=zvread(unit4,inbuf,"LINE",input_line, NULL);
   }
   if(extend == 1){ /* true */
     line1=inbuf[inpt_count][2];
     samp1=inbuf[inpt_count][3];
     line2=line1;
     samp2=samp1;
     inpt_count += 1;
     if((input_line == nl4) & (line1 == 0.0) & (samp1 == 0.0)) goto end_points;
   }
   else{
     line1=inbuf[inpt_count][0];
     samp1=inbuf[inpt_count][1];
     line2=inbuf[inpt_count][2];
     samp2=inbuf[inpt_count][3];
     inpt_count += 1;
     if((input_line == nl4) & (line1 == 0.0) & (samp1 == 0.0) &
        (line2 == 0.0) & (samp2 == 0.0)) goto end_points;
   }
 }

/* locate template in search area, return offset */
/* set initial coefficient values */
 line_coef[0]=0.;
 line_coef[1]=1.;
 line_coef[2]=0.;
 samp_coef[0]=1.;
 samp_coef[1]=0.;
 samp_coef[2]=0.;
 /* Choose between Gruen correlation and KQKKOR correlation */
 /* ( Ackermann, 1984)                                      */
 /*                                                         */

 if ( mode != 5 ) {
   /* read left image template */
   read_left_area(left,correl,line1,samp1,unit1,nl,ns,nlw,nsw,zero,&ind);
   if(ind == 1) goto new_point;

   /* read right search area */
   read_right_area(right,correl,line2,samp2,unit2,nl,ns,nlw2,nsw2,zero,&ind);
   if(ind == 1) goto new_point;


   gruen(left,nlw,nsw,right,nlw2,nsw2,correl,&line_offset,&samp_offset,
	 line_coef,samp_coef,line_coef_limits,samp_coef_limits,
	 line_temp,samp_temp,percent,limits,&quality,mode,&ind); 
 } else { 
   /* call_kqkkor routine maps inputs to the kqkkor program */
   /* forms inputs, makes nessesary calls and forms output  */
   /* for tracker3                                          */
   /* Added by Anton B. Ivanov (abi@mipl7), Apr 17 2000     */

 /* If we have found a correlation before, determine next best guess for the
    affine transformation paramters, based on currently obtained
    points. Note that feature tracking, these parameters will vary
    widely across the image.  */


   /*      fprintf( stderr, "%d %f %f\n", ptcount, line2, samp2);        */
   made_good_guess = 0.0; 
   quality = 0.0; 

   /* Make a guess on where the tiepoint should be */
   status = call_affinpar( line1, samp1, obuf, ptcount, &line2, &samp2, line_coef, samp_coef ); 
   /* status = call_affinpar( 0, 0,  obuf, ptcount, &line2, &samp2, line_coef, samp_coef ); */
   /* status = 0; */
   /* If an affine transform is not found, reset to standard best guess */
   if ( status < 1 ) {
     /* Reset line_coef and samp_coef to original values.*/
     line_coef[0]=0.;
     line_coef[1]=1.;
     line_coef[2]=0.;
     samp_coef[0]=1.;
     samp_coef[1]=0.;
     samp_coef[2]=0.;
     made_good_guess = 1.0; 
   }
   
   /* Iterate, until a good guess is obtained           */
   /* By default the guess made by affpar is not good.  */
   /* It is good only if the resulting quality          */
   /* will be higher than 0.0                           */

   do { 

     /* If quality returned by the previous iteration was not good   */
     /* return to the initial guess, and restore some default values */
     if ( quality == -1.0 ) {
        line_coef[0]=0.; line_coef[1]=1.; line_coef[2]=0.;
	samp_coef[0]=1.; samp_coef[1]=0.; samp_coef[2]=0.;
	line2 = line1; samp2 = samp1; 
	made_good_guess = 1; 
     }
     
     /* First find optimal dimensions for KQKKOR operation    */
     kqkkor_dimensions( nlw, nlw2, line_coef, samp_coef, quality_limit, 
			&kq_nlw, &kq_nsw, &kq_nlw2, &kq_nsw2, 
			&info);
 /*     fprintf( stderr, "%d %f %f %f %f %f %f %d %d \n", status, line_coef[0], line_coef[1], line_coef[2],    */
 /* 	      samp_coef[0], samp_coef[1], samp_coef[2], kq_nlw, kq_nlw2);   */

     /* Check kq_nlw for sanity, because sometimes the initial guess is pretty bad */
     if ( kq_nlw != kq_nsw || kq_nlw2 != kq_nsw2 || kq_nlw > nl || 
	  kq_nlw >= MAX_LEFT_AREA || kq_nlw2 >= MAX_RIGHT_AREA) {
        quality = -1.0;
     } else {
       /* Read the data in */
       /* read left image template */
       read_left_area(left,correl,line1,samp1,unit1,nl,ns,kq_nlw,kq_nsw,zero,&ind);
       if(ind == 1 && made_good_guess) goto new_point;
       if ( ind == 1 && !made_good_guess) {
	 quality = -1.0; 
       } else {
	 /* read right search area */
	 read_right_area(right,correl,line2,samp2,unit2,nl,ns,kq_nlw2,kq_nsw2,zero,&ind);
	 if(ind == 1 && made_good_guess ) goto new_point;
	 if ( ind == 1 && !made_good_guess ) {
	   quality = -1.0;
	 } else {
	   /* Do the correlation */
	   /*  fprintf( stderr, "\t\t \t \tActually used : %d %f %f %f %f %f %f %d \n", status, line_coef[0], line_coef[1], line_coef[2], samp_coef[0], samp_coef[1], samp_coef[2], ptcount);  */
	   call_kqkkor( &info, left, kq_nlw, kq_nsw, right, kq_nlw2, kq_nsw2, 
			line_coef, samp_coef, 
			&line_offset, &samp_offset, &quality, &ind); 
	 }
       }
/*         fprintf( stderr, "Tried " );   */
     }
/*      fprintf( stderr, "%d\t%d", kq_nlw, kq_nlw2); fprintf( stderr, "\t%f\n", quality );   */

   } while ( quality < 0.0 && made_good_guess != 1); 
 }

 if(ind == 1) goto new_point;
 if(ind == 2) goto new_point;
 if(quality <= quality_limit) goto new_point;
 
/*  if ( made_good_guess == 0) fprintf( stderr, "Made good guess %f %f\n", line2 - line1, samp2 - samp1); */

/* Compute right image coordinates from offsets */
 line2=line2 + line_offset;
 samp2=samp2 + samp_offset;

/* add new point to output record */
 ptcount += 1;
 obuf[ptcount-1][0]=line1;
 obuf[ptcount-1][1]=samp1;
 obuf[ptcount-1][2]=line2;
 obuf[ptcount-1][3]=samp2;

/* write record to ascii file if desired */
 if(ascii == 1){
   left_area_dn(left,nlw,nsw,&dn_left);
   right_area_dn(right,nlw2,nsw2,nlw,nsw,line_offset,samp_offset,&dn_right);
   fprintf(fd,"%.2f %.2f %.2f %.2f %.2f %.2f\n",line1,samp1,line2,samp2,
         dn_left,dn_right);
 }

/* if(mode != 0){
  printf("LineCoef=%f %f %f\n",line_coef[0],line_coef[1],line_coef[2]);
  printf("SampCoef=%f %f %f\n",samp_coef[0],samp_coef[1],samp_coef[2]);
 } */

 if(pmode == 1){
   sprintf(msg,"Line=%f Samp=%f Line=%f Samp=%f Qual=%f",
       line1,samp1,line2,samp2,quality);
   zvmessage(msg," ");
 }

/* if output record full, write it out */
 if(ptcount == 32){
    ptcount=0;
    output_line += 1;
    status=zvwrit(unit3,obuf,"LINE",output_line, NULL);
 }

 goto new_point;
 end_points: /* end of tiepoints acquisition cycle */

/* write out partial record of tiepoints */
 if(ptcount > 0){
   for(i=ptcount; i < 32; i++){
     obuf[i][0]=0.0;
     obuf[i][1]=0.0;
     obuf[i][2]=0.0;
     obuf[i][3]=0.0;
   }
     output_line += 1;
     status=zvwrit(unit3,obuf,"LINE",output_line, NULL);
 }

/* update output file dimensions */
 status=zldel(unit3,"SYSTEM","NL", NULL);
 zvsignal(unit3,status,1);
 status=zladd(unit3,"SYSTEM","NL",&output_line,"FORMAT","INT", NULL);
 zvsignal(unit3,status,1);

/* close ascii file if present */
 if(ascii == 1) fclose(fd);
}

/********************************************************************/
/* return mean dn of left area */

left_area_dn(left,nlw,nsw,dn)

float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
int nlw,nsw;
float *dn;
{
 int j,i;
 *dn=0.0;
 for(j=0; j < nlw; j++){
   for(i=0; i < nsw; i++){
     *dn += left[j][i];
   }
 }
 *dn = *dn/(nlw*nsw);
}

/********************************************************************/
/* return mean dn of right area */

right_area_dn(right,nlw2,nsw2,nlw,nsw,line_offset,samp_offset,dn)

float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float line_offset,samp_offset,*dn;
int nlw2,nsw2,nlw,nsw;
{
 int j,i,n,m;
 *dn=0.0;

 n=nlw2/2.0+line_offset-nlw/2.0+0.5;
 m=nsw2/2.0+samp_offset-nsw/2.0+0.5;
 if(n < 0) n=0;
 if(m < 0) m=0;
 if(n+nlw-1 > nlw2) n=nlw2-nlw+1;
 if(m+nsw-1 > nsw2) m=nsw2-nsw+1;

 for(j=n; j < n+nlw; j++){
   for(i=m; i < m+nsw; i++){
     *dn += right[j][i];
   }
 }
 *dn = *dn/(nlw*nsw);
}

/*************************************************************************
*/
amoeba(P,Y,ITER,nlw,nsw,nlw2,nsw2,left,right,
       coef_limits,sumx,left_sum,percent)

float P[6][7],Y[7];
int *ITER;

/* cost function arguments */
int nlw,nsw,nlw2,nsw2;
float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float coef_limits[6][2],percent;
double sumx[10],left_sum[10];
{
 int I,J,MP,NP,NDIM,ITMAX,MPTS,ILO,IHI,INHI,ind;
 float quality;
 float FTOL,RTOL,ALPHA,BETA,GAMMA,YPR,YPRR;
 float PR[7],PRR[7],PBAR[7];

 NP=6;
 MP=7;
 NDIM=6;
 FTOL=.00001;
 ITMAX=5000;
 ALPHA=1.0;
 BETA=0.5;
 GAMMA=2.0;
 MPTS=NDIM+1;
 *ITER=0;

 LOOP:

 ILO=0;
 if(Y[0] > Y[1]){
   IHI=0;
   INHI=1;
 }
 else{
   IHI=1;
   INHI=0;
 }

 for(I=0; I < MPTS; I++){
   if(Y[I] < Y[ILO]) ILO=I;
   if(Y[I] > Y[IHI]){
     INHI=IHI;
     IHI=I;
   }
   else if(Y[I] > Y[INHI]){
       if(I != IHI) INHI=I;
   }
 }

 RTOL=2.*fabs(Y[IHI]-Y[ILO])/(fabs(Y[IHI])+fabs(Y[ILO]));
 if(RTOL < FTOL){
   /*printf("Iterations=%d\n",*ITER);*/
   return;
 }
 if(*ITER == ITMAX){
   printf("Amoeba exceeding maximum iterations\n");
   return; 
 }
 *ITER= *ITER+1;
 for(J=0; J < NDIM; J++){
   PBAR[J]=0.;
 }
 for(I=0; I < MPTS; I++){
   if(I != IHI){
     for(J=0; J < NDIM; J++){
       PBAR[J]=PBAR[J]+P[J][I];
     }
   }
 }
 for(J=0; J < NDIM; J++){
   PBAR[J]=PBAR[J]/NDIM;
   PR[J]=(1.+ALPHA)*PBAR[J]-ALPHA*P[J][IHI];
 }
   cost(PR,coef_limits,left,nlw,nsw,right,nlw2,nsw2,sumx,left_sum,
     percent,&quality,&ind);
   YPR= quality+1.;
   if(YPR <= Y[ILO]){
        for(J=0; J < NDIM; J++){
          PRR[J]=GAMMA*PR[J]+(1.-GAMMA)*PBAR[J];
        }
        cost(PRR,coef_limits,left,nlw,nsw,right,nlw2,nsw2,sumx,left_sum,
             percent,&quality,&ind);
        YPRR= quality+1.;
        if(YPRR < Y[ILO]){
          for(J=0; J < NDIM; J++){
            P[J][IHI]=PRR[J];
          }
          Y[IHI]=YPRR;
        }
        else{
          for(J=0; J < NDIM; J++){
            P[J][IHI]=PR[J];
          }
          Y[IHI]=YPR;
        }
   }
   else if(YPR >= Y[INHI]){
        if(YPR < Y[IHI]){
          for(J=0; J < NDIM; J++){
            P[J][IHI]=PR[J];
          }
          Y[IHI]=YPR;
        }
        for(J=0; J < NDIM; J++){
          PRR[J]=BETA*P[J][IHI]+(1.-BETA)*PBAR[J];
        }
        cost(PRR,coef_limits,left,nlw,nsw,right,nlw2,nsw2,sumx,left_sum,
              percent,&quality,&ind);
        YPRR= quality+1.;
        if(YPRR < Y[IHI]){
          for(J=0; J < NDIM; J++){
            P[J][IHI]=PRR[J];
          }
          Y[IHI]=YPRR;
        }
        else{
          for(I=0; I < MPTS; I++){
            if(I != ILO){
              for(J=0; J < NDIM; J++){
                PR[J]=0.5*(P[J][I]+P[J][ILO]);
                P[J][I]=PR[J];
              }
              cost(PR,coef_limits,left,nlw,nsw,right,nlw2,
                   nsw2,sumx,left_sum,percent,&quality,&ind);
              Y[I]= quality+1.;
            }
          }
        }
   }
   else{
        for(J=0; J < NDIM; J++){
          P[J][IHI]=PR[J];
        }
        Y[IHI]=YPR;
   }
 goto LOOP;
}


/********************************************************************
Objective function to be minimized                                  
Polynomial is of the form:
rightline=answer[0]*leftsamp+answer[1]*leftline+answer[2]
rightsamp=answer[3]*leftsamp+answer[4]*leftline+answer[5]
*/
cost(answer,coef_limits,left,nlw,nsw,right,nlw2,nsw2,sumx,left_sum,
     percent,quality,ind)

int nlw,nsw,nlw2,nsw2,*ind;
float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float *quality,answer[6],coef_limits[6][2],percent;
double sumx[10],left_sum[10];
{

 int j,i,m,n,kl,ks,num_areas,k,kll,kss;
 float left_center_line,left_center_samp,right_center_line,right_center_samp;
 float r1,r2,x,y,right_line,right_samp,wl,wr,wt;
 float top,bot,rnlw2,rnsw2,best_r2;
 double sumy,sumy2,sumxy,rn,right_dn;

 *quality=1.0; /* worst return value possible */

/* check if solution limits are violated */
 for(i=0; i < 6; i++){
   if((answer[i] < coef_limits[i][0]) || 
      (answer[i] > coef_limits[i][1])){
      *ind=1;
      return;
   }
 }

/* constants */
 left_center_line=(nlw-1)/2.0;
 left_center_samp=(nsw-1)/2.0;
 right_center_line=(nlw2-1)/2.0;
 right_center_samp=(nsw2-1)/2.0;

/* check if corners of left area fall within right area */
 rnlw2=nlw2-1;
 rnsw2=nsw2-1;
 y= -left_center_line;
 x= -left_center_samp;
 right_line=x*answer[0]+y*answer[1]+answer[2]+right_center_line;
 right_samp=x*answer[3]+y*answer[4]+answer[5]+right_center_samp;
 if((right_line < 0.0) || (right_line > rnlw2) ||
    (right_samp < 0.0) || (right_samp > rnsw2)){
    *ind=1;
    return;
 }
 y= -left_center_line;
 x= (nsw-1)-left_center_samp;
 right_line=x*answer[0]+y*answer[1]+answer[2]+right_center_line;
 right_samp=x*answer[3]+y*answer[4]+answer[5]+right_center_samp;
 if((right_line < 0.0) || (right_line > rnlw2) ||
    (right_samp < 0.0) || (right_samp > rnsw2)){
    *ind=1;
    return;
 }
 y= (nlw-1)-left_center_line;
 x= -left_center_samp;
 right_line=x*answer[0]+y*answer[1]+answer[2]+right_center_line;
 right_samp=x*answer[3]+y*answer[4]+answer[5]+right_center_samp;
 if((right_line < 0.0) || (right_line > rnlw2) ||
    (right_samp < 0.0) || (right_samp > rnsw2)){
    *ind=1;
    return;
 }
 y=(nlw-1)-left_center_line;
 x=(nsw-1)-left_center_samp;
 right_line=x*answer[0]+y*answer[1]+answer[2]+right_center_line;
 right_samp=x*answer[3]+y*answer[4]+answer[5]+right_center_samp;
 if((right_line < 0.0) || (right_line > rnlw2) ||
    (right_samp < 0.0) || (right_samp > rnsw2)){
    *ind=1;
    return;
 }

/* compute sub area limits if percent < 100 */
 num_areas=1;
 if(percent < 99.99){
   num_areas=5;
   kl=(nlw*(percent/100.)) + .5;
   if(kl > nlw) kl=nlw;
   if(kl < 1) kl=1;
   ks=(nsw*(percent/100.)) + .5;
   if(ks > nlw) ks=nlw;
   if(ks < 1) ks=1;
   kll=nlw-kl-1;
   kss=nsw-ks-1;
 }

/* loop on all sub areas */
 best_r2=0.;
 for(k=0; k < num_areas; k++){

/* compute area of sub area */
   if(k == 0) rn=nlw*nsw;
   if(k == 1) rn=kl*nsw;
   if(k == 2) rn=kl*nsw;
   if(k == 3) rn=ks*nlw;
   if(k == 4) rn=ks*nlw;

/* compute coefficient of determination r2 */
   sumy=0.0;
   sumy2=0.0;
   sumxy=0.0;
   for(j=0; j < nlw; j++){
     y=j-left_center_line;
     /* store redundant terms */
     r1=y*answer[1]+answer[2]+right_center_line;
     r2=y*answer[4]+answer[5]+right_center_samp;
     for(i=0; i < nsw; i++){
       if(k == 0) goto compute;
       if(k == 1){
         if(j > kll) goto compute;
         continue;
       }
       if(k == 2){
         if(j < kl) goto compute;
         continue;
       }
       if(k == 3){
         if(i > kss) goto compute;
         continue;
       }
       if(k == 4){
         if(i < ks) goto compute;
         continue;
       }
       compute:
       x=i-left_center_samp;
       right_line=x*answer[0]+r1;
       right_samp=x*answer[3]+r2;
       m=right_samp;
       n=right_line;
       wl=right_samp-m;
       wr=1.0-wl;
       top=wl*right[n][m+1]+wr*right[n][m]; /* bilinear interpolation */
       bot=wl*right[n+1][m+1]+wr*right[n+1][m];
       wt=right_line-n;
       right_dn=bot*wt+top*(1.0-wt); /* got right DN value by interpolation */
       sumy += right_dn; /* compute sums for least squares fit */
       sumy2 += right_dn*right_dn;
       sumxy += right_dn*left[j][i];
     }
   }
   r2=pow((sumxy-sumx[k]*sumy/rn),2.0)/(left_sum[k]*(sumy2-sumy*sumy/rn));
   if(r2 > best_r2) best_r2=r2;
 }/* end of areas loop */

 *quality=1.0-best_r2;
 *ind=0;

}

/********************************************************************
Routine to perform conventional correlation only.
Correl is filled with the matrix of coefficient of determination values.
Then the peak value is interpolated to sub pixel.
Answer is returned with the equivalent polynomial mapping from
left to right areas centered on the central pixel.
*/

search_area(left,nlw,nsw,right,nlw2,nsw2,correl,sumx,left_sum,
            percent,answer,quality,ind)

float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float *quality,answer[6],percent;
double sumx[10],left_sum[10];
int nlw,nsw,nlw2,nsw2,*ind;

{
 double rn,right_dn,sumy,sumxy,sumy2;
 float r2,a,b,c,line_val[10],samp_val[10],qual[10];
 float denom,sample,line;
 int i,j,m,n,kl,ks,num_areas,k,kll,kss;

/* compute sub area limits if percent < 100 */
 num_areas=1;
 if(percent < 99.99){
   num_areas=5;
   kl=(nlw*(percent/100.)) + .5;
   if(kl > nlw) kl=nlw;
   if(kl < 1) kl=1;
   ks=(nsw*(percent/100.)) + .5;
   if(ks > nlw) ks=nlw;
   if(ks < 1) ks=1;
   kll=nlw-kl-1;
   kss=nsw-ks-1;
 }

/* loop on all sub areas */
 for(k=0; k < num_areas; k++){

/* compute area of sub area */
   if(k == 0) rn=nlw*nsw;
   if(k == 1) rn=kl*nsw;
   if(k == 2) rn=kl*nsw;
   if(k == 3) rn=ks*nlw;
   if(k == 4) rn=ks*nlw;

/* fill correlation matrix */
   for(n=0; n < nlw2-nlw+1; n++){
     for(m=0; m < nsw2-nsw+1; m++){
       sumy=0.0;
       sumy2=0.0;
       sumxy=0.0;
       for(j=0; j < nlw; j++){
         for(i=0; i < nsw; i++){
           if(k == 0) goto compute;
           if(k == 1){
             if(j > kll) goto compute;
             continue;
           }
           if(k == 2){
             if(j < kl) goto compute;
             continue;
           }
           if(k == 3){
             if(i > kss) goto compute;
             continue;
           }
           if(k == 4){
             if(i < ks) goto compute;
             continue;
           }
           compute:
           right_dn=right[j+n][i+m];
           sumy += right_dn;
           sumy2 += right_dn*right_dn;
           sumxy += right_dn*left[j][i];
         }
       }
       correl[n][m]=pow((sumxy-sumx[k]*sumy/rn),2.0)/
            (left_sum[k]*(sumy2-sumy*sumy/rn));
     }
   }
  
/* locate highest value */
   r2= -1.0;
   for(n=0; n < nlw2-nlw+1; n++){
     for(m=0; m < nsw2-nsw+1; m++){
       if(r2 < correl[n][m]){
         r2=correl[n][m];
         j=n;
         i=m;
       }
     }
   }

/* reject point if on border */
   if((j == 0) || (j == nlw2-nlw+1) || (i == 0) || (i == nsw2-nsw+1)){
     if(num_areas == 1){
       *ind=1;
       return;
     }
     else{
       qual[k]=0.;
       goto loopend;
     }
   }

/* compute sub-pixel location of best correlation.
   See Numerical Recipes, eqn: 10.2.1, note b+1/2 should read b-1/2   */
   a=correl[j][i-1];
   b=2.0*correl[j][i];
   c=correl[j][i+1];
   denom=2.0*(b-c-a);
   if(denom != 0.0){
     sample=(c-a)/denom+i+(nsw-1)/2;}
   else{
     sample=i+(nsw-1)/2;
   }
   a=correl[j-1][i];
   c=correl[j+1][i];
   denom=2.0*(b-c-a);
   if(denom != 0.0){
     line=(c-a)/denom+j+(nlw-1)/2;}
   else{
     line=j+(nlw-1)/2;
   }

   line_val[k]=line;
   samp_val[k]=sample;
   qual[k]=r2;
   loopend:;
 } /* end of areas loop */

/* locate best area fit */
 if(num_areas > 1){
   i= -1;
   r2=0.;
   for(k=0; k < num_areas; k++){
     if(qual[k] > r2){
       r2=qual[k];
       i=k;
     }
   }
   if(i == -1){
     *ind=1;
     return;
   }
   line=line_val[i];
   sample=samp_val[i];
   *quality=qual[i];
 }
 else
   *quality=correl[j][i];

/* assemble polynomial terms mapping left to right 
 rightline=answer[0]*leftsamp+answer[1]*leftline+answer[2]
 rightsamp=answer[3]*leftsamp+answer[4]*leftline+answer[5]
*/
 answer[0]=0.0;
 answer[1]=1.0;
 answer[2]=line-(nlw2-1)/2;
 answer[3]=1.0;
 answer[4]=0.0;
 answer[5]=sample-(nsw2-1)/2;

 *ind=0;
}


/********************************************************************
ARGUMENTS:

left=left image area template,[MAX_LEFT_AREA][MAX_LEFT_AREA]     float

nlw= # lines left area. odd number                               int

nsw= # samples left area. odd number                             int

right=right image search area, [MAX_RIGHT_AREA][MAX_RIGHT_AREA]  float

nlw2=# lines right area. odd number                              int

nsw2=# samples right area. odd number                            int

correl=correlation matrix, [MAX_RIGHT_AREA][MAX_RIGHT_AREA]      float
       Returned. Only used in mode=0.

line_offset=line shift to correct initial registration, returned float
            Negative means shift upwards.

samp_offset=sample shift to correct initial registration, returned float
            Negative means shift to the left.

line_coef=line equation coefficients,[3], returned               float
          Mapping polynomial from template to search area.
          rightline=line_coef[0]*leftsamp+line_coef[1]*leftline+line_coef[2]
          On entry contains best estimate of line polynomial solution.
          Try: 0., 1., 0.

samp_coef=sample equation coefficients,[3], returned               float
          Mapping polynomial from template to search area.
          rightsamp=samp_coef[0]*leftsamp+samp_coef[1]*leftline+samp_coef[2]
          On entry contains best estimate of sample polynomial solution.
          Try: 1., 0., 0.

line_coef_limits=initial boundary conditions, [3][2]             float
          Only used in modes 1 and 4.
          [][0] is negative limit
          [][1] is positive limit
          These limit the possible range of mapping polynomial values.

samp_coef_limits=initial boundary conditions, [3][2]             float
          Only used in modes 1 and 4.
          [][0] is negative limit
          [][1] is positive limit
          These limit the possible range of mapping polynomial values.

line_temp=line equation coefficients temperature,[3]              float
          Only used in modes 1 and 4.
          The initial guessing range for solutions of polynomial values.
          Try: (line_coef_limits[][1]-line_coef_limits[][0])/12.

samp_temp=sample equation coefficients temperature,[3]           float
          Only used in modes 1 and 4.
          The initial guessing range for solutions of polynomial values.
          Try: (samp_coef_limits[][1]-samp_coef_limits[][0])/12.

percent=Percentage of the template to use in correlations. If percent=100
        then one correlation is computed with the entire area, otherwise
        five correlations are computed, beginning with the entire area. 
        The other four correlations utilize only:
        "percent" lines at the bottom,
        "percent" lines at the top,
        "percent" columns at the right,
        "percent" columns at the left.                               float
        The purpose of this is to permit the exclusion of border points
        which for some reason are incompatible in intensity with the
        data being correlated. 
        For example if percent=80 then one of the correlation attempts
        will only use nlw*0.80 lines either at the top or bottom of the
        window.

limits=The number of iterations permitted in the annealing scheme.    int
       This should be several thousand at the least.

quality=correlation quality. Zero to one.  1=perfect   returned   float

mode= is the type of correlation desired. An integer from 0 to 4.     int
      (all modes are sub-pixel).
      mode=0 linear least squares fit. 
             Fastest, on integral pixel borders. Translation only.
             Accuracy limited to 1/10 pixel.
      mode=1 annealing fit. Very slow. Able to search entire area.
             Accuracy adjustable but around 1/30 pixel.
             Handles rotation,skew,flip,scale,offset.
      mode=2 amoeba fit. deterministic simplex search method.
             Accuracy as good as it is possible to achieve(1/100 pixel).
             Initial estimate must be within 2 pixels or else the
             resulting tiepoint will be in error.
             Speed intermediate between modes 0 and 1.
             Handles rotation,skew,flip,scale,offset.
      mode=3 linear (mode=0) followed by amoeba (mode=2) fit
             Linear fit locates minimum and amoeba fit refines it.
             Handles rotation,skew,flip,scale,offset.
      mode=4 annealing (mode=1) followed by amoeba (mode=2) fit.
             Annealing locates minimum and amoeba refines it.
             Handles rotation,skew,flip,scale,offset.

ind=return status, 0=OK, 1=unable to obtain a correlation.            int
*/

gruen(left,nlw,nsw,right,nlw2,nsw2,correl,line_offset,samp_offset,
       line_coef,samp_coef,line_coef_limits,samp_coef_limits,
       line_temp,samp_temp,percent,limits,quality,mode,ind)

int nlw,nsw,nlw2,nsw2,*ind,mode,limits;
float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float *line_offset,*samp_offset,*quality,percent;
float line_coef[3],samp_coef[3];
float line_coef_limits[3][2],samp_coef_limits[3][2];
float line_temp[3],samp_temp[3];
{

 float range[6],answer[6],coef_limits[6][2];
 float P[6][7],Y[7],scr[6];
 double sumx[10],left_sum[10],sumx2,left_dn,rn;
 int numten,norm,i,j,ITER,kl,ks,num_areas,k,kll,kss;

 *ind=0;

/* compute sub area limits if percent < 100 */
 num_areas=1;
 if(percent < 99.99){
   num_areas=5;
   kl=(nlw*(percent/100.)) + .5;
   if(kl > nlw) kl=nlw;
   if(kl < 1) kl=1;
   ks=(nsw*(percent/100.)) + .5;
   if(ks > nlw) ks=nlw;
   if(ks < 1) ks=1;
   kll=nlw-kl-1;
   kss=nsw-ks-1;
 }

/* loop on all sub areas */
 for(k=0; k < num_areas; k++){

/* compute area of sub area */
   if(k == 0) rn=nlw*nsw;
   if(k == 1) rn=kl*nsw;
   if(k == 2) rn=kl*nsw;
   if(k == 3) rn=ks*nlw;
   if(k == 4) rn=ks*nlw;

/* pre-compute left (template) sums for COST function */
   sumx2=0.0;
   sumx[k]=0.0;
   for(j=0; j < nlw; j++){
     for(i=0; i < nsw; i++){
        if(k == 0) goto compute;
        if(k == 1){
          if(j > kll) goto compute;
          continue;
        }
        if(k == 2){
          if(j < kl) goto compute;
          continue;
        }
        if(k == 3){
          if(i > kss) goto compute;
          continue;
        }
        if(k == 4){
          if(i < ks) goto compute;
          continue;
        }
        compute:
        left_dn=left[j][i];
        sumx[k] += left_dn;
        sumx2 += left_dn*left_dn;
     }
   }
   left_sum[k]=sumx2-sumx[k]*sumx[k]/rn;
 }

 if((mode == 0) || (mode == 3)){

   /* Use conventional linear correlation scheme only */

   search_area(left,nlw,nsw,right,nlw2,nsw2,correl,sumx,left_sum,
               percent,answer,quality,ind);
   if(mode == 3) goto Amoeba;
 }


 if((mode == 1) || (mode == 4)){

   /* use Annealing correlation */

   answer[0]=line_coef[0];
   answer[1]=line_coef[1];
   answer[2]=line_coef[2];
   answer[3]=samp_coef[0];
   answer[4]=samp_coef[1];
   answer[5]=samp_coef[2];
   range[0]=line_temp[0];
   range[1]=line_temp[1];
   range[2]=line_temp[2];
   range[3]=samp_temp[0];
   range[4]=samp_temp[1];
   range[5]=samp_temp[2];
   coef_limits[0][0]=line_coef_limits[0][0];
   coef_limits[0][1]=line_coef_limits[0][1];
   coef_limits[1][0]=line_coef_limits[1][0];
   coef_limits[1][1]=line_coef_limits[1][1];
   coef_limits[2][0]=line_coef_limits[2][0];
   coef_limits[2][1]=line_coef_limits[2][1];
   coef_limits[3][0]=samp_coef_limits[0][0];
   coef_limits[3][1]=samp_coef_limits[0][1];
   coef_limits[4][0]=samp_coef_limits[1][0];
   coef_limits[4][1]=samp_coef_limits[1][1];
   coef_limits[5][0]=samp_coef_limits[2][0];
   coef_limits[5][1]=samp_coef_limits[2][1];

   /* set iteration control */
   numten=limits/2;
   norm=numten/7;

   metropolis(left,nlw,nsw,right,nlw2,nsw2,answer,
        coef_limits,range,numten,limits,norm,sumx,left_sum,
        percent,quality,ind);
   if(mode == 4) goto Amoeba;
 }

 if(mode == 2){

   /* use Amoeba correlation */

   answer[0]=line_coef[0];
   answer[1]=line_coef[1];
   answer[2]=line_coef[2];
   answer[3]=samp_coef[0];
   answer[4]=samp_coef[1];
   answer[5]=samp_coef[2];
   Amoeba:
   coef_limits[0][0]= -1000.;
   coef_limits[0][1]=  1000.;
   coef_limits[1][0]= -1000.;
   coef_limits[1][1]=  1000.;
   coef_limits[2][0]= -1000.;
   coef_limits[2][1]=  1000.;
   coef_limits[3][0]= -1000.;
   coef_limits[3][1]=  1000.;
   coef_limits[4][0]= -1000.;
   coef_limits[4][1]=  1000.;
   coef_limits[5][0]= -1000.;
   coef_limits[5][1]=  1000.;

   /* precompute the matrix P of 7 simplex starting points 
      and array Y of corresponding errors */

   for(j=0; j < 7; j++){
     for(i=0; i < 6; i++){
       P[i][j]=answer[i];
     }
     if(j == 0) P[j][j]=P[j][j]-.05;
     if(j == 1) P[j][j]=P[j][j]-.05;
     if(j == 2) P[j][j]=P[j][j]-0.5;
     if(j == 3) P[j][j]=P[j][j]-.05;
     if(j == 4) P[j][j]=P[j][j]-.05;
     if(j == 5) P[j][j]=P[j][j]-0.5;
     for(i=0; i < 6; i++){
       scr[i]=P[i][j];
     }
     cost(scr,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
           sumx,left_sum,percent,quality,ind);
     Y[j]= *quality+1.;
   }

   /* solve for the minimum using the simplex method */

   amoeba(P,Y,&ITER,nlw,nsw,nlw2,nsw2,left,right,
       coef_limits,sumx,left_sum,percent);

   *quality=1.0-(Y[0]-1.0);
   for(i=0; i < 6; i++){
     answer[i]=P[i][0];
   }
 }

 if((mode < 0) || (mode > 4)){
   *ind=1;
   return;
 }


/* restore & return new polynomial coefficients */
 line_coef[0]=answer[0];
 line_coef[1]=answer[1];
 line_coef[2]=answer[2];
 samp_coef[0]=answer[3];
 samp_coef[1]=answer[4];
 samp_coef[2]=answer[5];

/* return offset from initial estimate, Note other polynomial terms are
   zero because multiply by x=y=0 at template origin */
 *line_offset=line_coef[2];
 *samp_offset=samp_coef[2];
 
}

/**********************************************************************/
zrangen(idum,rand_num)
  long *idum;        /* input seed (1st call only)and  returned integer*/
  float *rand_num;       /* returned random number  0> *rand_num>1 */
{
  static long iy,ir[98];
  static int iff=0;
  long M,IA,IC;
  int j;
  char msg[80];

  M=714025; IA=1366; IC=150889;
  if(*idum<0 || iff==0)
  {
    iff=1;
    if ((*idum=(IC - (*idum)) % M) < 0) *idum= -(*idum);
    for (j=1;j<97;j++)
    {
      *idum=(IA*(*idum)+IC) % M;
      ir[j]=(*idum);
    }
    *idum=(IA*(*idum)+IC) % M;
    iy=(*idum);
  }
  j=1 + 97.0*iy/M;
  if (j>97||j<1) 
  {
     sprintf(msg,"RANGEN: This cannot happen.");
     zvmessage(msg,"");
  }
  iy=ir[j];
  *idum=(IA*(*idum)+IC) % M;
  iy=(*idum);
  *rand_num= (float) iy/M;
  return; 
}

/*********************************************************************/
metropolis(left,nlw,nsw,right,nlw2,nsw2,answer,
        coef_limits,range,numten,limits,norm,sumx,left_sum,
        percent,quality,indic)

      float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
      float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
      float *quality,range[6],answer[6],coef_limits[6][2],percent;
      double sumx[10],left_sum[10];
      int nlw,nsw,nlw2,nsw2,numten,limits,norm,*indic;
{
      int fail1,fail2,success1,success2,limit,numreset,j,k,loop,narg,ind;
      unsigned long iseed;
      float temp[6],x[6],minx[6],mincost,pi,pi2,c1,c2,c3,scale,numtenf;
      float costsum,energy,boltzman,rand_max,ran,prob;

      pi=3.14159;
      pi2=pi/2.0;
      limit=limits;
      iseed=10109854;
      narg=6;
      numtenf=numten;
      fail1=0;
      fail2=0;
      success1=0;
      success2=0;
      numreset=numten/10;
      scale=exp((log(0.1))/numtenf);
      loop=0;
      rand_max=pow(2.0,31.0)-1.0;

/*  Compute the cost at position ANSWER and assign to variable C1. */
      cost(answer,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
           sumx,left_sum,percent,&c1,&ind);
      if(ind != 0){
         *indic=1;
         return;
      }

/*  Save the cost in case the user had good reason to inspect this
    solution position. */
      mincost=c1;
      for(j=0; j < narg; j++){
         minx[j]=answer[j];
      }

/*  Set initial temperatures to the range estimates. */
      for(j=0; j < narg; j++){
         temp[j]=range[j];
      }

/*   MAIN LOOP: loop on number of successful changes in solution space. */
      while(loop < limit){

/*   Compute the delta_cost/temperature ratio for
     normalization of probabilities.
     Note that this is the Boltzmann constant for this 'system'.*/

        k=loop/norm;
        if(loop-k*norm == 0){
           costsum=0.0;
           k=0;
           for(j=0; j < narg; j++){
              x[j]=answer[j];
           }
           for(j=0; j < narg; j++){
              x[j]=answer[j]-temp[j];
              cost(x,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
                   sumx,left_sum,percent,&c2,&ind);
              if(ind == 0){
                k=k+1;
                costsum=costsum+fabs(c1-c2)/temp[j];
              }
              x[j]=answer[j]+temp[j];
              cost(x,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
                   sumx,left_sum,percent,&c2,&ind);
              if(ind == 0){
                k=k+1;
                costsum=costsum+fabs(c1-c2)/temp[j];
              }
              x[j]=answer[j];
           }
           if(k == 0){
              *indic=2;
              return;
           }         
           boltzman=5.0*(costsum/k);
        }
                     
/*      Decrement the temperature according to the multiplicative
        cooling schedule.  */
        for(j=0; j < narg; j++){
           temp[j]=temp[j]*scale;
        }
        energy=boltzman*(temp[0]+temp[1]+temp[2]+temp[3]+temp[4]
                        +temp[5])/6.0;

/*      Compute a solution space guess using a Cauchy-Lorentzian
        random probability distribution function. */
A91:
        for(j=0; j < narg; j++){
           zrangen(&iseed,&ran);
           x[j]=temp[j]*tan(pi*ran+pi2)+answer[j];
        }
        cost(x,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
             sumx,left_sum,percent,&c2,&ind);
        if(ind != 0){
           fail1 += 1;
           goto A91;
        }

        if(c2 < c1){

/*          Accept lower cost position.
            We always accept a downhill cost route if offered.*/

            success1 += 1;
            c1=c2;
            for(j=0; j < narg; j++){
               answer[j]=x[j];
            }
        }
        else{
/*          Compute probability of accepting higher cost position.
            This comes from the Boltzmann probability of our system 
            transitioning from energy state c1 to energy state c2.*/

            c3=(c2-c1)/energy;
            if(c3 > 50.){
               goto A92;
            }
            prob=1.0/exp((double)c3);

/*          Evaluate the probability by comparing it against chance.*/

            zrangen(&iseed,&ran);
            if(prob > ran){
/*              Accept higher cost position.*/
                success2 += 1;
                c1=c2;
                for(j=0; j < narg; j++){
                   answer[j]=x[j];
                }
            }
            else{
/*              Reject higher cost position.*/
                fail2 += 1;
                goto A92;
            }
        }

/*       Save the minimum cost and associated solution as we go.*/

        if(c1 < mincost){
            mincost=c1;
            for(j=0; j < narg; j++){
               minx[j]=answer[j];
            }
        }
A92:
        loop=loop+1;

/*       Reset the solution pointer to the minimum cost
         location every numreset successful iterations. */

        k=loop/numreset;
        if(loop-k*numreset == 0){
            c1=mincost;
            for(j=0; j < narg; j++){
               answer[j]=minx[j];
            }
        }

      }  /*   END of MAIN WHILE LOOP  */

/*     Put minimum solution into ANSWER & it's cost into quality  */

      for(j=0; j < narg; j++){
         answer[j]=minx[j];
      }
      *quality=1.0-mincost;

 *indic=0;  
 /*printf("Initial ok=%d bad=%d, Probability ok=%d bad=%d\n",success1,
         fail1,success2,fail2);*/
}


/********************************************************************/
read_left_area(left,correl,left_line,left_samp,unit1,nl,ns,nlw,nsw,zero,ind)

float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float left_line,left_samp;
int unit1,nl,ns,nlw,nsw,zero,*ind;
{
 int status,start_samp,start_line,j,i,m,n;
 float wt,wb,wl,wr,top,bot,dn;

 *ind=0;

 n=left_line;
 m=left_samp;
 if(left_line-n + left_samp-m < .0001){     /* on pixel boundary */
   if(left_line-n > 0.5) n=n+1;
   if(left_samp-m > 0.5) m=m+1;
   start_samp=m - nsw/2;
   start_line=n - nlw/2;
   if(start_samp < 1) *ind=1;
   if(start_line < 1) *ind=1;
   if(start_samp+nsw-1 > ns) *ind=1;
   if(start_line+nlw-1 > nl) *ind=1;
   if(*ind == 1) return;
   for(j=0; j<nlw; j++){
     status=zvread(unit1,&left[j][0],"LINE",j+start_line,"SAMP",start_samp,
                   "NSAMPS",nsw, NULL);
   }
 }
 else{                             /* interpolate grid to center on a pixel */
   start_samp=m - nsw/2;
   start_line=n - nlw/2;
   if(start_samp < 1) *ind=1;
   if(start_line < 1) *ind=1;
   if(start_samp+nsw > ns) *ind=1;
   if(start_line+nlw > nl) *ind=1;
   if(*ind == 1) return;
   for(j=0; j< nlw+1; j++){
     status=zvread(unit1,&correl[j][0],"LINE",j+start_line,"SAMP",start_samp,
                   "NSAMPS",nsw+1, NULL);
   }
   wr=left_samp-m;
   wl=1.0-wr;
   wb=left_line-n;
   wt=1.0-wb;
   for(j=0; j < nlw; j++){
     for(i=0; i < nsw; i++){
       top=wl*correl[j][i]+wr*correl[j][i+1];
       bot=wl*correl[j+1][i]+wr*correl[j+1][i+1];
       left[j][i]=top*wt+bot*wb;
     }
   }
 }
 
 if(zero == 1){
   if(left[0][0] == 0.0) *ind=1;
   if(left[nlw-1][0] == 0.0) *ind=1;
   if(left[0][nsw-1] == 0.0) *ind=1;
   if(left[nlw-1][nsw-1] == 0.0) *ind=1;
 }
 if(*ind == 1) return;

 dn=left[0][0];
 for(j=0; j < nlw; j++){
   for(i=0; i < nsw; i++){
     if(left[j][i] != dn) return;
   }
 }
 *ind=1; /* bad */

}

/********************************************************************/
read_right_area(right,correl,left_line,left_samp,unit2,nl,ns,nlw2,nsw2,zero,ind)

float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float left_line,left_samp;
int unit2,nl,ns,nlw2,nsw2,zero,*ind;
{
 int status,start_samp,start_line,j,i,m,n;
 float wt,wb,wl,wr,top,bot,dn;

 *ind=0;

 n=left_line;
 m=left_samp;
 if(left_line-n + left_samp-m < .0001){     /* on pixel boundary */
   if(left_line-n > 0.5) n=n+1;
   if(left_samp-m > 0.5) m=m+1;
   start_samp=m - nsw2/2;
   start_line=n - nlw2/2;
   if(start_samp < 1) *ind=1;
   if(start_line < 1) *ind=1;
   if(start_samp+nsw2-1 > ns) *ind=1;
   if(start_line+nlw2-1 > nl) *ind=1;
   if(*ind == 1) return;

   for(j=0; j<nlw2; j++){
     status=zvread(unit2,&right[j][0],"LINE",j+start_line,"SAMP",start_samp,
                   "NSAMPS",nsw2, NULL);
   }
 }
 else{                             /* interpolate grid to center on a pixel */
   start_samp=m - nsw2/2;
   start_line=n - nlw2/2;
   if(start_samp < 1) *ind=1;
   if(start_line < 1) *ind=1;
   if(start_samp+nsw2 > ns) *ind=1;
   if(start_line+nlw2 > nl) *ind=1;
   if(*ind == 1) return;
   for(j=0; j< nlw2+1; j++){
     status=zvread(unit2,&correl[j][0],"LINE",j+start_line,"SAMP",start_samp,
                   "NSAMPS",nsw2+1, NULL);
   }
   wr=left_samp-m;
   wl=1.0-wr;
   wb=left_line-n;
   wt=1.0-wb;
   for(j=0; j < nlw2; j++){
     for(i=0; i < nsw2; i++){
       top=wl*correl[j][i]+wr*correl[j][i+1];
       bot=wl*correl[j+1][i]+wr*correl[j+1][i+1];
       right[j][i]=top*wt+bot*wb;
     }
   }
 }

 if(zero == 1){
   if(right[0][0] == 0.0) *ind=1;
   if(right[nlw2-1][0] == 0.0) *ind=1;
   if(right[0][nsw2-1] == 0.0) *ind=1;
   if(right[nlw2-1][nsw2-1] == 0.0) *ind=1;
 }
 if(*ind == 1) return;

 dn=right[0][0];
 for(j=0; j < nlw2; j++){
   for(i=0; i < nsw2; i++){
     if(right[j][i] != dn) return;
   }
 }
 *ind=1; /* bad */

}


