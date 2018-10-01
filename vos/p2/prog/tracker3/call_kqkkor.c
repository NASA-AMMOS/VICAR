/* Subroutine to call kqkkor routine from the tracker3 program */
/* By Anton B. Ivanov (abi@mipl7.jpl.nasa.gov)                 */
/* April 17 2000 (the tax day!)                                */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "kqkkor.h"

#define MAX_LEFT_AREA 50 
#define MAX_RIGHT_AREA 150 

/* Utility patch for conversion of float matrices to int)       */
void flt2int( int* , int , int, float* , int , int);
void print_matrix( int* , int , int );
/* Testing string for KQKKOR

./tracker3 -print "inp=(left.img,right.img) out=pts.img grid=30 nlw=19 nsw=19 nlarea=33 nsarea=33 correl=KQKKOR"

*/

/* This subroutine calls dim_muster_such and finds optimal dimensions for  */
/* the data required by the KQKKOR routine. Returns kq_nlw,kq_nsw and      */
/* kq_nlw2,kq_nsw2 which are reference patch and search window sizes. The  */
/* results are used in read_left_area and read_right_area routines of      */
/* TRACKER3. Note that this program accepts only square dimensions for     */
/* the template and search windows. The sub also returns correlation info  */
/* structure to be used in call_kqkor                                      */

void kqkkor_dimensions( nlw, nlw2, line_coef, samp_coef, quality_limit,
			kq_nlw, kq_nsw, kq_nlw2, kq_nsw2, info)
     int nlw, nlw2;
     int *kq_nlw, *kq_nsw, *kq_nlw2, *kq_nsw2;
     float line_coef[3], samp_coef[3];
     float quality_limit;
     KORPAR *info;     /* parameters for the correlation */
{
  /* Declare KQKKOR inputs */
  INT_MATRIX        musterma; /* pattern/reference matrix       */
  INT_MATRIX        suchma;   /* search matrix                  */
  float deg;
  /* Map the TRACKER3 inputs to KQKOR inputs */

  /* Set values for determination of KQKKOR parameters */

   info -> suchfns = nlw;     /* DEF_SUCHB;   Search window = 10       */ 

   info -> pmkdim  = nlw - 1; /* DEF_PMKDIM;  Patch size for PMK = 9   */ 
   info -> kqkdim  = nlw;    /* Patch size for LSM = 17 */ 
   info -> lsmacu  = 5.0   ; /* Accuracy of the least-squares fit = 0.08  */ 

   /* Calculate search window for KQKKOR, from the nlw and nlw2 of tracker3 */
   /* Search window and search size are different in KQKKOR                 */
   info -> suchfns = nlw2 - 1 - 2 * ( KQK_RAND + info -> kqkdim / 2.0 + 1);
   
   /* info -> suchfns = DEF_SUCHB;    /* Search window = 10      */
   /* info -> pmkdim  = DEF_PMKDIM;   /* Patch size for PMK = 9  */
   /* info -> kqkdim  = 17;           /* Patch size for LSM = 17 */ 
   /* info -> lsmacu  = 0.08;         /* Accuracy of the least-squares fit = 0.08  */

  /* Quality of the correlation input from the tracker3          */
   info -> minpmk  = quality_limit;

  /* Initial coefficients from tracker3                          */
  /* Check if the order is correct                               */
     info -> affin[0] = (double)  line_coef[1];
     info -> affin[1] = (double)  line_coef[0];
     info -> affin[2] = (double)  line_coef[2];
     info -> affin[3] = (double)  samp_coef[1];
     info -> affin[4] = (double)  samp_coef[0];
     info -> affin[5] = (double)  samp_coef[2];
 
/*   info -> affin[0] =   0.903521; */
/*   info -> affin[1] =   0.098851;  */
/*   info -> affin[2] =  -0.127089;  */
/*   info -> affin[3] =  -0.103596; */
/*   info -> affin[4] =   0.904245;  */
/*   info -> affin[5] =   8.392435; */


/*   fprintf( stderr, "DIM : %f %f %f\n", info -> affin[0],  info -> affin[1],info -> affin[2]); */
  
  /* Initialize matrices with minimum dimensions */
  dim_muster_such( info, &musterma, &suchma );
  
/*   printf ("\n minum dimensioning of the reference patch:  (nlw)  dimz:%6d dims:%6d\n",  */
/*   	  musterma.dimz, musterma.dims); */

/*   printf (" minum dimensioning of the search image patch: (nlw2) dimz:%6d dims:%6d\n\n",  */
/*    	  suchma.dimz, suchma.dims); */

  /* Get the minimum required dimensions from the matrices */
  *kq_nlw = musterma.dimz;
  *kq_nsw = musterma.dims;
  *kq_nlw2 = suchma.dimz;
  *kq_nsw2 = suchma.dims;

}

/* ARGUMENTS for call_kqkkor(partly copied from TRACKER3
left=left image area template                                    float*

nlw= # lines left area. odd number                               int

nsw= # samples left area. odd number                             int

right=right image search area                                    float*

nlw2=# lines right area. odd number                              int

nsw2=# samples right area. odd number                            int

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

line_offset=line shift to correct initial registration, returned   float
            Negative means shift upwards.

samp_offset=sample shift to correct initial registration, returned float
            Negative means shift to the left.

quality = correlation quality. Zero to one.  1=perfect   returned   float

ind = return status, 0=OK, 1=unable to obtain a correlation.            int

*****************************************************************************/
void call_kqkkor( info,
		  left, nlw, nsw, right, nlw2, nsw2,
		  line_coef, samp_coef,
		  line_offset, samp_offset, quality, ind)

KORPAR *info;
float *left;
int nlw, nsw;
float *right;
int nlw2, nsw2;
float line_coef[3],samp_coef[3];
float *line_offset,*samp_offset,*quality;
int *ind;
{
  /* Declare KQKKOR inputs */
  INT_MATRIX        musterma; /* pattern/reference matrix       */
  INT_MATRIX        suchma;   /* search matrix                  */
  ERG               erg;      /* results                        */

  char msg[100];
  /* Map the TRACKER3 inputs to KQKOR inputs */
  /* We have already prepared the info structure in the kqkkor_dimensions routine */

  /* Initialize data in the matrices             */

  /* KQKKOR only accepts integer values for inputs. Patch this problem for testing */
  /* Float matrix must be implemented in KQKKOR for full compatibility with TRACKER3 */

  musterma.dimz   = nlw;
  musterma.dims   = nsw;
  musterma.ptr_m = (int *) malloc( sizeof( int ) * nlw * nsw);
  if ( nlw > MAX_LEFT_AREA) fprintf( stderr, "LSKGJSLKJLSKDJFLF\n"); 
  flt2int( musterma.ptr_m, nlw, nsw, left, MAX_LEFT_AREA, MAX_LEFT_AREA);
  /*print_matrix( musterma.ptr_m, nlw, nsw );*/
  suchma.dimz     = nlw2;
  suchma.dims     = nsw2;

  suchma.ptr_m =  (int *) malloc( sizeof( int ) * nlw2 * nsw2);
  flt2int( suchma.ptr_m,  nlw2, nsw2, right, MAX_RIGHT_AREA , MAX_RIGHT_AREA);

/*   fprintf (stderr, "\n actual dimension of the reference patch:    dimz:%6d dims:%6d\n",  */
/*            musterma.dimz, musterma.dims);  */

/*   fprintf (stderr, " actual dimension of the search image patch: dimz:%6d dims:%6d\n\n",  */
/*            suchma.dimz, suchma.dims); */

  /* Perform the correlation                     */
  kqkkor( info, &musterma, &suchma, &erg );

  /* Map the output to the TRACKER3 parameters   */
  *line_offset = erg.dz;
  *samp_offset = erg.ds;
  /* Correlation coefficient (PMK) after the littlest squares adaptation */
  /* Calculate the square of the result, because KQKKOR returns rho and tracker3 uses rho^2 */
  if ( erg.kqk > 0.0 ) 
    *quality =  erg.kqk * erg.kqk; 
  else
    *quality =  erg.kqk; /* -1 */

/*   sprintf( msg, "This is stderr %f %f\n",  erg.kqk, erg.kqk * erg.kqk ); */
/*   zvmessage( msg, " ");  */

/*   if ( *quality > 0.0 ) { */
/*     fprintf( stderr, "KQKKOR : %f %f %f\n", erg.pmk, erg.kqk, erg.mp ); */
/*   } */
 
  *ind = 0;
  /* return output coefficents as well           */
  
  line_coef[1] = info -> affin[0];
  line_coef[0] = info -> affin[1];
  line_coef[2] = info -> affin[2];
  samp_coef[1] = info -> affin[3];
  samp_coef[0] = info -> affin[4];
  samp_coef[2] = info -> affin[5];
  /*fprintf( stderr, "%f <<<<- %f\n", samp_coef[0], info -> affin[3]);
  */
  /*     fprintf( stderr, "DIM NEW: %f %f %f %f %f %f\n", info -> affin[0],  info -> affin[1],info -> affin[2], 
	 info -> affin[3],  info -> affin[4],info -> affin[5]); */

  free( musterma.ptr_m);
  free( suchma.ptr_m);

}

/*************************************************************************** 
   call_affinpar - interface to affinpar routine from the
   kqkkor/affinpar suite. This function returns 1 if the affine
   transformation was successfully found, otherwise returns status of
   the affinepar subroutine (< 1 ). The procedure works with fixed
   lengths arrays, which are defined in the tracker3. 

   INPUTS : 
   line1, samp1 - Line and sample for the point of interest in the
                  patch (left) image
   obuf         - float[4][n] array, containing previously acquired tiepoints
   ptcount      - number of points in obuf

   OUTPUT : 
   line2, samp2         - best guess for Line and sample in the search
                          image. These are not used yet. 
   line_coef, samp_coef - coefficients for the affine transform,
                          computed based on the previously acquired points. 
****************************************************************************/

int call_affinpar(  line1, samp1, obuf, ptcount,
		     line2, samp2, line_coef, samp_coef)
/* INPUT */
     float line1, samp1;
     float obuf[32*4]; 
     int   ptcount; 	    
/* OUTPUT */     
     float *line2, *samp2; /* line2 and samp2 are not used yet */ 
     float *line_coef, *samp_coef;
{
  int i;
  int status;

  /* Declare affinpar inputs.  */
  double lne_in; /* Coordinates of the input point to project */
  double smp_in; 
  
  double  lne_0[32]; /* Tie points */
  double  smp_0[32]; /* *_0 - patch image  */
  double  lne_1[32]; /* *_1 - search image */
  double  smp_1[32];

  double lne_out; /* Coordinates of the input point in the search*/
  double smp_out; /*   (right) image */
  
  double affcoef[6]; /* Affine transformation coefficients */

  /* Map tracker3 inputs to affinpar inputs. */
  lne_in = (double) line1; 
  smp_in = (double) samp1; 
  for( i = 0; i < ptcount; i++ ) {
    lne_0[i] = (double) obuf[i * 4 + 0];
    smp_0[i] = (double) obuf[i * 4 + 1];
    lne_1[i] = (double) obuf[i * 4 + 2];
    smp_1[i] = (double) obuf[i * 4 + 3];
/*     fprintf( stderr, "Input points : %f %f %f %f\n",  lne_0[i],  smp_0[i],  lne_1[i],  smp_1[i]); */
  }
  
  /* Call affinpar and determine the affine transformation coefs. */
  status = affinpar (lne_in, smp_in, 
		     ptcount,  lne_0,  smp_0,  lne_1,  smp_1, 
		     &lne_out, &smp_out, affcoef);
  if ( status != 1) {
    /* If something's wrong with finding the affine transformation */
    /* Let the calling program decide what to do                   */
    return status; 
  }
  
  /* Create output */
  /* Best guess for line and sample - NOT USED */
  *line2 = lne_out;                  
  *samp2 = smp_out;                  

  /* Affine transformation coeffcient */
  line_coef[1] = affcoef[0];
  line_coef[0] = affcoef[1]; 
  line_coef[2] = affcoef[2]; 
  samp_coef[1] = affcoef[3]; 
  samp_coef[0] = affcoef[4]; 
  samp_coef[2] = affcoef[5]; 

  return 1; 
     
}
void flt2int( out,  sx, sy, data, ix, iy)
     int* out;
     float* data;
     int sx, sy, ix, iy;
{
  int i, j;
  char msg[180];

  if ( out == NULL ) {
    sprintf( msg, "Not enough memory to allocate for the patches in KQKKOR %d %d\n", sx, sy);
    zvmessage( msg, " ");
    zabend();
  }

 /*  fprintf( stderr, "%d %d \n", sx, sy);  */
  for ( j = 0; j < sy; j++ ) {
    for ( i = 0; i < sx; i++ ) {
      out[j * sx + i] = (int) floor(data[j * ix + i] + 0.5);
 /*     fprintf( stderr, "%3d ", out[j * sx + i] ); */
    } 
  /*   fprintf( stderr, "\n" ); */
  }
 
}

void print_matrix( int* data, int sx, int sy)
{
  int i, j;

  for ( j = 0; j < sy; j++ ) {
    for( i = 0; i < sx; i++)
      fprintf( stderr, "%3d ", data[j * sx + i]);
    fprintf( stderr, "\n");
  }
  fprintf( stderr, "-----------------------------------------------------------\n");
  
}

