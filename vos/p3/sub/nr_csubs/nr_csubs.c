#include <math.h>
#include <stdlib.h>
#include <stdio.h>


#define IGREG (15 +31L*(10+12L+1582)) /* gregorian cal. adopted oct 15, 1582 */

/*---------------------------------------------------------------------------*/
long nr_julday(mm,id,iyyy)
  int mm,id,iyyy;
{
/* julday returns the Julian Day Number which begins at noon of the calender */
/* date specified by month mm, day id, abd year iyyy, all integers. Positive */
/* year signifies A.D.; negative, B.C. The year after 1 B.C. was 1 A.D.      */

   long jul;
   int ja,jy,jm;
   void nr_error();

   if (iyyy == 0) nr_error("julday: there is no year zero");
   if (iyyy < 0) ++iyyy;
   if (mm > 2) {
      jy = iyyy;
      jm = mm + 1;
   }
   else {
      jy = iyyy - 1;
      jm = mm + 13;
   }
   jul = (long) (floor(365.25*jy) + floor(30.6001*jm) + id + 1720995);
   if (id+31L*(mm+12L*iyyy) >= IGREG) {
      ja = 0.01 * jy;
      jul += 2 - ja + (int) (0.25*ja);
   }
   return jul;
}

/*---------------------------------------------------------------------------*/
int nr_get_doy (date)
  int date;
{
   int  day, month, year;

   year  = (int)(date/10000);
   month = (int)((date-year*10000)/100);
   day   = date - 10000*year - 100*month;

   if (month > 1)  day += 31;
   if (month > 2)  day += 28;
   if (month > 3)  day += 31;
   if (month > 4)  day += 30;
   if (month > 5)  day += 31;
   if (month > 6)  day += 30;
   if (month > 7)  day += 31;
   if (month > 8)  day += 31;
   if (month > 9)  day += 30;
   if (month > 10) day += 31;
   if (month > 11) day += 30;

   if ((((int)(year/4))*4 == year) && month > 2)
      day += 1;

   return day;
}

/*---------------------------------------------------------------------------*/
void nr_error(error_text)
  char error_text[];
{
   void exit();

   fprintf(stderr,"\nNR error - %s\n",error_text);
   exit(0);
}

/*---------------------------------------------------------------------------*/
float *nr_vector (nl,nh)
  int nl,nh;
{
   float *v;

   v=(float *)malloc((unsigned) (nh-nl+1)*sizeof(float));
   if (!v) nr_error("allocation failure in nr_vector()");
   return v-nl;
}

/*---------------------------------------------------------------------------*/
int *nr_ivector (nl,nh)
  int nl,nh;
{
   int *v;

   v=(int *)malloc((unsigned) (nh-nl+1)*sizeof(int));
   if (!v) nr_error("allocation failure in nr_ivector()");
   return v-nl;
}

/*---------------------------------------------------------------------------*/
short int *nr_svector (nl,nh)
  int nl,nh;
{
   short int *v;

   v=(short int *)malloc((unsigned) (nh-nl+1)*sizeof(short int));
   if (!v) nr_error("allocation failure in nr_svector()");
   return v-nl;
}

/*---------------------------------------------------------------------------*/
double *nr_dvector (nl,nh)
  int nl,nh;
{
   double *v;

   v=(double *)malloc((unsigned) (nh-nl+1)*sizeof(double));
   if (!v) nr_error("allocation failure in nr_dvector()");
   return v-nl;
}

/*---------------------------------------------------------------------------*/
void nr_free_vector(v,nl,nh)
  float *v;
  int nl, nh;
{
   free((char*) (v+nl));
}

/*---------------------------------------------------------------------------*/
void nr_free_ivector(v,nl,nh)
  int *v;
  int nl, nh;
{
   free((char*) (v+nl));
}

/*---------------------------------------------------------------------------*/
void nr_free_dvector(v,nl,nh)
  double *v;
  int nl, nh;
{
   free((char*) (v+nl));
}


/*---------------------------------------------------------------------------*/
float **nr_matrix(nrl,nrh,ncl,nch)
  int nrl,nrh,ncl,nch;
{
   int i;
   float **m;

   m = (float **) malloc((unsigned) (nrh-nrl+1)*sizeof(float*));
   if (!m) nr_error("allocation failure 1 in nr_matrix()");
   m -= nrl;

   for (i=nrl;i<=nrh;i++) {
      m[i]=(float *) malloc((unsigned) (nch-ncl+1)*sizeof(float));
      if (!m[i]) nr_error("allocation failure 2 in nr_matrix()");
      m[i] -= ncl;
   }
   return m;
}

/*---------------------------------------------------------------------------*/
double **nr_dmatrix(nrl,nrh,ncl,nch)
  int nrl,nrh,ncl,nch;
{
   int i;
   double **m;

   m = (double **) malloc((unsigned) (nrh-nrl+1)*sizeof(double*));
   if (!m) nr_error("allocation failure 1 in nr_dmatrix()");
   m -= nrl;

   for (i=nrl;i<=nrh;i++) {
      m[i]=(double *) malloc((unsigned) (nch-ncl+1)*sizeof(double));
      if (!m[i]) nr_error("allocation failure 2 in nr_dmatrix()");
      m[i] -= ncl;
   }
   return m;
}

/*---------------------------------------------------------------------------*/
int **nr_imatrix(nrl,nrh,ncl,nch)
  int nrl,nrh,ncl,nch;
{
   int i;
   int **m;

   m = (int **) malloc((unsigned) (nrh-nrl+1)*sizeof(int*));
   if (!m) nr_error("allocation failure 1 in nr_imatrix()");
   m -= nrl;

   for (i=nrl;i<=nrh;i++) {
      m[i]=(int *) malloc((unsigned) (nch-ncl+1)*sizeof(int));
      if (!m[i]) nr_error("allocation failure 2 in nr_imatrix()");
      m[i] -= ncl;
   }
   return m;
}

/*---------------------------------------------------------------------------*/
void nr_free_matrix(m,nrl,nrh,ncl,nch)
  float **m;
  int nrl,nrh,ncl,nch;
{
   int i;

   for (i=nrh; i>nrl;i--)  free ((char*) (m[i]+ncl));
   free ((char*) (m+nrl));
}

/*---------------------------------------------------------------------------*/
void nr_free_dmatrix(m,nrl,nrh,ncl,nch)
  double **m;
  int nrl,nrh,ncl,nch;
{
   int i;

   for (i=nrh; i>nrl;i--)  free ((char*) (m[i]+ncl));
   free ((char*) (m+nrl));
}

/*---------------------------------------------------------------------------*/
void nr_free_imatrix(m,nrl,nrh,ncl,nch)
  int **m;
  int nrl,nrh,ncl,nch;
{
   int i;

   for (i=nrh; i>nrl;i--)  free ((char*) (m[i]+ncl));
   free ((char*) (m+nrl));
}


/*---------------------------------------------------------------------------*/
float **nr_convert_matrix(a,nrl,nrh,ncl,nch)
  float *a;
  int nrl,nrh,ncl,nch;
{
   int i, j, nrow, ncol;
   float **m;

   nrow = nrh - nrl + 1;
   ncol = nch - ncl + 1;

   m = (float **) malloc((unsigned) (nrow)*sizeof(float*));
   if (!m) nr_error ("allocation failure in nr_convert_matrix()");
   m -= nrl;
   for(i=0,j=nrl;i<=nrow-1;i++,j++) m[j]=a+ncol*i-ncl;
   return m;
}

/*---------------------------------------------------------------------------*/
void nr_free_convert_matrix(b,nrl,nrh,ncl,nch)
  float **b;
  int nrl, nrh, ncl, nch;
{
   free((char*) (b+nrl));
}


/*---------------------------------------------------------------------------*/
void nr_spline(x,y,n,yp1,ypn,y2)
  float x[], y[], yp1, ypn, y2[];
  int n;

/*   This subroutine was taken from Press, et al, "Numerical Recipes -
     The Art of Scientific Computing"

     Given arrays X and Y of length N containing a tabulated function,
     i.e. Yi = f(Xi), with X1<X2<. . .<XN, and given values YP1 and YPN
     for the first derivative of the interpolating function at points 1
     and N, respectively, this routine returns an array Y2 of length N
     which contains the second derivatives of the interpolating function
     at the tabulated points Xi.  if YP1 and/or YPN are equal to 10**30
     or larger, the routine is signalled to set the corresponding boun-
     dary condition for a natural spline, with zero second derivative on
     that boundary.
*/

{
   int i,k;
   float p, qn, sig, un, *u, *nr_vector();
   void nr_free_vector();

   /* lower boundary condition is "natural" or has specified 1st derivative */
   /* - since i've padded zeroes onto the front of the filter, always use   */
   /* the "natural"  boundary conditions                                    */
   u = nr_vector(1,n-1);
/*   if (yp1 > 0.99e30) */
      y2[1] = u[1] = 0.0;
/*   else {
      Y2[1] = -0.5;
      U[1] = (3.0/(x[2]-x[1]))*((y[2]-y[1])/(x[2]-x[1])-yp1);
   }
*/
    /* this is the decomposition loop of the tridiagonal algorithm.
      Y2 and U are for temporary storage of the decomposed factors.      */

   for (i = 2; i <=n-1;i++) {
      sig = (x[i]-x[i-1])/(x[i+1]-x[i-1]);
      p = sig*y2[i-1]+2.0;
      y2[i]=(sig-1.0)/p;
      u[i] = (y[i+1]-y[i])/(x[i+1]-x[i]) - (y[i]-y[i-1])/(x[i]-x[i-1]);
      u[i] = (6.0*u[i]/(x[i+1]-x[i-1])-sig*u[i-1])/p;
   }

   /* upper boundary condition is "natural" or has specified 1st derivative */
   /* - since i've padded zeroes onto the end of the filter, always use     */
   /* the "natural"  boundary conditions                                    */

/*   if (ypn > 0.99e30) */
      qn = un = 0.0;
/*   else {
      qn = 0.5;
      un = (3.0/(x[n]-x[n-1]))*(ypn-(y[n]-y[n-1])/(x[n]-x[n-1]));
   }*/
   y2[n] = (un-qn*u[n-1])/(qn*y2[n-1]+1.0);

   /* this is the back substitution loop of the tridiagonal algorithm.   */

   for (k=n-1;k>=1;k--)
      y2[k] = y2[k]*y2[k+1] + u[k];

   nr_free_vector(u,1,n-1);
}

/*---------------------------------------------------------------------------*/
/*  This subroutine was taken from Press, et al, "Numerical Recipes -
    The Art of Scientific Computing"

    Given the arrays XA and YA of length N, which tabulate a function
    (with the XAi's in order), and given the array Y2A, which is the
    output from SPLinE above, and given a value of X, this routine
    returns a cubic-spline interpolated value Y.
    We will find the right place in the table by means of bisection.
    This is optimal if sequential calls to this routine are at random
    values of X.  if sequential calls are in order, and closely spaced
    one would do better to store previous values of KLO and KHI and
    test if they remain appropriate on the next call.
*/

void nr_splint(xa,ya,y2a,n,x,y)
  float xa[], ya[], y2a[], x, *y;
  int n;
{
   int low, high, k;
   float h, b, a;
   void nr_error();

   low=1;
   high=n;
   while (high-low > 1) {
      k = (high+low) >> 1;
      if (xa[k] > x) high = k;
      else           low = k;
   }

   h = xa[high] - xa[low];
   if (h == 0.0) nr_error("Bad XA input to routine nr_splint");
   a = (xa[high] - x) / h;
   b = (x - xa[low]) / h;

   /* evaluate cubic spline polynomial */
   *y = a*ya[low] + b*ya[high] +
        ((a*a*a-a)*y2a[low] + (b*b*b-b)*y2a[high]) * (h*h)/6.0;

   /* if the x is outside the range of the input function, set it to zero */
   if (x > xa[high] || x < xa[low])
     *y = 0.0;

}




