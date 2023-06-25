/************************************************************************/
/* Amoeba algorithm for finding a minimum of the given multidimensional	*/
/* function.  Implements the "downhill simplex method" of Nelder and	*/
/* Mead.								*/
/* Taken by Jean Lorre from Numerical Recipes.				*/
/* Genericized by Bob Deen.						*/
/*									*/
/* For the Mars open source release, this has been replaced with calls  */
/* to pamoeba, a parallelized version of amoeba that dos not have       */
/* Numerical Recipes code and is distributable.  They should be         */
/* functionally equivalent.  Note that parallelism is explicitly        */
/* disabled in these wrappers.  Callers really should use pamoeba       */
/* instead, to take advantage of parallelism.                           */      
/*									*/
/* The rest of the comment block explains the params and is unchanged.  */
/* rgd 2023-04-11                                                       */
/*									*/
/*----------------------------------------------------------------------*/
/* P is an array of NDIM+1 rows, where each row is a potential solution.*/
/* The algorithm operates by moving the solutions around in		*/
/* multidimensional space using one of 4 defined transforms, "flowing"	*/
/* the highest values "downhill", in an amoeba-like manner.  See the	*/
/* book for more details.						*/
/*									*/
/* NDIM is the number of dimensions (independent variables).		*/
/*									*/
/* P must be initialized as follows:  the first row is an initial guess	*/
/* at the solution.  Each successive row is equal to that first row,	*/
/* with one of the variables perturbed by an amount "which is your	*/
/* guess of the problem's characteristic length scale", so each variable*/
/* is perterbed in one and only one row.  Upon completion, any row will	*/
/* hold a valid solution, although P[0] is normally used.  See amoeba2	*/
/* or amoeba3 for wrapper functions which do this initialization for	*/
/* you.									*/
/*									*/
/* Physically, P must be an array[][WIDTH], where WIDTH is the physical	*/
/* size of the second dimension.  Only NDIM elements are used from the	*/
/* second dimension.  The first dimension must be big enough to hold	*/
/* NDIM+1 rows.  Note that P must be a true two-dimensional array, not	*/
/* an array of pointers to arrays.  The code internally is complicated	*/
/* somewhat in order to support a variable WIDTH dimension.		*/
/*									*/
/* Y is an array of NDIM+1 values, which hold the results of the	*/
/* function evaluation for each trial in P.  It must be pre-initialized	*/
/* based on the initial P.						*/
/*									*/
/* FTOL is the fractional convergence tolerance to be achieved in the	*/
/* function. 0.00000001 (1e-8) is a good value.				*/
/*									*/
/* The number of iterations is returned in ITER.  ITMAX is the maximum	*/
/* number of iterations allowed (suggestion: 5000).			*/
/*									*/
/* "func" is a pointer to the actual function to be minimized.  It	*/
/* must have the signature:						*/
/*   double func(double p[], int ndim, void *func_args);		*/
/* AmoebaFunc is a typedef for this pointer defined in the include	*/
/* file.  If you're using C++, this function must be declared		*/
/* extern "C".								*/
/*									*/
/* "func_args" is an opaque pointer that is passed into the function.	*/
/* It will normally be a pointer to a structure that contains all the	*/
/* additional arguments that the function needs.			*/
/*									*/
/* "print_interval" says how often to print a progress message.  When	*/
/* the number of iterations is a multiple of this, the message is	*/
/* printed.  If 0, no message is printed.				*/
/*									*/
/* Sample call:								*/
/*	struct CostParams {						*/
/*	    int a, b;  double c;					*/
/*	};								*/
/*	double cost(double p[], int ndim, void *func_args)		*/
/*	    struct CostParams *params = (struct CostParams *)func_args;	*/
/*	    return params->a + p[0] * params->c - p[1];	    (whatever)	*/
/*	}								*/
/*      ... in the caller ...						*/
/*	struct CostParams parm;						*/
/*	double P[11][10], Y[11];					*/
/*	parm.a = 5;  parm.b = 10;  parm.c = 3.14159;			*/
/*	... fill up P[0][*] with initial solution ...			*/
/*	... copy P[0][*] to P[1:10][*] ...				*/
/*	... add labmda to P[i+1][i] ...					*/
/*	for (i=0; i<11; i++)						*/
/*	    Y[i] = cost(P[i], 10, &parm);				*/
/*	amoeba(P, Y, 10, 10, 1e-8, 1000, &iter, cost, &parm);		*/
/*	... P[0][*] contains solution and Y[0] the function value ...	*/
/*									*/
/* Note that this is callable only from C; due to the function pointers	*/
/* and such a Fortran bridge is not feasible.  Use the original Fortran	*/
/* code from the book if you need to.					*/
/************************************************************************/

#include "amoeba.h"
#include "pamoeba.h"
#include <zvproto.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

void amoeba_base(double *Parg, double Y[], int NDIM, int WIDTH,
	double FTOL, int ITMAX, int *ITER, AmoebaFunc func, void *func_args,
	int print_interval)
{
    pamoeba_base(Parg, Y, NDIM, WIDTH, FTOL, ITMAX, ITER,
		func, func_args, print_interval, 1, 0);
}

/************************************************************************/
/* This is the traditional call.					*/
/************************************************************************/
void amoeba(double *Parg, double Y[], int NDIM, int WIDTH,
	double FTOL, int ITMAX, int *ITER, AmoebaFunc func, void *func_args)
{
    pamoeba(Parg, Y, NDIM, WIDTH, FTOL, ITMAX, ITER, func, func_args, 1, 0);
}

/************************************************************************/
/* This is a wrapper around the amoeba algorithm which does the		*/
/* initialization for you.  You provide only the initial solution,	*/
/* Pzero, as a simple vector of size NDIM, plus the "length scale"	*/
/* constant, lambda.  This wrapper will allocate P and Y, fill them	*/
/* up appropriately, call amoeba, and return the P[0] solution in	*/
/* Pzero an the Y[0] value as the function return.			*/
/*									*/
/* This should handle all uses of amoeba except when you want lambda	*/
/* to be a vector, e.g. a different "length scale" for each variable.	*/
/* See amoeba3 for this case.						*/
/*									*/
/* Note that the P macro from above is used here as well.		*/
/*									*/
/* Sample call:								*/
/*	struct CostParams {						*/
/*	    int a, b;  double c;					*/
/*	};								*/
/*	double cost(double p[], int ndim, void *func_args)		*/
/*	    struct CostParams *params = (struct CostParams *)func_args;	*/
/*	    return params->a + p[0] * params->c - p[1];	    (whatever)	*/
/*	}								*/
/*      ... in the caller ...						*/
/*	struct CostParams parm;						*/
/*	double Pzero[10], value;					*/
/*	parm.a = 5;  parm.b = 10;  parm.c = 3.14159;			*/
/*	... fill up Pzero[*] with initial solution ...			*/
/*	value = amoeba2(Pzero, .1, 10, 1e-8, 1000, &iter, cost, &parm);	*/
/*	... Pzero contains solution and "value" the function value ...	*/
/*									*/
/************************************************************************/

double amoeba2(double *Pzero, double lambda,
		int NDIM, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args)
{
    return pamoeba2(Pzero, lambda, NDIM, FTOL, ITMAX, ITER,
		func, func_args, 1, 0);
}

/************************************************************************/
/* This is another wrapper around the amoeba algorithm which does the	*/
/* initialization for you.  It is exactly like amoeba2, except that the	*/
/* "length scale" constant, lambda, is an array of doubles (of size	*/
/* NDIM) instead of a single value.  This allows you to have a		*/
/* different length scale for each variable.				*/
/************************************************************************/

double amoeba3(double *Pzero, double *lambda_vec,
		int NDIM, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args)
{
    return pamoeba3(Pzero, lambda_vec, NDIM, FTOL, ITMAX, ITER,
		func, func_args, 1, 0);
}

/************************************************************************/
/* This is another wrapper around the amoeba algorithm.  Just like	*/
/* amoeba3 except a "print_interval" argument allows you to specify	*/
/* how often to print a progress message.  A message will be printedy	*/
/* every "print_interval" iterations, if it is >= 0.			*/
/************************************************************************/

double amoeba4(double *Pzero, double *lambda_vec,
		int NDIM, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args, int print_interval)
{
    return pamoeba4(Pzero, lambda_vec, NDIM, FTOL, ITMAX, ITER,
		func, func_args, print_interval, 1, 0);
}
