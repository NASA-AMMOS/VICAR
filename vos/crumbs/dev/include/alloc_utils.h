/***************** ACTORS Software **************************************
 *
 *	Copyright (C) 1994, California Institute of Technology
 *	All rights reserved.
 ************************************************************************
 *	Developed by the Visualization & Earth Science Applications Group,
 *	Image Processing Applications and Development Section,
 *	Jet Propulsion Laboratory,
 *	California Institute of Technology
 ************************************************************************
 * Module: alloc_utils.h
 *
 * Purpose: Memory allocation utility macros
 *
 * Limitations:
 *              
 * Original Author: Stephen H. Watson
 *
 * Current cognizant programmer: Stephen H. Watson
 *
 * Created: April 1994
 *
 * Last Modified: 12 Oct 1994  4.1
 *
 ************************************************************************
 */

#define NOT_ENOUGH_MEMORY -1

#define alloc_matrix(new_var,dim1,dim2,type) {  \
  int i; \
  if (!((new_var) = (type **)malloc(dim1*sizeof(type *))) ) { \
    fprintf(stderr, "Out of core; allocation failed in alloc_matrix(). \n"); \
    exit(NOT_ENOUGH_MEMORY); \
  } \
  else { \
    if (!(new_var[0] = (type *) malloc(dim2*dim1*sizeof(type))) ) {\
      fprintf(stderr, "Out of core; allocation failed in alloc_matrix(). \n"); \
      exit(NOT_ENOUGH_MEMORY); \
    } \
    else { \
      for(i=1;i<dim1;i++) new_var[i]=new_var[0]+dim2*i; \
    } \
  } \
}

#define alloc_vector(new_var,dim1,type) { \
  if ( !((new_var) = (type *)malloc(dim1*sizeof(type))) ) { \
    fprintf(stderr, "Out of core; allocation failed. \n"); \
    exit(NOT_ENOUGH_MEMORY); \
  } \
}

#define alloc_struct(new_var,type)  {  \
  if ( !((new_var) = (type *)malloc(sizeof(type))) ) { \
    fprintf(stderr, "Out of core; allocation failed. \n"); \
    exit(NOT_ENOUGH_MEMORY); \
  } \
}
