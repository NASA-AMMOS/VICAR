#ifndef CARTOSORTUTILS_H
#define CARTOSORTUTILS_H

#define CART_CHAR   1
#define CART_SHORT  2
#define CART_INT    3
#define CART_FLOAT  4
#define CART_DOUBLE 5
#define CART_LONG   6
#define CART_UCHAR  7
#define CART_USHORT 8
#define CART_UINT   9
#define CART_ULONG  10
#define CANNOT_COMPARE -2

/***************************************************/
/* This function performs selection sort on the    */
/* unsorted array and stores the SORTED ORDER      */
/* INDICES into indices array.  This function      */
/* does not move around the data but only returns  */
/* what the sorted index would be inside indices.  */
/*                                                 */
/* IN: void *unsorted - buffer containing unsorted */
/*     data                                        */
/*     int n - number of entries in unsorted buf   */
/*     int type - type of elements in unsorted     */
/*                buffer                           */
/*           1 = char                              */
/*           2 = short int                         */
/*           3 = int                               */
/*           4 = float                             */
/*           5 = double                            */
/*           6 = long int                          */
/*           7 = unsigned char                     */
/*           8 = unsigned short int                */
/*           9 = unsigned int                      */
/*           10 = unsigned long int                */
/*                                                 */
/* OUT: int *indices - returns sorted index order  */
/*      indices must be allocated before calling   */
/***************************************************/
void getSelectionSortIndices(void *unsorted, int *indices, int n, int type);

void sort8( double * buf, int * ptr, int n );

void sort88( double * buf, int * ptr, int n );

void sortrec4( int * key, int * ptr, int len );

void sortrec88( double * key, int * ptr, int len );

void sortrec8( double *key, int* ptr,int len );

void sortretn8( double *key, int* ptr, int len );

void sort4(int *buf, int *ptr, int n);

void sort7( float *buf, int *ptr, int n );

#endif
