#ifndef _AEJ_H
#define _AEJ_H
// aej.h 1.2 02/07/10 12:48:08
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:    4-Feb-98
 + PURPOSE: Defines globals and includes standard header files 
 **/

#include <fstream.h> 
#include <iostream.h> 
#include <time.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define Pi 3.14159265359
#define EPSILON 1e-8
#define LARGE 1e8
#define DEG2RAD 0.01745329251
#define RAD2DEG 57.29578
#define POLYGON_END -1
#define NULLINDEX -1
#define TRUE 1
#define FALSE 0
#define MAX(_x,_y) (_x)<(_y) ? (_y) : (_x)
#define MIN(_x,_y) (_x)<(_y) ? (_x) : (_y)
#define SGN(_x) (_x)<0 ? -1.0 : 1.0
#define SQR(_x) ((_x)*(_x))

#endif
