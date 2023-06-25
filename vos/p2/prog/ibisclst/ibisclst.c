#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "defines.h"
#include "ibisControlMapper.h"
#include "ibishelper.h"

#define MAX_IND 20

/*=========================================================*/
void getInds(IBISStruct *ibis, IBIS_CONTROL_MAP *map, int *indcols, double *ind, int index, int indcnt)
{
   int i;

   for(i = 0; i < indcnt; i++)
      ind[i] = IBISHELPER_getDouble(ibis, indcols[i], index);
}

/*=========================================================*/
void printCluster(IBISStruct *ibis, IBIS_CONTROL_MAP *map, int *inCluster)
{
   int i, status, outCol, dumcnt, dumdef;

   status = zvparm("outcol", &outCol, &dumcnt, &dumdef, 1, 0);
   assert(status == 1);
   --outCol;

   for(i = 0; i < map->length; i++)
   {
      if(!inCluster[i])
         IBISHELPER_setDouble(ibis, outCol, map->toIbisIndices[i], 0.);
      else
         IBISHELPER_setDouble(ibis, outCol, map->toIbisIndices[i], 1.);
   }
}

/*=========================================================*/
void markCluster(int **linkMatrix, int *inCluster, int root, int n)
{
   int i, j;

   inCluster[root] = 1;
   for(i = root; i < n; i++)
   {
      if(!inCluster[i]) continue;
      for(j = i+1; j < n; j++)
         if(linkMatrix[i][j]) inCluster[j] = 1;
   }
}

/*=========================================================*/
int calculateAggregateNeighbors(int **links, int* aggregNeighbors, int n)
{
   int i, j, max;

   for(i = n; i >= 0; i--)
      for(j = i+1; j < n; j++)
         if(links[i][j]) aggregNeighbors[i] += aggregNeighbors[j] + 1;

   max = 0;
   for(i = 1; i < n; i++)
      if(aggregNeighbors[i] > aggregNeighbors[max]) max = i;

   return max;
}

/*=========================================================*/
void markLinks(double **distMatrix, int **linkMatrix, int n)
{
   int i, j, status, dumcnt, dumdef;
   double thresh;

   status = zvparmd("thresh", &thresh, &dumcnt, &dumdef, 1, 0);
   assert(status == 1);

   for(i = 0; i < n; i++)
      for(j = i+1; j < n; j++)
         if(distMatrix[i][j] < thresh) linkMatrix[i][j] = 1;
         else linkMatrix[i][j] = 0;
}

/*=========================================================*/
double getDistance(double *ind1, double *ind2, int cnt)
{
   int i;
   double sum;

   sum = 0;
   for(i = 0; i < cnt; i++)
      sum += pow(ind1[i]-ind2[i], 2.0);
   sum = pow(sum, 0.5);

   return sum;
}

/*=========================================================*/
void calculateDistance(IBISStruct *ibis, IBIS_CONTROL_MAP *map, double **matrix)
{
   int status;
   int i, j;
   double ind1[MAX_IND], ind2[MAX_IND];
   int indcols[MAX_IND], indcnt, dumdef;

   status = zvparm("cols", indcols, &indcnt, &dumdef, MAX_IND, 0);
   assert(status == 1);

   for(i = 0; i < indcnt; i++) indcols[i]--;

   for(i = 0; i < map->length; i++)
   {
      getInds(ibis, map, indcols, ind1, map->toIbisIndices[i], indcnt);
      for(j = i+1; j < map->length; j++)
      {
         getInds(ibis, map, indcols, ind2, map->toIbisIndices[j], indcnt);
         matrix[i][j] = getDistance(ind1, ind2, indcnt);
      }
   }
}

/*=========================================================*/
void cluster(IBISStruct *ibis, IBIS_CONTROL_MAP *map)
{
   int i;
   double **distMatrix;
   int **linkMatrix, *neighborCnt, *inCluster, root;

   distMatrix = (double**)malloc(sizeof(double*)*map->length);
   linkMatrix = (int**)malloc(sizeof(int*)*map->length);
   for(i = 0; i < map->length; i++)
   {
      distMatrix[i] = (double*)calloc(map->length, sizeof(double));
      linkMatrix[i] = (int*)calloc(map->length, sizeof(int));
   }
   neighborCnt = (int*)calloc(map->length, sizeof(int));
   inCluster = (int*)calloc(map->length, sizeof(int));

   calculateDistance(ibis, map, distMatrix);
   markLinks(distMatrix, linkMatrix, map->length);
   root = calculateAggregateNeighbors(linkMatrix, neighborCnt, map->length);
   markCluster(linkMatrix, inCluster, root, map->length);
   printCluster(ibis, map, inCluster);
}

/*=========================================================*/
void main44(void)
{
   int i, status, controlCnt, controlCol, dumdef;
   IBIS_CONTROL_MAPPER *mapper;
   IBISStruct *ibis;

   zifmessage("ibisclst version 2017-08-08");

   ibis = IBISHELPER_openIBIS("inp", 1, "update");
   status = zvparm("concol", &controlCol, &controlCnt, &dumdef, 1, 0);
   assert(status == 1);

   if(controlCnt == 0) mapper = IBISCONTROL_getSingleMapper(ibis);
   else mapper = IBISCONTROL_getMapper(ibis, controlCol);

   for(i = 0; i < mapper->nControls; i++) cluster(ibis, mapper->maps[i]);

   IBISHELPER_closeIBIS(&ibis);
}
