#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <zvproto.h>
#include "ImageUtils.h"
#include "LandsatManager.h"

/******************************************************************************/
int LM_checkLandsatCommonPreconditions(LANDSAT_MANAGER *lm, int band)
{
   if(!lm->units_set[band]) return LANDSAT_UNIT_NOT_SET;
   if(lm->lmin[band] == LANDSAT_UNINITIALIZED) return LANDSAT_LMIN_NOT_SET;
   if(lm->lmax[band] == LANDSAT_UNINITIALIZED) return LANDSAT_LMAX_NOT_SET;
   if(lm->qcalmin[band] == LANDSAT_UNINITIALIZED || lm->qcalmax[band] == LANDSAT_UNINITIALIZED) return LANDSAT_QCAL_NOT_SET;

   return LANDSAT_SUCCESS;
}

/******************************************************************************/
double LM_getEarthSunDist(int julDate)
{
   double eccentricity, distance;

   eccentricity = 0.016710219;
   distance = 1/((1 + eccentricity*cos((julDate - 4)*2*M_PI/365.25))/(1-pow(eccentricity, 2)));

   return distance;
}

/******************************************************************************/
int LM_getDayOfYear(int year, int month, int day)
{
   int leap;
   int leapyear[] = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335};
   int regyear[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
   int dayOfYear;

   leap = 0;
   if(!(year%400)) leap = 1;
   else if(!(year%100)) leap = 0;
   else if(!(year%4)) leap = 1;

   if(leap) dayOfYear = leapyear[month-1];
   else dayOfYear = regyear[month-1];

   dayOfYear += day;

   return dayOfYear;
}

/******************************************************************************/
int LM_fillMetadata(LANDSAT_MANAGER *lm, FILE *metafile)
{
   int i, year, month, day, julDate;
   char metabuf[LANDSAT_METABUF_SIZE];

   if(metafile == NULL) return LANDSAT_NO_METAFILE;

   year = month = day = LANDSAT_UNINITIALIZED;
   for(i = 0; i < LANDSAT_N_BANDS; i++)
   {
      lm->lmax[i] = LANDSAT_UNINITIALIZED;
      lm->lmin[i] = LANDSAT_UNINITIALIZED;
      lm->qcalmax[i] = LANDSAT_UNINITIALIZED;
      lm->qcalmin[i] = LANDSAT_UNINITIALIZED;
   }

   fseek(metafile, 0, SEEK_SET);
   while(fscanf(metafile, "%s", metabuf) != EOF)
   {
     //      printf("%s\n", metabuf);
      if(!strcmp(metabuf, "LMAX_BAND1") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND1])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND2") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND2])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND3") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND3])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND4") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND4])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND5") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND5])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND6") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND61])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND61") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND61])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND62") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND62])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND7") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND7])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMAX_BAND8") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmax[LANDSAT_BAND8])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND1") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND1])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND2") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND2])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND3") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND3])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND4") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND4])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND5") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND5])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND6") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND61])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND61") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND61])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND62") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND62])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND7") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND7])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "LMIN_BAND8") && fscanf(metafile, "%s %lf", metabuf, &(lm->lmin[LANDSAT_BAND8])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND1") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND1])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND2") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND2])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND3") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND3])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND4") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND4])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND5") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND5])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND6") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND61])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND61") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND61])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND62") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND62])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND7") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND7])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMAX_BAND8") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmax[LANDSAT_BAND8])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND1") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND1])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND2") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND2])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND3") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND3])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND4") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND4])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND5") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND5])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND6") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND61])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND61") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND61])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND62") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND62])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND7") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND7])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "QCALMIN_BAND8") && fscanf(metafile, "%s %lf", metabuf, &(lm->qcalmin[LANDSAT_BAND8])) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "SUN_ELEVATION") && fscanf(metafile, "%s %lf", metabuf, &(lm->elevation)) != 2)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "ACQUISITION_DATE") && fscanf(metafile, "%s %d %c %d %c %d", metabuf, &year, metabuf, &month, metabuf, &day) != 6)
         return LANDSAT_NO_DATA;
      else if(!strcmp(metabuf, "END")) break;
   }

   if(year == LANDSAT_UNINITIALIZED ||
      month == LANDSAT_UNINITIALIZED ||
      day == LANDSAT_UNINITIALIZED)
      return LANDSAT_NO_DATA;
   julDate = LM_getDayOfYear(year, month, day);
   lm->dist = LM_getEarthSunDist(julDate);

   /* debug printing statements */
   printf("lmax2: %f\n", lm->lmax[LANDSAT_BAND2]);
   printf("lmin2: %f\n", lm->lmin[LANDSAT_BAND2]);
   printf("lmax3: %f\n", lm->lmax[LANDSAT_BAND3]);
   printf("lmin3: %f\n", lm->lmin[LANDSAT_BAND3]);
   printf("lmax4: %f\n", lm->lmax[LANDSAT_BAND4]);
   printf("lmin4: %f\n", lm->lmin[LANDSAT_BAND4]);
   printf("lmax5: %f\n", lm->lmax[LANDSAT_BAND5]);
   printf("lmin5: %f\n", lm->lmin[LANDSAT_BAND5]);
   printf("lmax6: %f\n", lm->lmax[LANDSAT_BAND61]);
   printf("lmin6: %f\n", lm->lmin[LANDSAT_BAND61]);

   printf("qcalmax2: %f\n", lm->qcalmax[LANDSAT_BAND2]);
   printf("qcalmin2: %f\n", lm->qcalmin[LANDSAT_BAND2]);
   printf("qcalmax3: %f\n", lm->qcalmax[LANDSAT_BAND3]);
   printf("qcalmin3: %f\n", lm->qcalmin[LANDSAT_BAND3]);
   printf("qcalmax4: %f\n", lm->qcalmax[LANDSAT_BAND4]);
   printf("qcalmin4: %f\n", lm->qcalmin[LANDSAT_BAND4]);
   printf("qcalmax5: %f\n", lm->qcalmax[LANDSAT_BAND5]);
   printf("qcalmin5: %f\n", lm->qcalmin[LANDSAT_BAND5]);
   printf("qcalmax6: %f\n", lm->qcalmax[LANDSAT_BAND61]);
   printf("qcalmin6: %f\n", lm->qcalmin[LANDSAT_BAND61]);

   printf("elevation: %f\n", lm->elevation);
   printf("juldate: %d\n", julDate);
   printf("distance: %f\n", lm->dist);

   printf("Acquired data from metafile...\n");


   return LANDSAT_SUCCESS;
}

/******************************************************************************/
void LM_setBuffers(LANDSAT_MANAGER **lm)
{
   int i;

   // set radiance buffers
   (*lm)->rad_buffs = (double**)malloc(sizeof(double*)*LANDSAT_N_BANDS);
   for(i = 0; i < LANDSAT_N_BANDS; i++)
   {
      if((*lm)->units_set[i] == LANDSAT_BAND_SET)
      {
         (*lm)->rad_buffs[i] = (double*)calloc((*lm)->images[i]->ns, sizeof(double));
         (*lm)->curr_line_in_rad_buffs[i] = -1;
      }
      else
         (*lm)->rad_buffs[i] = NULL;
   }

   // set reflectance buffers
   (*lm)->ref_buffs = (double**)malloc(sizeof(double*)*(LANDSAT_N_REF_BANDS));
   for(i = 0; i < LANDSAT_N_REF_BANDS-2; i++)
   {
      if((*lm)->units_set[i] == LANDSAT_BAND_SET)
      {
         (*lm)->ref_buffs[i] = (double*)calloc((*lm)->images[i]->ns, sizeof(double));
         (*lm)->curr_line_in_ref_buffs[i] = -1;
      }
      else
         (*lm)->ref_buffs[i] = NULL;
   }
   if((*lm)->units_set[LANDSAT_SHORTWAVE_IR_2] == LANDSAT_BAND_SET)
   {
      (*lm)->ref_buffs[i] = (double*)calloc((*lm)->images[LANDSAT_SHORTWAVE_IR_2]->ns, sizeof(double));
      (*lm)->curr_line_in_ref_buffs[i] = -1;
   }
   else
      (*lm)->ref_buffs[i] = NULL;

   if((*lm)->units_set[LANDSAT_PAN] == LANDSAT_BAND_SET)
   {
      (*lm)->ref_buffs[i+1] = (double*)calloc((*lm)->images[LANDSAT_PAN]->ns, sizeof(double));
      (*lm)->curr_line_in_ref_buffs[i] = -1;
   }
   else
      (*lm)->ref_buffs[i+1] = NULL;

   // set brightness temperature buffer
   (*lm)->b_temp_buffs = (double**)malloc(sizeof(double*)*LANDSAT_N_TIR_BANDS);
   for(i = 0; i < LANDSAT_N_TIR_BANDS; i++)
   {
      if((*lm)->units_set[LANDSAT_THERMAL_IR_1 + i] == LANDSAT_BAND_SET)
      {
         (*lm)->b_temp_buffs[i] = (double*)calloc((*lm)->images[LANDSAT_THERMAL_IR_1]->ns, sizeof(double));
         (*lm)->curr_line_in_b_temp_buffs[i] = -1;
      }
      else
         (*lm)->b_temp_buffs[i] = NULL;
   }
}

/******************************************************************************/
LANDSAT_MANAGER* LM_getLandsatManager(int units[LANDSAT_N_BANDS], FILE *metafile)
{
   int i, status;
   LANDSAT_MANAGER *lm;

   lm = (LANDSAT_MANAGER*)malloc(sizeof(LANDSAT_MANAGER));

   status = LM_fillMetadata(lm, metafile);
   switch(status)
   {
      case LANDSAT_NO_METAFILE:
         printf("Metafile not found.");
         zabend();
      case LANDSAT_NO_DATA:
         printf("Error while attempting to attain metafile data.");
         zabend();
   }

   for(i = 0; i < LANDSAT_N_BANDS; i++)
   {
      if(units[i] > -1)
      {
         lm->images[i] = getImage(units[i]);
         lm->units_set[i] = LANDSAT_BAND_SET;
      }
      else
         lm->units_set[i] = LANDSAT_BAND_NOT_SET;
   }

   LM_setBuffers(&lm);

   return lm;
}

/******************************************************************************/
LANDSAT_MANAGER* LM_getLandsatManager2(VICAR_IMAGE *vi[LANDSAT_N_BANDS], FILE *metafile)
{
   int i, status;
   LANDSAT_MANAGER *lm;

   lm = (LANDSAT_MANAGER*)malloc(sizeof(LANDSAT_MANAGER));

   status = LM_fillMetadata(lm, metafile);
   switch(status)
   {
      case LANDSAT_NO_METAFILE:
         printf("Metafile not found.");
         zabend();
      case LANDSAT_NO_DATA:
         printf("Error while attempting to attain metafile data.");
         zabend();
   }

   for(i = 0; i < LANDSAT_N_BANDS; i++)
   {
      lm->images[i] = vi[i];
      if(vi[i] != NULL)
         lm->units_set[i] = LANDSAT_BAND_SET;
      else
         lm->units_set[i] = LANDSAT_BAND_NOT_SET;
   }

   LM_setBuffers(&lm);

   return lm;
}

/******************************************************************************/
void LM_deleteLandsatManager(LANDSAT_MANAGER **lm)
{
   int i;

   // free rad buffs and delete VICAR IMAGE structs
   for(i = 0; i < LANDSAT_N_BANDS; i++)
   {
      if((*lm)->units_set[i] == LANDSAT_BAND_SET)
      {
         if((*lm)->rad_buffs[i] != NULL)
            free((*lm)->rad_buffs[i]);
         (*lm)->rad_buffs[i] = NULL;
         deleteAndCloseImage(&((*lm)->images[i]));
      }
   }

   // free reflectance buffers
   for(i = 0; i < LANDSAT_N_REF_BANDS; i++)
   {
       if((*lm)->ref_buffs[i] != NULL)
          free((*lm)->ref_buffs[i]);
   }
   // free brightness termperature buffer
   for(i = 0; i < LANDSAT_N_TIR_BANDS; i++)
   {
      if((*lm)->b_temp_buffs[i] != NULL)
         free((*lm)->b_temp_buffs[i]);
   }

   free((*lm)->b_temp_buffs);
   free((*lm)->rad_buffs);
   free((*lm)->ref_buffs);
   free(*lm);
}

/******************************************************************************/
int LM_getRadianceLine(LANDSAT_MANAGER *lm, int band, int line)
{
   int i;

   if(lm->curr_line_in_rad_buffs[band] == line) return LANDSAT_SUCCESS;

   readVicarImageLine(lm->images[band], line);
   for(i = 0; i < lm->images[band]->ns; i++)
   {
      double pixel;

      pixel = lm->images[band]->buffer[i];
      if(fabs(pixel) < 10E-10) lm->rad_buffs[band][i] = 0.0;
      else lm->rad_buffs[band][i] = (lm->lmax[band] - lm->lmin[band])/
                                    (lm->qcalmax[band] - lm->qcalmin[band])*
                                    (pixel - lm->qcalmin[band]) + lm->lmin[band];
   }
   lm->curr_line_in_rad_buffs[band] = line;

   return LANDSAT_SUCCESS;
}

/******************************************************************************/
int LM_createRadianceImage(LANDSAT_MANAGER *lm, VICAR_IMAGE *vi, int band)
{
   int i, err;

   err = LM_checkLandsatCommonPreconditions(lm, band);
   if(err != LANDSAT_SUCCESS) return err;

   for(i = 0; i < vi->nl; i++)
   {
      assert(LM_getRadianceLine(lm, band, i) == LANDSAT_SUCCESS);
      memcpy(vi->buffer, lm->rad_buffs[band], vi->ns*sizeof(double));
      writeVicarImageLine(vi, i);
   }

   return LANDSAT_SUCCESS;
}

/******************************************************************************/
int LM_getReflectanceLine(LANDSAT_MANAGER *lm, int band, int line)
{
   int i;

   if(lm->curr_line_in_ref_buffs[band] == line) return LANDSAT_SUCCESS;

   LM_getRadianceLine(lm, band, line);
   for(i = 0; i < lm->images[band]->ns; i++)
   {
      double rad;

      rad = lm->rad_buffs[band][i];
      if(fabs(rad) < 10E-10) lm->ref_buffs[band][i] = 0.0;
      else lm->ref_buffs[band][i] = lm->rad_buffs[band][i]*pow(lm->dist, 2.0)/
                                    (LANDSAT_ESUN[band]*cos((90-lm->elevation)*M_PI/180))*M_PI;
   }
   lm->curr_line_in_ref_buffs[band] = line;

   return LANDSAT_SUCCESS;
}

/******************************************************************************/
int LM_createReflectanceImage(LANDSAT_MANAGER *lm, VICAR_IMAGE *vi, int band)
{
   int i, err;

   err = LM_checkLandsatCommonPreconditions(lm, band);
   if(err != LANDSAT_SUCCESS) return err;

   for(i = 0; i < vi->nl; i++)
   {
      assert(LM_getReflectanceLine(lm, band, i) == LANDSAT_SUCCESS);
      memcpy(vi->buffer, lm->ref_buffs[band], vi->ns*sizeof(double));
      writeVicarImageLine(vi, i);
   }

   return LANDSAT_SUCCESS;
}

/******************************************************************************/
int LM_getBTempLine(LANDSAT_MANAGER *lm, int band, int line)
{
   int i, bTempBand;

   bTempBand = band - LANDSAT_THERMAL_IR_1;
   if(lm->curr_line_in_b_temp_buffs[bTempBand] == line) return LANDSAT_SUCCESS;

   LM_getRadianceLine(lm, band, line);
   for(i = 0; i < lm->images[band]->ns; i++)
   {
      double logparm;

      if(fabs(lm->images[band]->buffer[i]) < 10E-10 || fabs(lm->rad_buffs[band][i]) < 10E-10)
      {
         lm->b_temp_buffs[bTempBand][i] = 0.0;
         continue;
      }

      logparm = LANDSAT_BTEMP_K1/lm->rad_buffs[band][i] + 1.;
      if(logparm <= 0.)
         lm->b_temp_buffs[bTempBand][i] = 0.0;
      else
         lm->b_temp_buffs[bTempBand][i] = LANDSAT_BTEMP_K2/log(logparm);
   }
   lm->curr_line_in_b_temp_buffs[bTempBand] = line;

   return LANDSAT_SUCCESS;
}

/******************************************************************************/
int LM_createBTempImage(LANDSAT_MANAGER *lm, VICAR_IMAGE *vi, int band)
{
   int i, bTempBand, err;

   err = LM_checkLandsatCommonPreconditions(lm, band);
   if(err != LANDSAT_SUCCESS) return err;

   bTempBand = band - LANDSAT_THERMAL_IR_1;
   for(i = 0; i < vi->nl; i++)
   {
      assert(LM_getBTempLine(lm, band, i) == LANDSAT_SUCCESS);
      memcpy(vi->buffer, lm->b_temp_buffs[bTempBand], vi->ns*sizeof(double));
      writeVicarImageLine(vi, i);
   }

   return LANDSAT_SUCCESS;
}
