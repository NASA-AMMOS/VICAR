#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <zvproto.h>
#include "ImageUtils.h"
#include "WV2Manager.h"

/******************************************************************************/
void WV2_checkPreconditions(WV2_MANAGER *wv2, int band)
{
   if(band < WV2_BAND1 || band > WV2_BAND_PAN)
   {
      printf("Band number :%d is outside of range.\n", band+1);
      zabend();
   }

   if(wv2->units_set[band] == WV2_UNIT_NOT_SET)
   {
      printf("Image for band %d not give.\n", band+1);
      zabend();
   }

   if(wv2->metaFlags[band] != WV2_META_ALL_SET)
   {
      printf("Meta data for band %d unavailable.", band+1);
      zabend();
   }
}

/******************************************************************************/
double WV2_getEarthSunDist(double year, double month, double day, double hh, double mm, double ssdd)
{
   double ut, JD, D, g, radg, dist;
   int A, B;

   ut = hh + mm/60.0 + ssdd/3600.0;
   if(month == 1.0 || month == 2.0)
   {
      year = year - 1;
      month = month + 12;
   }

   A = (int)(year/100);
   B = 2 - A + (int)(A/4);
   JD = (int)(365.25*(year + 4716)) + (int)(30.6001*(month + 1)) + day + ut/24.0 + B - 1524.5;
   D = JD - 2451545.0;
   g = 357.529 + 0.98560028*D;
   radg = g*(M_PI/180.);
   dist = 1.00014 - 0.01671*cos(radg) - 0.00014*cos(2*radg);

   if(dist < 0.983 || dist > 1.017)
      zmabend("ERROR: earth sun distance is outside of range\n");

   return dist;
}

/******************************************************************************/
int WV2_readMetaFile(WV2_MANAGER *wv2, char *fname, int isPan)
{
   int index, corrupted;
   char line[500];
   FILE *metafile = NULL;

   //   printf("strlen: %d\n", strlen(fname));
   metafile = fopen(fname, "r");
   if(metafile == NULL)
      return WV2_NO_METAFILE;

   // assume file is corrupted until we see good data at the end
   corrupted = 1;
   index = -1;
   while(fgets(line, sizeof(line), metafile) != NULL)
   {
      char *ptr;

      ptr = strstr(line, "BEGIN_GROUP = BAND_C");
      if(ptr != NULL) index = WV2_BAND1;
      ptr = strstr(line, "BEGIN_GROUP = BAND_B");
      if(ptr != NULL) index = WV2_BAND2;
      ptr = strstr(line, "BEGIN_GROUP = BAND_G");
      if(ptr != NULL) index = WV2_BAND3;
      ptr = strstr(line, "BEGIN_GROUP = BAND_Y");
      if(ptr != NULL) index = WV2_BAND4;
      ptr = strstr(line, "BEGIN_GROUP = BAND_R");
      if(ptr != NULL) index = WV2_BAND5;
      ptr = strstr(line, "BEGIN_GROUP = BAND_RE");
      if(ptr != NULL) index = WV2_BAND6;
      ptr = strstr(line, "BEGIN_GROUP = BAND_N");
      if(ptr != NULL) index = WV2_BAND7;
      ptr = strstr(line, "BEGIN_GROUP = BAND_N2");
      if(ptr != NULL) index = WV2_BAND8;
      ptr = strstr(line, "BEGIN_GROUP = BAND_P");
      if(ptr != NULL) index = WV2_BAND_PAN;
      ptr = strstr(line, "END_GROUP = BAND_");
      if(ptr != NULL) index = -1;
      //      printf("%d %s", index, line);

      // look for absolute calibration factor
      ptr = strstr(line, "absCalFactor = ");
      if(ptr != NULL)
      {
         ptr += strlen("absCalFactor = ");
         wv2->absCalFactor[index] = atof(ptr);
         wv2->metaFlags[index] += WV2_META_CAL_SET;
         continue;
      }

      // look for bandwidth
      ptr = strstr(line, "effectiveBandwidth = ");
      if(ptr != NULL)
      {
         ptr += strlen("effectiveBandwidth = ");
         wv2->effectiveBandwidth[index] = atof(ptr);
         wv2->metaFlags[index] += WV2_META_BWID_SET;
         continue;
      }

      // look for date
      ptr = strstr(line, "firstLineTime = ");
      if(ptr != NULL)
      {
         ptr += strlen("firstLineTime = ");
         if(!isPan)
            sscanf(ptr, "%lf-%lf-%lfT%lf:%lf:%lfZ;", &(wv2->year), &(wv2->month), &(wv2->day), &(wv2->hh), &(wv2->mm), &(wv2->ssdd));
         else
            sscanf(ptr, "%lf-%lf-%lfT%lf:%lf:%lfZ;", &(wv2->year), &(wv2->month), &(wv2->day), &(wv2->hh), &(wv2->mm), &(wv2->pan_ssdd));
         continue;
      }

      // look for elevation and set solar zenith angle
      ptr = strstr(line, "meanSunEl = ");
      if(ptr != NULL)
      {
         ptr += strlen("meanSunEl = ");
         if(!isPan)
         {
            wv2->solarElevation = atof(ptr);
            wv2->solarZenithAngle = 90. - wv2->solarElevation;
            wv2->solarZenithAngleInRadians = wv2->solarZenithAngle*(M_PI/180.);
         }
         else
         {
            wv2->pan_solarElevation = atof(ptr);
            wv2->pan_solarZenithAngle = 90. - wv2->pan_solarElevation;
            wv2->pan_solarZenithAngleInRadians = wv2->pan_solarZenithAngle*(M_PI/180.);
         }
         continue;
      }

      // look for good end of data
      ptr = strstr(line, "END;");
      if(ptr != NULL) corrupted = 0;
   }

   fclose(metafile);

   // return error if end of good data "END;"  was not found
   if(corrupted) return WV2_NO_DATA;

   return WV2_SUCCESS;
}

/******************************************************************************/
int WV2_fillMetadata(WV2_MANAGER *wv2, char *multiMetaFname, char *panMetaFname)
{
   int status; 

   // do multi-band meta file if specified
   if(strlen(multiMetaFname) > 0)
   {
      status = WV2_readMetaFile(wv2, multiMetaFname, WV2_IS_NOT_PAN);
      if(status != WV2_SUCCESS) return status;
      wv2->solarDist = WV2_getEarthSunDist(wv2->year, wv2->month, wv2->day, wv2->hh, wv2->mm, wv2->ssdd);
   }

   // do pan meta file if specified
   if(strlen(panMetaFname) > 0)
   {
      status = WV2_readMetaFile(wv2, panMetaFname, WV2_IS_PAN);
      if(status != WV2_SUCCESS) return status;
      wv2->pan_solarDist = WV2_getEarthSunDist(wv2->year, wv2->month, wv2->day, wv2->hh, wv2->mm, wv2->pan_ssdd);
   }


   return WV2_SUCCESS;
}

/******************************************************************************/
void WV2_setBuffers(WV2_MANAGER **wv2)
{
   int i;

   // set radiance buffers
   (*wv2)->rad_buffs = (double**)malloc(sizeof(double*)*WV2_N_BANDS);
   (*wv2)->radLookupTable = (double**)malloc(sizeof(double*)*WV2_N_BANDS);
   for(i = 0; i < WV2_N_BANDS; i++)
   {
      if((*wv2)->units_set[i] == WV2_UNIT_SET && (*wv2)->metaFlags[i] == WV2_META_ALL_SET)
      {
         (*wv2)->rad_buffs[i] = (double*)calloc((*wv2)->images[i]->ns, sizeof(double));
         (*wv2)->radLookupTable[i] = (double*)calloc(pow(2, 16), sizeof(double));
         (*wv2)->curr_line_in_rad_buffs[i] = -1;
      }
      else
         (*wv2)->rad_buffs[i] = NULL;
   }

   // set reflectance buffers
   (*wv2)->ref_buffs = (double**)malloc(sizeof(double*)*(WV2_N_BANDS));
   (*wv2)->refLookupTable = (double**)malloc(sizeof(double*)*WV2_N_BANDS);
   for(i = 0; i < WV2_N_BANDS; i++)
   {
      if((*wv2)->units_set[i] == WV2_UNIT_SET && (*wv2)->metaFlags[i] == WV2_META_ALL_SET)
      {
         (*wv2)->ref_buffs[i] = (double*)calloc((*wv2)->images[i]->ns, sizeof(double));
         (*wv2)->refLookupTable[i] = (double*)calloc(pow(2, 16), sizeof(double));
         (*wv2)->curr_line_in_ref_buffs[i] = -1;
      }
      else
         (*wv2)->ref_buffs[i] = NULL;
   }
}

/******************************************************************************/
WV2_MANAGER* WV2_getWV2Manager(VICAR_IMAGE *vi[WV2_N_BANDS], char *multiMetaFname, char *panMetaFname)
{
   int i, status;
   WV2_MANAGER *wv2;

   wv2 = (WV2_MANAGER*)malloc(sizeof(WV2_MANAGER));

   // initialize metaflags and get metadata
   for(i = 0; i < WV2_N_BANDS; i++)
      wv2->metaFlags[i] = WV2_META_NOT_SET;

   status = WV2_fillMetadata(wv2, multiMetaFname, panMetaFname);
   switch(status)
   {
      case WV2_NO_METAFILE:
         printf("Metafile not found.");
         zabend();
      case WV2_NO_DATA:
         printf("Error while attempting to attain metafile data.");
         zabend();
   }

   // check for input vicar images and initialize unit_set flags
   for(i = 0; i < WV2_N_BANDS; i++)
   {
      wv2->images[i] = vi[i];

      if(vi[i] != NULL) wv2->units_set[i] = WV2_UNIT_SET;
      else wv2->units_set[i] = WV2_UNIT_NOT_SET;
   }

   WV2_setBuffers(&wv2);

   return wv2;
}

/******************************************************************************/
void WV2_deleteWV2Manager(WV2_MANAGER **wv2)
{
   int i;

   // free rad buffs and delete VICAR IMAGE structs
   for(i = 0; i < WV2_N_BANDS; i++)
   {
      if((*wv2)->units_set[i] == WV2_UNIT_SET)
      {
         if((*wv2)->rad_buffs[i] != NULL)
            free((*wv2)->rad_buffs[i]);

         if((*wv2)->radLookupTable[i] != NULL)
            free((*wv2)->radLookupTable[i]);

         (*wv2)->rad_buffs[i] = NULL;
         deleteAndCloseImage(&((*wv2)->images[i]));
      }
   }

   // free reflectance buffers
   for(i = 0; i < WV2_N_BANDS; i++)
   {
      if((*wv2)->ref_buffs[i] != NULL)
         free((*wv2)->ref_buffs[i]);

      if((*wv2)->refLookupTable[i] != NULL)
         free((*wv2)->refLookupTable[i]);
   }

   free((*wv2)->rad_buffs);
   free((*wv2)->ref_buffs);
   free((*wv2)->radLookupTable);
   free((*wv2)->refLookupTable);
   free(*wv2);
}

/******************************************************************************/
double WV2_getTOARadiance(WV2_MANAGER *wv2, int band, double dn)
{
   return ((wv2->absCalFactor[band]*dn)/wv2->effectiveBandwidth[band]);
}

/******************************************************************************/
int WV2_setTOARadianceLine(WV2_MANAGER *wv2, int band, int line)
{
   int i;
   VICAR_IMAGE *vi;

   if(wv2->curr_line_in_rad_buffs[band] == line) return WV2_SUCCESS;

   vi = wv2->images[band];
   readVicarImageLine(vi, line);
   for(i = 0; i < vi->ns; i++)
   {
      wv2->rad_buffs[band][i] = wv2->radLookupTable[band][(int)(vi->buffer[i])];
//      wv2->rad_buffs[band][i] = WV2_getTOARadiance(wv2, band, vi->buffer[i]);
   }

   wv2->curr_line_in_rad_buffs[band] = line;

   return WV2_SUCCESS;
}


/******************************************************************************/
void WV2_initRadLookupTable(WV2_MANAGER *wv2, int band)
{
   int i;

   for(i = 0; i < (int)(pow(2, 16)); i++)
      wv2->radLookupTable[band][i] = WV2_getTOARadiance(wv2, band, (double)i);
}

/******************************************************************************/
int WV2_createTOARadianceImage(WV2_MANAGER *wv2, int outInst, int band)
{
   int i, status;
   VICAR_IMAGE *inp;
   VICAR_IMAGE *out;

   WV2_checkPreconditions(wv2, band);
   inp = wv2->images[band];
   status = zvselpi(outInst);
   out = getVI_out("REAL", outInst, inp->nl, inp->ns);

   // free the default buffer in output VICAR_IMAGE and point it to wv2 reflectance buffer
   free(out->buffer);
   out->buffer = wv2->rad_buffs[band];

   WV2_initRadLookupTable(wv2, band);
   for(i = 0; i < out->nl; i++)
   {
      WV2_setTOARadianceLine(wv2, band, i);
      writeVicarImageLine(out, i);
   }

   deleteAndCloseImage(&out);

   return WV2_SUCCESS;
}

/******************************************************************************/
double WV2_getTOAReflectance(WV2_MANAGER *wv2, int band, double radiance)
{
   double reflectance;
   
   if(band == WV2_BAND_PAN)
      reflectance = (radiance*pow(wv2->pan_solarDist, 2.0)*M_PI)/(WV2_ESUN[band]*cos(wv2->pan_solarZenithAngleInRadians));
   else
      reflectance = (radiance*pow(wv2->solarDist, 2.0)*M_PI)/(WV2_ESUN[band]*cos(wv2->solarZenithAngleInRadians));

   return reflectance;
}

/******************************************************************************/
int WV2_setTOAReflectanceLine(WV2_MANAGER *wv2, int band, int line)
{
   int i;
   VICAR_IMAGE *vi;

   if(wv2->curr_line_in_ref_buffs[band] == line) return WV2_SUCCESS;

   vi = wv2->images[band];
   readVicarImageLine(vi, line);
//   WV2_setTOARadianceLine(wv2, band, line);
   for(i = 0; i < vi->ns; i++)
   {
      wv2->ref_buffs[band][i] = wv2->refLookupTable[band][(int)vi->buffer[i]];
//      wv2->ref_buffs[band][i] = WV2_getTOAReflectance(wv2, band, wv2->rad_buffs[band][i]);
   }

   wv2->curr_line_in_ref_buffs[band] = line;

   return WV2_SUCCESS;
}

/******************************************************************************/
void WV2_initRefLookupTable(WV2_MANAGER *wv2, int band)
{
   int i;

   for(i = 0; i < (int)(pow(2, 16)); i++)
   {
      double rad = WV2_getTOARadiance(wv2, band, (double)i);
      wv2->refLookupTable[band][i] = WV2_getTOAReflectance(wv2, band, rad);
   }
}

/******************************************************************************/
int WV2_createTOAReflectanceImage(WV2_MANAGER *wv2, int outInst, int band)
{
   int i, status;
   VICAR_IMAGE *inp;
   VICAR_IMAGE *out;

   WV2_checkPreconditions(wv2, band);
   inp = wv2->images[band];
   status = zvselpi(outInst);
   out = getVI_out("REAL", outInst, inp->nl, inp->ns);

   // free the default buffer in output VICAR_IMAGE and point it to wv2 reflectance buffer
   free(out->buffer);
   out->buffer = wv2->ref_buffs[band];

   WV2_initRefLookupTable(wv2, band);
   for(i = 0; i < out->nl; i++)
   {
      WV2_setTOAReflectanceLine(wv2, band, i);
      writeVicarImageLine(out, i);
   }

   deleteAndCloseImage(&out);

   return WV2_SUCCESS;
}

/******************************************************************************/
void WV2_print(WV2_MANAGER *wv2)
{
   int i;

   printf("Metadata:\n");
   printf("=========\n");
   printf("datetime: %lf %lf %lf %lf %lf %lf\n", wv2->year, wv2->month, wv2->day, wv2->hh, wv2->mm, wv2->ssdd);
   printf("solar elevation/zenith: %lf %lf\n", wv2->solarElevation, wv2->solarZenithAngle);
   printf("solar distance: %.9lf\n", wv2->solarDist);

   printf("pan ssdd: %lf\n", wv2->pan_ssdd);
   printf("pan solar elevation/zenith: %lf %lf\n", wv2->pan_solarElevation, wv2->pan_solarZenithAngle);
   printf("pan solar distance: %.9lf\n", wv2->pan_solarDist);

   for(i = 0; i < WV2_N_BANDS; i++)
   {
      printf("-----\n");
      printf("Band: %d metaflag: %d\n", i+1, wv2->metaFlags[i]);
      printf("abscalfactor: %lf\n", wv2->absCalFactor[i]);
      printf("effectiveBandwidth: %lf\n", wv2->effectiveBandwidth[i]);
   }

}
