#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <zvproto.h>
#include "ImageUtils.h"
#include "IKManager.h"

/******************************************************************************/
void IK_checkPreconditions(IK_MANAGER *ik, int band)
{
   if(band < IK_BAND1 || band > IK_BAND_PAN)
   {
      printf("Band number :%d is outside of range.\n", band+1);
      zabend();
   }

   if(ik->units_set[band] == IK_UNIT_NOT_SET)
   {
      printf("Image for band %d not give.\n", band+1);
      zabend();
   }

   if(ik->metaFlag != IK_META_ALL_SET)
   {
      printf("Meta data for band %d unavailable - flag %d.\n", band+1, ik->metaFlag);
      zabend();
   }
}

/******************************************************************************/
double IK_getEarthSunDist(double year, double month, double day, double hh, double mm, double ssdd)
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
int IK_readMetaFile(IK_MANAGER *ik, char *fname)
{
   int corrupted, dateSet, elevationSet;
   char line[500];
   FILE *metafile = NULL;

   //   printf("strlen: %d\n", strlen(fname));
   metafile = fopen(fname, "r");
   if(metafile == NULL)
      return IK_NO_METAFILE;

   // read metadata
   dateSet = elevationSet = 0;
   while(fgets(line, sizeof(line), metafile) != NULL)
   {
      char *ptr;

      if(dateSet && elevationSet) break;
      ptr = strstr(line, "Acquisition Date/Time:");
      if(ptr != NULL)
      {
         sscanf(line, "Acquisition Date/Time: %lf-%lf-%lf %lf:%lf", &(ik->year), &(ik->month), &(ik->day), &(ik->hh), &(ik->mm));
         dateSet = 1;
         continue;
      }

      ptr = strstr(line, "Sun Angle Elevation:");
      if(ptr != NULL)
      {
         sscanf(line, "Sun Angle Elevation: %lf degrees", &(ik->solarElevation));
         ik->solarZenithAngle = 90. - ik->solarElevation;
         ik->solarZenithAngleInRadians = ik->solarZenithAngle*(M_PI/180.);
         elevationSet = 1;
         continue;
      }
   }
   if(!dateSet || !elevationSet) ik->metaFlag = IK_META_NOT_SET;
   else ik->metaFlag = IK_META_ALL_SET;

   fclose(metafile);

   return IK_SUCCESS;
}

/******************************************************************************/
int IK_fillMetadata(IK_MANAGER *ik, char *metaFname)
{
   int status; 


   status = IK_readMetaFile(ik, metaFname);
   if(status != IK_SUCCESS) return status;
   ik->solarDist = IK_getEarthSunDist(ik->year, ik->month, ik->day, ik->hh, ik->mm, 0.);

   IK_print(ik);

   return IK_SUCCESS;
}

/******************************************************************************/
void IK_setBuffers(IK_MANAGER **ik)
{
   int i;

   // set radiance buffers
   (*ik)->rad_buffs = (double**)malloc(sizeof(double*)*IK_N_BANDS);
   for(i = 0; i < IK_N_BANDS; i++)
   {
      if((*ik)->units_set[i] == IK_UNIT_SET && (*ik)->metaFlag == IK_META_ALL_SET)
      {
         (*ik)->rad_buffs[i] = (double*)calloc((*ik)->images[i]->ns, sizeof(double));
         (*ik)->curr_line_in_rad_buffs[i] = -1;
      }
      else
         (*ik)->rad_buffs[i] = NULL;
   }

   // set reflectance buffers
   (*ik)->ref_buffs = (double**)malloc(sizeof(double*)*(IK_N_BANDS));
   for(i = 0; i < IK_N_BANDS; i++)
   {
      if((*ik)->units_set[i] == IK_UNIT_SET && (*ik)->metaFlag == IK_META_ALL_SET)
      {
         (*ik)->ref_buffs[i] = (double*)calloc((*ik)->images[i]->ns, sizeof(double));
         (*ik)->curr_line_in_ref_buffs[i] = -1;
      }
      else
         (*ik)->ref_buffs[i] = NULL;
   }
}

/******************************************************************************/
IK_MANAGER* IK_getIKManager(VICAR_IMAGE *vi[IK_N_BANDS], char *metaFname)
{
   int i, status;
   IK_MANAGER *ik;

   ik = (IK_MANAGER*)malloc(sizeof(IK_MANAGER));

   // initialize metaflags and get metadata
   for(i = 0; i < IK_N_BANDS; i++)
      ik->metaFlag = IK_META_NOT_SET;

   status = IK_fillMetadata(ik, metaFname);
   switch(status)
   {
      case IK_NO_METAFILE:
         printf("Metafile not found.");
         zabend();
      case IK_NO_DATA:
         printf("Error while attempting to attain metafile data.");
         zabend();
   }

   // check for input vicar images and initialize unit_set flags
   for(i = 0; i < IK_N_BANDS; i++)
   {
      ik->images[i] = vi[i];

      if(vi[i] != NULL) ik->units_set[i] = IK_UNIT_SET;
      else ik->units_set[i] = IK_UNIT_NOT_SET;
   }

   IK_setBuffers(&ik);

   return ik;
}

/******************************************************************************/
void IK_deleteIKManager(IK_MANAGER **ik)
{
   int i;

   // free rad buffs and delete VICAR IMAGE structs
   for(i = 0; i < IK_N_BANDS; i++)
   {
      if((*ik)->units_set[i] == IK_UNIT_SET)
      {
         if((*ik)->rad_buffs[i] != NULL)
            free((*ik)->rad_buffs[i]);
         (*ik)->rad_buffs[i] = NULL;
         deleteAndCloseImage(&((*ik)->images[i]));
      }
   }

   // free reflectance buffers
   for(i = 0; i < IK_N_BANDS; i++)
   {
      if((*ik)->ref_buffs[i] != NULL)
         free((*ik)->ref_buffs[i]);
   }

   free((*ik)->rad_buffs);
   free((*ik)->ref_buffs);
   free(*ik);
}

/******************************************************************************/
double IK_getTOARadiance(IK_MANAGER *ik, int band, double dn)
{
   return dn/(IK_COEFFS[band]*IK_BANDWIDTH[band]);
}

/******************************************************************************/
int IK_setTOARadianceLine(IK_MANAGER *ik, int band, int line)
{
   int i;
   VICAR_IMAGE *vi;

   if(ik->curr_line_in_rad_buffs[band] == line) return IK_SUCCESS;

   vi = ik->images[band];
   readVicarImageLine(vi, line);
   for(i = 0; i < vi->ns; i++)
      ik->rad_buffs[band][i] = IK_getTOARadiance(ik, band, vi->buffer[i]);

   ik->curr_line_in_rad_buffs[band] = line;

   return IK_SUCCESS;
}

/******************************************************************************/
int IK_createTOARadianceImage(IK_MANAGER *ik, int outInst, int band)
{
   int i, status;
   VICAR_IMAGE *inp;
   VICAR_IMAGE *out;

   IK_checkPreconditions(ik, band);
   inp = ik->images[band];
   status = zvselpi(outInst);
   out = getVI_out("REAL", outInst, inp->nl, inp->ns);

   // free the default buffer in output VICAR_IMAGE and point it to ik reflectance buffer
   free(out->buffer);
   out->buffer = ik->rad_buffs[band];

   for(i = 0; i < out->nl; i++)
   {
      IK_setTOARadianceLine(ik, band, i);
      writeVicarImageLine(out, i);
   }

   deleteAndCloseImage(&out);

   return IK_SUCCESS;
}

/******************************************************************************/
double IK_getTOAReflectance(IK_MANAGER *ik, int band, double radiance)
{
   return (radiance*pow(ik->solarDist, 2.0)*M_PI)/(IK_ESUN[band]*cos(ik->solarZenithAngleInRadians));
}

/******************************************************************************/
int IK_setTOAReflectanceLine(IK_MANAGER *ik, int band, int line)
{
   int i;
   VICAR_IMAGE *vi;

   if(ik->curr_line_in_ref_buffs[band] == line) return IK_SUCCESS;

   vi = ik->images[band];
   IK_setTOARadianceLine(ik, band, line);
   for(i = 0; i < vi->ns; i++)
      ik->ref_buffs[band][i] = IK_getTOAReflectance(ik, band, ik->rad_buffs[band][i]);

   ik->curr_line_in_ref_buffs[band] = line;

   return IK_SUCCESS;
}

/******************************************************************************/
int IK_createTOAReflectanceImage(IK_MANAGER *ik, int outInst, int band)
{
   int i, status;
   VICAR_IMAGE *inp;
   VICAR_IMAGE *out;

   IK_checkPreconditions(ik, band);
   inp = ik->images[band];
   status = zvselpi(outInst);
   out = getVI_out("REAL", outInst, inp->nl, inp->ns);

   // free the default buffer in output VICAR_IMAGE and point it to ik reflectance buffer
   free(out->buffer);
   out->buffer = ik->ref_buffs[band];

   for(i = 0; i < out->nl; i++)
   {
      IK_setTOAReflectanceLine(ik, band, i);
      writeVicarImageLine(out, i);
   }

   deleteAndCloseImage(&out);

   return IK_SUCCESS;
}

/******************************************************************************/
void IK_print(IK_MANAGER *ik)
{
   int i;

   printf("Metadata:\n");
   printf("=========\n");
   printf("datetime: %lf %lf %lf %lf %lf\n", ik->year, ik->month, ik->day, ik->hh, ik->mm);
   printf("solar elevation/zenith: %lf %lf\n", ik->solarElevation, ik->solarZenithAngle);
   printf("solar distance: %.9lf\n", ik->solarDist);

   for(i = 0; i < IK_N_BANDS; i++)
   {
      printf("-----\n");
      printf("Band: %d metaflag: %d\n", i+1, ik->metaFlag);
   }
}
