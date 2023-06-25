#include <math.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <zvproto.h>
#include "ImageUtils.h"
#include "AsterManager.h"

/******************************************************************************/
int AM_checkAsterCommonPreconditions(ASTER_MANAGER *am, int band)
{
  if(!am->units_set[band]) return ASTER_UNIT_NOT_SET_ERR;
  if(ASTER_GAIN_COEFFS[band][am->gain[band]] < 10E-10)
    return ASTER_GAIN_NOT_SET_ERR;

  return ASTER_SUCCESS;
}

/******************************************************************************/
double AM_getEarthSunDist(int julDate)
{
  double eccentricity, distance;

  eccentricity = 0.016710219;
  distance = 1/((1 + eccentricity*cos((julDate - 4)*2*M_PI/365.25))/(1-pow(eccentricity, 2)));

  return distance;
}

/******************************************************************************/
int AM_getDayOfYear(int year, int month, int day)
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
int AM_fillMetadata(ASTER_MANAGER *am, FILE *metafile)
{
  char metabuf[ASTER_METABUF_SIZE];
  int year, month, day, i, dayOfYear;
  char buff[2000];

  if(metafile == NULL) return ASTER_NO_METAFILE;

  for(i = 0; i < ASTER_N_BANDS; i++)
    am->gain[i] = ASTER_GAIN_INIT;
  year = month = day = 0;

  fseek(metafile, 0, SEEK_SET);
  while(fgets(metabuf, ASTER_METABUF_SIZE, metafile) != NULL)
    {
      char *p, dum, label[30];

      p = strstr(metabuf, "ASTER_CALENDARDATE");
      if(p != NULL)
	{
	  p = strstr(metabuf, "=");
	  assert(p != NULL);
	  p++;
	  sscanf(p, "%c%d%c%d%c%d", &dum, &year, &dum, &month, &dum, &day);

	  continue;
	}
      p = strstr(metabuf, "ASTER_SOLAR_ELEVATION");
      if(p != NULL)
	{
	  p = strstr(metabuf, "=");
	  assert(p != NULL);
	  p++;
	  sscanf(p, "%lf", &am->elevation);

	  continue;
	}

      for(i = 0; i < ASTER_N_VNIR_BANDS + ASTER_N_SWIR_BANDS; i++)
	{
	  sprintf(label, "GAIN_INFORMATION_%d=", i+1);
	  p = strstr(metabuf, label);
	  if(p != NULL)
	    {
	      p = strstr(metabuf, "HGH");
	      if(p != NULL)
		{
		  am->gain[i] = ASTER_GAIN_HIGH;
		  break;
		}
	      p = strstr(metabuf, "NOR");
	      if(p != NULL)
		{
		  am->gain[i] = ASTER_GAIN_NORMAL;
		  break;
		}
	      p = strstr(metabuf, "LO1");
	      if(p != NULL)
		{
		  am->gain[i] = ASTER_GAIN_LOW1;
		  break;
		}
	      p = strstr(metabuf, "LO2");
	      if(p != NULL)
		{
		  am->gain[i] = ASTER_GAIN_LOW2;
		  break;
		}
	    }
	}
    }
  am->gain[ASTER_BAND10] = ASTER_GAIN_NORMAL;
  am->gain[ASTER_BAND11] = ASTER_GAIN_NORMAL;
  am->gain[ASTER_BAND12] = ASTER_GAIN_NORMAL;
  am->gain[ASTER_BAND13] = ASTER_GAIN_NORMAL;
  am->gain[ASTER_BAND14] = ASTER_GAIN_NORMAL;

  for(i = 0; i < ASTER_N_BANDS; i++)
    if(am->gain[i] == ASTER_GAIN_INIT) return ASTER_NO_GAIN;

  if(!year && !month && !year) return ASTER_NO_DATE_DATA;

  dayOfYear = AM_getDayOfYear(year, month, day);
  am->dist = AM_getEarthSunDist(dayOfYear);

  sprintf(buff, "Extracted from metafile."); zifmessage(buff);
  sprintf(buff, "========================"); zifmessage(buff);
  sprintf(buff, "Calendar date: %d %d %d", year, month, day); zifmessage(buff);
  sprintf(buff, "Elevation:     %f", am->elevation); zifmessage(buff);
  for(i = 0; i < ASTER_N_BANDS; i++) {
    sprintf(buff, "Gain %d: %d", i+1, am->gain[i]);
    zifmessage(buff);
  }
  sprintf(buff, "day of year: %d", dayOfYear); zifmessage(buff);
  sprintf(buff, "Earth Sun Dist: %f", am->dist); zifmessage(buff);

  sprintf(buff, "Acquired data from metafile..."); zifmessage(buff);

  return ASTER_SUCCESS;
}

/******************************************************************************/
ASTER_MANAGER* AM_getAsterManager(int units[ASTER_N_BANDS], FILE *metafile)
{
  int i, j, status;
  ASTER_MANAGER *am;
  char buff[2000];

  am = (ASTER_MANAGER*)malloc(sizeof(ASTER_MANAGER));

  status = AM_fillMetadata(am, metafile);
  switch(status)
    {
    case ASTER_NO_METAFILE:
      sprintf(buff, "Metafile not found."); zifmessage(buff);
      zabend();
    case ASTER_NO_GAIN:
      sprintf(buff, "Error while attaining gain info."); zifmessage(buff);
      zabend();
    case ASTER_NO_DATE_DATA:
      sprintf(buff, "Error while attaining date info."); zifmessage(buff);
      zabend();
    }

  for(i = 0; i < ASTER_N_BANDS; i++)
    {
      if(units[i] > -1)
	{
	  am->images[i] = getImage(units[i]);
	  am->units_set[i] = ASTER_BAND_SET;
	}
      else
	am->units_set[i] = ASTER_BAND_NOT_SET;
    }

  am->rad_buffs = (double**)malloc(sizeof(double*)*ASTER_N_BANDS);
  am->ref_buffs = (double**)malloc(sizeof(double*)*(ASTER_N_REF_BANDS));
  am->b_temp_buffs = (double**)malloc(sizeof(double*)*ASTER_N_TIR_BANDS);
  for(i = 0; i < ASTER_N_BANDS; i++)
    {
      if(am->units_set[i] == ASTER_BAND_SET)
	{
	  am->rad_buffs[i] = (double*)calloc(am->images[i]->ns, sizeof(double));
	  am->curr_line_in_rad_buffs[i] = -1;
	}
    }
  for(i = 0; i < ASTER_N_REF_BANDS; i++)
    {
      if(am->units_set[i] == ASTER_BAND_SET)
	{
	  am->ref_buffs[i] = (double*)calloc(am->images[i]->ns, sizeof(double));
	  am->curr_line_in_ref_buffs[i] = -1;
	}
    }
  for(j = 0; j < ASTER_N_TIR_BANDS; j++)
    {
      if(am->units_set[i] == ASTER_BAND_SET)
	{
	  am->b_temp_buffs[j] = (double*)calloc(am->images[i]->ns, sizeof(double));
	  am->curr_line_in_b_temp_buffs[j] = -1;
	}
      ++i;
    }

  return am;
}

/******************************************************************************/
void AM_deleteAsterManager(ASTER_MANAGER *am)
{
  int i, j;

  for(i = 0; i < ASTER_N_BANDS; i++)
    {
      if(am->units_set[i] == ASTER_BAND_SET)
	{
	  if(am->rad_buffs[i] != NULL)
            free(am->rad_buffs[i]);
	  am->rad_buffs[i] = NULL;
	  deleteAndCloseImage(&(am->images[i]));
	}
    }
  for(i = 0; i < ASTER_N_REF_BANDS; i++)
    {
      if(am->units_set[i] == ASTER_BAND_SET)
	{
	  if(am->ref_buffs[i] != NULL)
            free(am->ref_buffs[i]);
	  am->ref_buffs[i] = NULL;
	}
    }
  for(j = 0; j < ASTER_N_TIR_BANDS; j++)
    {
      if(am->units_set[i] == ASTER_BAND_SET)
	{
	  if(am->b_temp_buffs[j] != NULL)
            free(am->b_temp_buffs[j]);
	  am->b_temp_buffs[j] = NULL;
	}
      ++i;
    }

  free(am->rad_buffs);
  free(am->ref_buffs);
  free(am->b_temp_buffs);
  free(am);
}

/******************************************************************************/
int AM_getRadianceLine(ASTER_MANAGER *am, int band, int line)
{
  int i, err;

  // check gain coefficient is not 0 and band is set
  err = AM_checkAsterCommonPreconditions(am, band);
  if(err != ASTER_SUCCESS) return err;
  if(am->curr_line_in_rad_buffs[band] == line) return ASTER_SUCCESS;

  readVicarImageLine(am->images[band], line);
  for(i = 0; i < am->images[band]->ns; i++)
    {
      double pixel;

      pixel = am->images[band]->buffer[i];
      if(fabs(pixel) < 10E-10) am->rad_buffs[band][i] = 0.0;
      else am->rad_buffs[band][i] = ASTER_GAIN_COEFFS[band][am->gain[band]]*(pixel - 1);
    }
  am->curr_line_in_rad_buffs[band] = line;

  return ASTER_SUCCESS;
}

/******************************************************************************/
void AM_createRadianceImage(ASTER_MANAGER *am, VICAR_IMAGE *vi, int band)
{
  int i;

  for(i = 0; i < vi->nl; i++)
    {
      assert(AM_getRadianceLine(am, band, i) == ASTER_SUCCESS);
      memcpy(vi->buffer, am->rad_buffs[band], vi->ns*sizeof(double));
      writeVicarImageLine(vi, i);
    }
}

/******************************************************************************/
int AM_getReflectanceLine(ASTER_MANAGER *am, int band, int line)
{
  int i, err;

  // check gain coefficient is not 0 and band is set
  err = AM_checkAsterCommonPreconditions(am, band);
  if(err != ASTER_SUCCESS) return err;
  if(am->curr_line_in_ref_buffs[band] == line) return ASTER_SUCCESS;

  AM_getRadianceLine(am, band, line);
  for(i = 0; i < am->images[band]->ns; i++)
    {
      double rad;

      rad = am->rad_buffs[band][i];
      if(fabs(rad) < 10E-10) am->ref_buffs[band][i] = 0.0;
      else am->ref_buffs[band][i] = am->rad_buffs[band][i]*pow(am->dist, 2.0)/
	(ASTER_ESUN[band]*cos((90-am->elevation)*M_PI/180))*M_PI;
    }
  am->curr_line_in_ref_buffs[band] = line;

  return ASTER_SUCCESS;
}

/******************************************************************************/
void AM_createReflectanceImage(ASTER_MANAGER *am, VICAR_IMAGE *vi, int band)
{
  int i;

  for(i = 0; i < vi->nl; i++)
    {
      assert(AM_getReflectanceLine(am, band, i) == ASTER_SUCCESS);
      memcpy(vi->buffer, am->ref_buffs[band], vi->ns*sizeof(double));
      writeVicarImageLine(vi, i);
    }
}

/******************************************************************************/
int AM_getBTempLine(ASTER_MANAGER *am, int band, int line)
{
  int i, bTempBand, err;

  bTempBand = band - ASTER_N_REF_BANDS;
  err = AM_checkAsterCommonPreconditions(am, band);
  if(err != ASTER_SUCCESS) return err;
  if(am->curr_line_in_b_temp_buffs[bTempBand] == line) return ASTER_SUCCESS;

  AM_getRadianceLine(am, band, line);
  for(i = 0; i < am->images[band]->ns; i++)
    {
      double logparm;

      if(fabs(am->images[band]->buffer[i]) < 10E-10)
	{
	  am->b_temp_buffs[bTempBand][i] = 0.0;
	  continue;
	}

      logparm = log(ASTER_BTEMP_CONST1/
		    (pow(ASTER_BTEMP_WD[bTempBand], 5.)*M_PI*am->rad_buffs[band][i])+1);
      if(logparm <= 0)
	am->b_temp_buffs[bTempBand][i] = 0.0;
      else
	am->b_temp_buffs[bTempBand][i] = ASTER_BTEMP_CONST2/(ASTER_BTEMP_WD[bTempBand]*logparm);
    }

  return ASTER_SUCCESS;
}

/******************************************************************************/
void AM_createBTempImage(ASTER_MANAGER *am, VICAR_IMAGE *vi, int band)
{
  int i, bTempBand;

  bTempBand = band - ASTER_N_REF_BANDS;
  for(i = 0; i < vi->nl; i++)
    {
      assert(AM_getBTempLine(am, band, i) == ASTER_SUCCESS);
      memcpy(vi->buffer, am->b_temp_buffs[bTempBand], vi->ns*sizeof(double));
      writeVicarImageLine(vi, i);
    }
}
