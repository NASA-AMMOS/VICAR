#ifndef WV2MANAGER
#define WV2MANAGER

#include <stdio.h>
#include "ImageUtils.h"

#define WV2_N_BANDS          9
#define WV2_MSI_BANDS        8

                                // CENTER WAVELENGTH
#define WV2_COASTAL         0   // 0.427 micron 
#define WV2_BLUE            1   // 0.478 micron
#define WV2_GREEN           2   // 0.546 micron
#define WV2_YELLOW          3   // 0.608 micron
#define WV2_RED             4   // 0.659 micron
#define WV2_RED_EDGE        5   // 0.724 micron
#define WV2_NIR_1           6   // 0.831 micron
#define WV2_NIR_2           7   // 0.902 micron
#define WV2_PAN             8   // 0.632 micron

#define WV2_IS_PAN          1
#define WV2_IS_NOT_PAN      0

#define WV2_BAND1     0
#define WV2_BAND2     1
#define WV2_BAND3     2
#define WV2_BAND4     3
#define WV2_BAND5     4
#define WV2_BAND6     5
#define WV2_BAND7     6
#define WV2_BAND8     7
#define WV2_BAND_PAN  8

#define WV2_UNIT_SET      1
#define WV2_UNIT_NOT_SET  0
#define WV2_META_NOT_SET  0
#define WV2_META_CAL_SET  1
#define WV2_META_BWID_SET 2
#define WV2_META_ALL_SET  3

#define WV2_METABUF_SIZE 10000

// WV2 ERROR CODES

#define WV2_UNINITIALIZED  -999
#define WV2_NO_DATA          -2
#define WV2_NO_METAFILE      -1
#define WV2_SUCCESS           1

/***************************************************************************/
/* band constants solar spectral irradiances bands 1 thru 8 + pan          */
/***************************************************************************/
static const double WV2_ESUN[WV2_N_BANDS] =
                       /*BAND1      BAND2      BAND3      BAND4      BAND5      BAND6      BAND7      BAND8     PANBAND*/
                        {1758.2229, 1974.2416, 1856.4104, 1738.4791, 1559.4555, 1342.0695, 1069.7302, 861.2866, 1580.8140};

/***************************************************************************/ 	 	 
/* lowerbound bandwiths for the bands in micrometers 5% Band Pass          */ 	 	 
/***************************************************************************/ 	 	 
static const double WV2_LBW[WV2_N_BANDS] =  	 	 
                       /*BAND1  BAND2  BAND3  BAND4  BAND5  BAND6  BAND7  BAND8  PANBAND*/ 	 	 
                       {0.396, 0.442, 0.506, 0.584, 0.624, 0.699, 0.765, 0.856, 0.447}; 	 	 
	 	 	 
/***************************************************************************/ 	 	 
/* upperbound bandwiths for the bands in micrometers 5% Band Pass          */ 	 	 
/***************************************************************************/ 	 	 
static const double WV2_UBW[WV2_N_BANDS] =  	 	 
                       /*BAND1  BAND2  BAND3  BAND4  BAND5  BAND6  BAND7  BAND8  PANBAND*/ 	 	 
                       {0.458, 0.515, 0.586, 0.632, 0.694, 0.749, 0.901, 1.043, 0.808}; 

typedef struct
{
   VICAR_IMAGE* images[WV2_N_BANDS];
   unsigned char units_set[WV2_N_BANDS];
   double **rad_buffs;
   double **ref_buffs;
   double **radLookupTable;
   double **refLookupTable;
   int curr_line_in_rad_buffs[WV2_N_BANDS];
   int curr_line_in_ref_buffs[WV2_N_BANDS];

   /* Metadata */
   double year;
   double month;
   double day;
   double hh;
   double mm;
   double ssdd;
   double solarElevation;
   double solarZenithAngle;
   double solarZenithAngleInRadians;
   double solarDist;

   double pan_ssdd;
   double pan_solarElevation;
   double pan_solarZenithAngle;
   double pan_solarZenithAngleInRadians;
   double pan_solarDist;

   double absCalFactor[WV2_N_BANDS];
   double effectiveBandwidth[WV2_N_BANDS];

   /* flags set if calibration factor and bandwidth are set from metadata */
   /* WV2_META_NOT_SET  - non are set */
   /* WV2_META_CAL_SET  - calibration factor was set */
   /* WV2_META_BWID_SET - bandwidth was set */
   /* WV2_META_ALL_SET  - both calibration factor and bandwidth were set */
   unsigned char metaFlags[WV2_N_BANDS];
}WV2_MANAGER;

/***************************************************************************/
// WV2_getWV2Manager: returns a WV2_MANAGER struct
//
// input:
// ======
// + vi
//    - VICAR_IMAGE array
//    - ordered in accordance with WV2_BAND#
//    - !! uninitialized units must be set to NULL !!
// + metafname
//    - path and filename of .IMD metafile for multi bands
//    - pass in NULL if not doing multi bands
// + panMetaFname
//    - path and filename of .IMD metafile for pan band
//    - pass in NULL if not doing pan band
//
// output:
// =======
// + WV2_MANAGER struct
/***************************************************************************/
WV2_MANAGER* WV2_getWV2Manager(VICAR_IMAGE *vi[WV2_N_BANDS], char* metafname, char* panMetaFname);

/***************************************************************************/
// WV2_deleteWV2Manager: deletes a WV2_MANAGER struct and frees buffers
//                       and closes files
//
// input:
// ======
// + **wv2
//    - WV2_MANAGER struct to free
/***************************************************************************/
void WV2_deleteWV2Manager(WV2_MANAGER **wv2);

/***************************************************************************/
// WV2_setTOARadianceLine: calculates the radiance from raw dn image
//
// input:
// ======
// + *wv2
//    - WV2_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate radiance for
// + line
//    - line to calculate radiance for
//
// output:
// =======
// + wv2m->rad_buffs[band]
//    - contains the radiance data inside the rad_buffs
// + status
//    - WV2_SUCCESS
/***************************************************************************/
int WV2_setTOARadianceLine(WV2_MANAGER *wv2, int band, int line);

/***************************************************************************/
// WV2_createTOARadianceImage: creates a radiance image for specified band
//
// input:
// ======
// + *wv2
//    - WV2_MANAGER struct with the band raw dn file set
// + outInst
//    - in out argument, the out instance of output file
// + band
//    - band to calculate radiance for
//
// output:
// =======
// + *vi
//    - VICAR_IMAGE struct containing output file
/***************************************************************************/
int WV2_createTOARadianceImage(WV2_MANAGER *wv2, int outInst, int band);

/***************************************************************************/
// WV2_setTOAReflectanceLine: calculates the reflectance from raw dn image
//
// input:
// ======
// + *wv2
//    - WV2_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate reflectance for
// + line
//    - line to calculate reflectance for
//
// output:
// =======
// + wv2m->ref_buffs[band]
//    - contains the reflectance data inside the rad_buffs
// + status
//    - WV2_SUCCESS
/***************************************************************************/
int WV2_setTOAReflectanceLine(WV2_MANAGER *wv2, int band, int line);

/***************************************************************************/
// WV2_createTOAReflectanceImage: creates a reflectance image for specified band
//
// input:
// ======
// + *wv2
//    - WV2_MANAGER struct with the band raw dn file set
// + outInst
//    - int out argument, the out instance of the output file
// + band
//    - band to calculate reflectance for
//
// output:
// =======
// + *vi
//    - VICAR_IMAGE struct containing output file
/***************************************************************************/
int WV2_createTOAReflectanceImage(WV2_MANAGER *wv2, int outInst, int band);

/***************************************************************************/
// WV2_print: prints out given wv2 struct to screen
//
// input:
// ======
// + *wv2
//    - WV2_MANAGER struct with the band raw dn file set
//
// output:
// =======
// + prints out to screen
/***************************************************************************/
void WV2_print(WV2_MANAGER *wv2);

#endif
