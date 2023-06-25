#ifndef IKMANAGER
#define IKMANAGER

#include <stdio.h>
#include "ImageUtils.h"

#define IK_N_BANDS          5
#define IK_MSI_BANDS        4

                               // SPECTRAL RANGE
#define IK_BLUE            1   // 0.450-0.530 micron
#define IK_GREEN           2   // 0.520-0.610 micron
#define IK_RED             3   // 0.640-0.720 micron
#define IK_NIR             4   // 0.770-0.880 micron
#define IK_PAN             5   // 0.450-0.900 micron

#define IK_BAND1     0
#define IK_BAND2     1
#define IK_BAND3     2
#define IK_BAND4     3
#define IK_BAND_PAN  4

#define IK_UNIT_SET        1
#define IK_UNIT_NOT_SET    0
#define IK_META_NOT_SET    0
#define IK_META_ALL_SET    1

#define IK_METABUF_SIZE 10000

// IK ERROR CODES

#define IK_UNINITIALIZED  -999
#define IK_NO_DATA          -2
#define IK_NO_METAFILE      -1
#define IK_SUCCESS           1

/***************************************************************************/
/* band constants solar spectral irradiances bands 1 thru 4 + pan          */
/***************************************************************************/
                                                /*BAND1   BAND2   BAND3   BAND4   PANBAND*/
static const double IK_ESUN[IK_N_BANDS]      = {1930.9,  1854.8,  1556.5,  1156.9,  1375.8};
static const double IK_BANDWIDTH[IK_N_BANDS] = {0.0713,    0.0886,   0.0658,    0.0954,    0.403};
static const double IK_COEFFS[IK_N_BANDS]    = {72.8,    72.7,    94.9,    84.3,    16.1};

/***************************************************************************/ 	 	 
/* lowerbound bandwiths for the bands in micrometers 5% Band Pass          */ 	 	 
/***************************************************************************/ 	 	 
static const double WV2_LBW[5] =  	 	 
                       /*BAND1  BAND2  BAND3  BAND4  PAN*/
                       {0.445,   0.506,  0.632,  0.757,  0.45};
	 	 	 
/***************************************************************************/ 	 	 
/* upperbound bandwiths for the bands in micrometers 5% Band Pass          */ 	 	 
/***************************************************************************/ 	 	 
static const double WV2_UBW[5] =  	 	 
                       /*BAND1  BAND2  BAND3  BAND4  PANBAND*/ 	 	 
                       {0.516,   0.595,  0.698,  0.853,  0.9}; 

typedef struct
{
   VICAR_IMAGE* images[IK_N_BANDS];
   unsigned char units_set[IK_N_BANDS];
   double **rad_buffs;
   double **ref_buffs;
   int curr_line_in_rad_buffs[IK_N_BANDS];
   int curr_line_in_ref_buffs[IK_N_BANDS];

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

   /* flag set if date and solar elevation angle are set from metadata */
   /* IK_META_NOT_SET  - none are set */
   /* IK_META_ALL_SET  - both calibration factor and bandwidth were set */
   unsigned char metaFlag;
}IK_MANAGER;

/***************************************************************************/
// IK_getIKManager: returns a IK_MANAGER struct
//
// input:
// ======
// + vi
//    - VICAR_IMAGE array
//    - ordered in accordance with IK_BAND#
//    - !! uninitialized units must be set to NULL !!
// + metafname
//    - path and filename of .pvl metafile
//
// output:
// =======
// + IK_MANAGER struct
/***************************************************************************/
IK_MANAGER* IK_getIKManager(VICAR_IMAGE *vi[IK_N_BANDS], char* metafname);

/***************************************************************************/
// IK_deleteIKManager: deletes a IK_MANAGER struct and frees buffers
//                       and closes files
//
// input:
// ======
// + **ik
//    - IK_MANAGER struct to free
/***************************************************************************/
void IK_deleteIKManager(IK_MANAGER **ik);

/***************************************************************************/
// IK_setTOARadianceLine: calculates the radiance from raw dn image
//
// input:
// ======
// + *ik
//    - IK_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate radiance for
// + line
//    - line to calculate radiance for
//
// output:
// =======
// + ik->rad_buffs[band]
//    - contains the radiance data inside the rad_buffs
// + status
//    - IK_SUCCESS
/***************************************************************************/
int IK_setTOARadianceLine(IK_MANAGER *ik, int band, int line);

/***************************************************************************/
// IK_createTOARadianceImage: creates a radiance image for specified band
//
// input:
// ======
// + *ik
//    - IK_MANAGER struct with the band raw dn file set
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
int IK_createTOARadianceImage(IK_MANAGER *ik, int outInst, int band);

/***************************************************************************/
// IK_setTOAReflectanceLine: calculates the reflectance from raw dn image
//
// input:
// ======
// + *ik
//    - IK_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate reflectance for
// + line
//    - line to calculate reflectance for
//
// output:
// =======
// + ik->ref_buffs[band]
//    - contains the reflectance data inside the rad_buffs
// + status
//    - IK_SUCCESS
/***************************************************************************/
int IK_setTOAReflectanceLine(IK_MANAGER *ik, int band, int line);

/***************************************************************************/
// IK_createTOAReflectanceImage: creates a reflectance image for specified band
//
// input:
// ======
// + *ik
//    - IK_MANAGER struct with the band raw dn file set
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
int IK_createTOAReflectanceImage(IK_MANAGER *ik, int outInst, int band);

/***************************************************************************/
// IK_print: prints out given ik struct to screen
//
// input:
// ======
// + *ik
//    - IK_MANAGER struct with the band raw dn file set
//
// output:
// =======
// + prints out to screen
/***************************************************************************/
void IK_print(IK_MANAGER *ik);

#endif
