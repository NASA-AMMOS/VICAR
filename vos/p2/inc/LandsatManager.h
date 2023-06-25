#ifndef LANDSATMANAGER
#define LANDSATMANAGER

#include <stdio.h>
#include "ImageUtils.h"

#define LANDSAT_N_BANDS          9

                                    // BANDWIDTHS
#define LANDSAT_BLUE_IR         0   // 0.45  - 0.515 micron 
#define LANDSAT_GREEN_IR        1   // 0.525 - 0.605 micron
#define LANDSAT_RED_IR_N        2   // 0.63  - 0.690 micron
#define LANDSAT_NEAR_IR_B       3   // 0.75  - 0.90  micron
#define LANDSAT_SHORTWAVE_IR_1  4   // 1.55  - 1.75  micron
#define LANDSAT_THERMAL_IR_1    5   // 10.40 - 12.5  micron
#define LANDSAT_THERMAL_IR_2    6   // 10.40 - 12.5  micron
#define LANDSAT_SHORTWAVE_IR_2  7   // 2.09  - 2.35  micron
#define LANDSAT_PAN             8   // 0.52  - 0.90  micron

#define LANDSAT_BAND1     0
#define LANDSAT_BAND2     1
#define LANDSAT_BAND3     2
#define LANDSAT_BAND4     3
#define LANDSAT_BAND5     4
#define LANDSAT_BAND61    5
#define LANDSAT_BAND62    6
#define LANDSAT_BAND7     7
#define LANDSAT_BAND8     8

#define LANDSAT_BAND_SET      1
#define LANDSAT_BAND_NOT_SET  0

#define LANDSAT_GAIN_BEFORE   0 // flags to determine which gain
#define LANDSAT_GAIN_AFTER    1 // to use before/after July 1, 2000
#define LANDSAT_GAIN_DATE     2
#define LANDSAT_GAIN_LOW      0
#define LANDSAT_GAIN_HIGH     1
#define LANDSAT_GAIN_LOHI     2
#define LANDSAT_GAIN_LMIN     0
#define LANDSAT_GAIN_LMAX     1
#define LANDSAT_GAIN_MINMAX   2

#define LANDSAT_METABUF_SIZE 1000

#define LANDSAT_N_REF_BANDS  7
#define LANDSAT_N_TIR_BANDS  2
// LANDSAT ERROR CODES

#define LANDSAT_UNINITIALIZED  -999
#define LANDSAT_QCAL_NOT_SET     -6
#define LANDSAT_LMAX_NOT_SET     -5
#define LANDSAT_LMIN_NOT_SET     -4
#define LANDSAT_UNIT_NOT_SET     -3
#define LANDSAT_NO_DATA          -2
#define LANDSAT_NO_METAFILE      -1
#define LANDSAT_SUCCESS           1

/*
static const double LANDSAT_GAIN_COEFFS[LANDSAT_GAIN_DATE]
                                       [LANDSAT_GAIN_LOHI]
                                       [LANDSAT_GAIN_MINMAX]
                                       [LANDSAT_N_BANDS]
                    = {{{{ -6.2,  -6.0,  -4.5,  -4.5, -1.0,   0.0, -0.35,  -5.0},
                         {297.5, 303.4, 235.5, 235.0, 47.7, 17.04,  16.6, 244.0}},
                        {{ -6.2,  -6.0,  -4.5,  -4.5, -1.0,   3.2, -0.35,  -5.0},
                         {194,3, 202.4, 158.6, 157.5,31.76, 12.65,10.932, 158.4}}},
                       {{{ -6.2,  -6.4,  -5.0,  -5.1, -1.0,   0.0, -0.35,  -4.7},
                         {293.7, 300.9, 234.4, 241.1,47.57, 17.04, 16.54, 243.1}},
                        {{ -6.2,  -6.4,  -5.0,  -5.1, -1.0,   3.2, -0.35,  -4.7},
                         {191,6, 196.5, 152.9, 157.4,31.06, 12.65,  10.8, 158.3}}}}
*/

/***************************************************************************/
/* band constants exoatmospheric irradiances bands 1 thru 8                */
/***************************************************************************/
static const double LANDSAT_ESUN[LANDSAT_N_BANDS] =
                       /*BAND1  BAND2  BAND3  BAND4  BAND5 BAND61 BAND62  BAND7  BAND8*/
//                        {1997., 1812., 1533., 1039., 230.8,    0.,    0.,  84.9, 1362.};
                        {1969., 1840., 1551., 1044., 225.7, 0., 0., 82.07, 1368.};

/***************************************************************************/
/* band constants to calculate the brightness temperature                  */
/* not to be confused with at-satellite temperature                        */
/***************************************************************************/
static const double LANDSAT_BTEMP_K1 = 666.09;
static const double LANDSAT_BTEMP_K2 = 1282.71;

typedef struct
{
   VICAR_IMAGE* images[LANDSAT_N_BANDS];
   unsigned char units_set[LANDSAT_N_BANDS];
   double dist;
   double lmin[LANDSAT_N_BANDS];
   double lmax[LANDSAT_N_BANDS];
   double qcalmax[LANDSAT_N_BANDS];
   double qcalmin[LANDSAT_N_BANDS];
   double elevation;
   double **rad_buffs;
   double **ref_buffs;
   double **b_temp_buffs;
   int curr_line_in_rad_buffs[LANDSAT_N_BANDS];
   int curr_line_in_ref_buffs[LANDSAT_N_REF_BANDS];
   int curr_line_in_b_temp_buffs[LANDSAT_N_TIR_BANDS];
}LANDSAT_MANAGER;

/***************************************************************************/
// (deprecated - use getLandsatManager2)
// getLandsatManager: returns a LANDSAT_MANAGER struct
//
// input:
// ======
// + units
//    - opened units for raw dn files
//    - ordered in accordance with LANDSAT_BAND#
//    - uninitialized units set to -1
// + metafile
//    - opened file containing metadata
//
// output:
// =======
// + LANDSAT_MANAGER struct
/***************************************************************************/
LANDSAT_MANAGER* LM_getLandsatManager(int units[LANDSAT_N_BANDS], FILE *metafile);

/***************************************************************************/
// getLandsatManager2: returns a LANDSAT_MANAGER struct
//
// input:
// ======
// + vi
//    - VICAR_IMAGE array
//    - ordered in accordance with LANDSAT_BAND#
//    - uninitialized units set to NULL
// + metafile
//    - opened file containing metadata
//
// output:
// =======
// + LANDSAT_MANAGER struct
/***************************************************************************/
LANDSAT_MANAGER* LM_getLandsatManager2(VICAR_IMAGE *vi[LANDSAT_N_BANDS], FILE *metafile);

/***************************************************************************/
// deleteLandsatManager: deletes a LANDSAT_MANAGER struct and frees buffers
//                       and closes files
//
// input:
// ======
// + **lm
//    - LANDSAT_MANAGER struct to free
/***************************************************************************/
void LM_deleteLandsatManager(LANDSAT_MANAGER **lm);

/***************************************************************************/
// getRadianceLine: calculates the radiance from raw dn image
//
// input:
// ======
// + *lm
//    - LANDSAT_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate radiance for
// + line
//    - line to calculate radiance for
//
// output:
// =======
// + lm->rad_buffs[band]
//    - contains the radiance data inside the rad_buffs
/***************************************************************************/
int LM_getRadianceLine(LANDSAT_MANAGER *lm, int band, int line);

/***************************************************************************/
// createRadianceImage: creates a radiance image for specified band
//
// input:
// ======
// + *lm
//    - LANDSAT_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate radiance for
//
// output:
// =======
// + *vi
//    - VICAR_IMAGE struct containing output file
/***************************************************************************/
int LM_createRadianceImage(LANDSAT_MANAGER *lm, VICAR_IMAGE *vi, int band);

/***************************************************************************/
// getReflectanceLine: calculates the reflectance from raw dn image
//
// input:
// ======
// + *lm
//    - LANDSAT_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate reflectance for
// + line
//    - line to calculate reflectance for
//
// output:
// =======
// + lm->ref_buffs[band]
//    - contains the reflectance data inside the rad_buffs
/***************************************************************************/
int LM_getReflectanceLine(LANDSAT_MANAGER *lm, int band, int line);

/***************************************************************************/
// createReflectanceImage: creates a reflectance image for specified band
//
// input:
// ======
// + *lm
//    - LANDSAT_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate reflectance for
//
// output:
// =======
// + *vi
//    - VICAR_IMAGE struct containing output file
/***************************************************************************/
int LM_createReflectanceImage(LANDSAT_MANAGER *lm, VICAR_IMAGE *vi, int band);

/***************************************************************************/
// getBTempLine: calculates the brightness temp from raw dn image
//
// input:
// ======
// + *lm
//    - LANDSAT_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate brightness temp for
// + line
//    - line to calculate brightness temp for
//
// output:
// =======
// + lm->b_temp_buffs[band]
//    - contains the brightness temp data inside the rad_buffs
/***************************************************************************/
int LM_getBTempLine(LANDSAT_MANAGER *lm, int band, int line);

/***************************************************************************/
// createBTempImage: creates a brightness temp image for specified band
//
// input:
// ======
// + *lm
//    - LANDSAT_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate brightness temp for
//
// output:
// =======
// + *vi
//    - VICAR_IMAGE struct containing output file
/***************************************************************************/
int LM_createBTempImage(LANDSAT_MANAGER *lm, VICAR_IMAGE *vi, int band);

#endif
