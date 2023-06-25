#ifndef ASTERMANAGER
#define ASTERMANAGER

#include <stdio.h>
#include "ImageUtils.h"

                                    // BANDWIDTHS
#define ASTER_GREEN_IR        0     // 0.52  - 0.60  micron 
#define ASTER_RED_IR          1     // 0.63  - 0.69  micron
#define ASTER_NEAR_IR_N       2     // 0.78  - 0.86  micron
#define ASTER_NEAR_IR_B       3     // 0.78  - 0.86  micron
#define ASTER_SHORTWAVE_IR_1  4     // 1.60  - 1.70  micron
#define ASTER_SHORTWAVE_IR_2  5     // 2.145 - 2.185 micron
#define ASTER_SHORTWAVE_IR_3  6     // 2.185 - 2.225 micron
#define ASTER_SHORTWAVE_IR_4  7     // 2.235 - 2.285 micron
#define ASTER_SHORTWAVE_IR_5  8     // 2.295 - 2.365 micron
#define ASTER_SHORTWAVE_IR_6  9     // 2.360 - 2.430 micron
#define ASTER_THERMAL_IR_1    10    // 8.125 - 8.475 micron
#define ASTER_THERMAL_IR_2    11    // 8.475 - 8.825 micron
#define ASTER_THERMAL_IR_3    12    // 8.925 - 9.275 micron
#define ASTER_THERMAL_IR_4    13    // 10.25 - 10.95 micron
#define ASTER_THERMAL_IR_5    14    // 10.95 - 11.65 micron

#define ASTER_BAND1     0
#define ASTER_BAND2     1
#define ASTER_BAND3N    2
#define ASTER_BAND3B    3
#define ASTER_BAND4     4
#define ASTER_BAND5     5
#define ASTER_BAND6     6
#define ASTER_BAND7     7
#define ASTER_BAND8     8
#define ASTER_BAND9     9
#define ASTER_BAND10    10
#define ASTER_BAND11    11
#define ASTER_BAND12    12
#define ASTER_BAND13    13
#define ASTER_BAND14    14

#define ASTER_GAIN_LOW1   3
#define ASTER_GAIN_LOW2   2
#define ASTER_GAIN_NORMAL 1
#define ASTER_GAIN_HIGH   0
#define ASTER_GAIN_INIT   -1

#define ASTER_BAND_SET        1
#define ASTER_BAND_NOT_SET    0

#define ASTER_N_VNIR_BANDS  4
#define ASTER_N_SWIR_BANDS  6
#define ASTER_N_TIR_BANDS   5
#define ASTER_N_REF_BANDS   10
#define ASTER_N_BANDS       15

#define ASTER_METABUF_SIZE 1000
// ASTER ERROR CODES

#define ASTER_NO_DATE_DATA     -6
#define ASTER_NO_ELEV_DATA     -5
#define ASTER_NO_GAIN          -4
#define ASTER_NO_METAFILE      -3
#define ASTER_UNIT_NOT_SET_ERR -2
#define ASTER_GAIN_NOT_SET_ERR -1
#define ASTER_SUCCESS           1

// VNIR - resolution 4200NL X 4980NS = 20,916,000 pixels (BYTE)
// SWIR - resolution 2100NL X 2490NS = 5,229,000  pixels (BYTE)
// TIR  - resolution 700 NL X 830 NS = 581,000    pixels (HALF)

static const double ASTER_GAIN_COEFFS[ASTER_N_BANDS][4]
                    = {{0.676,  1.688,    2.25,   0.0},    // BAND 1
                       {0.708,  1.415,    1.89,   0.0},    // BAND 2
                       {0.423,  0.862,    1.15,   0.0},    // BAND 3N
                       {0.423,  0.862,    1.15,   0.0},    // BAND 3B
                       {0.1087, 0.2174,   0.290,  0.290},  // BAND 4
                       {0.0348, 0.0696,   0.0925, 0.409},  // BAND 5
                       {0.0313, 0.0625,   0.0830, 0.3900}, // BAND 6
                       {0.0299, 0.0597,   0.0795, 0.3320}, // BAND 7
                       {0.0209, 0.0417,   0.0556, 0.2450}, // BAND 8
                       {0.0159, 0.0318,   0.0424, 0.2650}, // BAND 9
                       {0.0,    0.0,      0.0,    0.0},    // BAND 10 UNKNOWN
                       {0.0,    0.0,      0.0,    0.0},    // BAND 11 UNKNOWN
                       {0.0,    0.0,      0.0,    0.0},    // BAND 12 UNKNOWN
                       {0.0,    5.693E-3, 0.0,    0.0},    // BAND 13
                       {0.0,    5.225E-3, 0.0,    0.0}};   // BAND 14

/***************************************************************************/
/* band constants exoatmospheric irradiances bands 1 thru 5                */
/* DATA ACQUIRED FROM http://www.cnrhome.uidaho.edu/default.aspx?pid=85984 */
/* ** changed to match MATLAB version "Landsat-5 values in Markham **      */
/***************************************************************************/
/*                          BANDS =   1     2     3N    3B    4     5      */
static const double ASTER_ESUN[6] = {1826, 1554, 1036, 1036, 215, 80.67};

/***************************************************************************/
/* band constants to calculate the brightness temperature                  */
/* not to be confused with at-satellite temperature                        */
/***************************************************************************/
static const double ASTER_BTEMP_WD[ASTER_N_TIR_BANDS]
/*            BANDS =    10      11      12      13       14               */
                    = {8.3E-6, 8.6E-6, 9.1E-6, 10.6E-6, 11.3E-6};
static const double ASTER_BTEMP_CONST1 = 3.741775E-22;
static const double ASTER_BTEMP_CONST2 = 0.0143877;

typedef struct
{
   VICAR_IMAGE* images[ASTER_N_BANDS];
   unsigned char units_set[ASTER_N_BANDS];
   double dist;
   int gain[ASTER_N_BANDS];
   double elevation;
   double **rad_buffs;
   double **ref_buffs;
   double **b_temp_buffs;
   int curr_line_in_rad_buffs[ASTER_N_BANDS];
   int curr_line_in_ref_buffs[ASTER_N_REF_BANDS];
   int curr_line_in_b_temp_buffs[ASTER_N_TIR_BANDS];
}ASTER_MANAGER;

/***************************************************************************/
// getAsterManager: returns a ASTER_MANAGER struct
//
// input:
// ======
// + units
//    - opened units for raw dn files
//    - ordered in accordance with ASTER_BAND#
//    - uninitialized units set to -1
// + metafile
//    - opened file containing metadata
//
// output:
// =======
// + ASTER_MANAGER struct
/***************************************************************************/
ASTER_MANAGER* AM_getAsterManager(int units[ASTER_N_BANDS], FILE *metafile);

/***************************************************************************/
// deleteAsterManager: deletes a ASTER_MANAGER struct and frees buffers
//                       and closes files
//
// input:
// ======
// + **am
//    - ASTER_MANAGER struct to free
/***************************************************************************/
void AM_deleteAsterManager(ASTER_MANAGER *am);

/***************************************************************************/
// getRadianceLine: calculates the radiance from raw dn image
//
// input:
// ======
// + *am
//    - ASTER_MANAGER struct with the band raw dn file set
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
int AM_getRadianceLine(ASTER_MANAGER *am, int band, int line);

/***************************************************************************/
// createRadianceImage: creates a radiance image for specified band
//
// input:
// ======
// + *am
//    - ASTER_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate radiance for
//
// output:
// =======
// + *vi
//    - VICAR_IMAGE struct containing output file
/***************************************************************************/
void AM_createRadianceImage(ASTER_MANAGER *am, VICAR_IMAGE *vi, int band);

/***************************************************************************/
// getReflectanceLine: calculates the reflectance from raw dn image
//
// input:
// ======
// + *am
//    - ASTER_MANAGER struct with the band raw dn file set
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
int AM_getReflectanceLine(ASTER_MANAGER *am, int band, int line);

/***************************************************************************/
// createReflectanceImage: creates a reflectance image for specified band
//
// input:
// ======
// + *am
//    - ASTER_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate reflectance for
//
// output:
// =======
// + *vi
//    - VICAR_IMAGE struct containing output file
/***************************************************************************/
void AM_createReflectanceImage(ASTER_MANAGER *am, VICAR_IMAGE *vi, int band);

/***************************************************************************/
// getBTempLine: calculates the brightness temp from raw dn image
//
// input:
// ======
// + *am
//    - ASTER_MANAGER struct with the band raw dn file set
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
int AM_getBTempLine(ASTER_MANAGER *am, int band, int line);

/***************************************************************************/
// createBTempImage: creates a brightness temp image for specified band
//
// input:
// ======
// + *am
//    - ASTER_MANAGER struct with the band raw dn file set
// + band
//    - band to calculate brightness temp for
//
// output:
// =======
// + *vi
//    - VICAR_IMAGE struct containing output file
/***************************************************************************/
void AM_createBTempImage(ASTER_MANAGER *am, VICAR_IMAGE *vi, int band);

#endif
