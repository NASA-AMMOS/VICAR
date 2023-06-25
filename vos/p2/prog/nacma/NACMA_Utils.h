#ifndef NACMA_UTILS
#define NACMA_UTILS

#define NACMA_BANDS_USED  7
#define NACMA_REF_UNITS   5
#define NACMA_MAX_FNAME   100

#include "cloud_masks.h"

typedef struct
{
   FILE *metafile;

   ASTER_MANAGER *am;

   int CMcloudUnit;
   int CMsnowUnit;
   int CMdesertUnit;
   int CMcloud_warmUnit;
   int CMcloud_coldUnit;
   int iceUnit;
   int filter_cirrusUnit;
   int ambigUnit;
   int validUnit;

   int filter1Unit;
   int filter2Unit;
   int filter3Unit;
   int filter4Unit;
   int filter5Unit;
   int filter6Unit;
   int filter7Unit;
   int filter8Unit;

   VICAR_IMAGE *rad1Image;
   VICAR_IMAGE *rad2Image;
   VICAR_IMAGE *rad3Image;
   VICAR_IMAGE *rad4Image;
   VICAR_IMAGE *rad5Image;
   VICAR_IMAGE *rad13Image;
   VICAR_IMAGE *rad14Image;

   VICAR_IMAGE *ref1Image;
   VICAR_IMAGE *ref2Image;
   VICAR_IMAGE *ref3Image;
   VICAR_IMAGE *ref4Image;
   VICAR_IMAGE *ref5Image;
   VICAR_IMAGE *bTemp13Image;
   VICAR_IMAGE *bTemp14Image;

   VICAR_RESAMPLE_IMAGE *ref1dsImage;
   VICAR_RESAMPLE_IMAGE *ref2dsImage;
   VICAR_RESAMPLE_IMAGE *ref3dsImage;
   VICAR_RESAMPLE_IMAGE *ref4dsImage;
   VICAR_RESAMPLE_IMAGE *ref5dsImage;

   VICAR_IMAGE *tempCompImage;
   VICAR_IMAGE *ndsiImage;
   VICAR_IMAGE *gvImage;
   VICAR_IMAGE *svImage;
   VICAR_IMAGE *rsImage;
}NACMA_STRUCT;

/******************************************************************************/
// getNACMA: returns an initialized NACMA struct
//
// output:
// =======
// + NACMA_STRUCT pointer
/******************************************************************************/
NACMA_STRUCT* getNACMA();

/******************************************************************************/
// deleteNACMA: deletes a given NACMA struct
//              frees buffers and closes all files
//
// input:
// ======
// + *nacma
//    - NACMA_STRUCT to delete
/******************************************************************************/
void deleteNACMA(NACMA_STRUCT *nacma);

/******************************************************************************/
// createRadianceFiles: creates radiance files for all raw dn files
//
// input:
// ======
// + *nacma
//    - NACMA_STRUCT to containing raw dn images
/******************************************************************************/
void createRadianceFiles(NACMA_STRUCT *nacma);

/******************************************************************************/
// createReflectanceFiles: creates reflectance files for VNIR/SWIR raw dn files
//
// input:
// ======
// + *nacma
//    - NACMA_STRUCT to containing raw dn images
/******************************************************************************/
void createReflectanceFiles(NACMA_STRUCT *nacma);

/******************************************************************************/
// createBTempFiles: creates brightness temperature files for TIR raw dn files
//
// input:
// ======
// + *nacma
//    - NACMA_STRUCT to containing raw dn images
/******************************************************************************/
void createBTempFiles(NACMA_STRUCT *nacma);

/******************************************************************************/
// createDownsampledFiles: creates downsampled files for reflectance files
//
// input:
// ======
// + *nacma
//    - NACMA_STRUCT to containing reflectance files
/******************************************************************************/
void createDownSampledFiles(NACMA_STRUCT *nacma);

/******************************************************************************/
// reopenReflectanceFiles: reopens reflectance files for reading
//
// input:
// ======
// + *nacma
//    - NACMA_STRUCT to containing reflectance files
/******************************************************************************/
void reopenReflectanceFiles(NACMA_STRUCT *nacma);

/******************************************************************************/
// reopenFilesForPass1: changes reflectance file pointers to downsampled
//                      reflectance files
//
// input:
// ======
// + *nacma
//    - NACMA_STRUCT to containing reflectance files
/******************************************************************************/
void reopenFilesForPass1(NACMA_STRUCT *nacma);

/******************************************************************************/
// createMaskFiles: creates mask files from 
//
// input:
// ======
// + *nacma
//    - NACMA_STRUCT to containing mask file units
//
// output:
// =======
// + *cm
//    - CLOUD_MASKS struct containing binary masks
/******************************************************************************/
void createMaskFiles(NACMA_STRUCT *nacma, CLOUD_MASKS *cm);

#endif
