#include "applic.h"
#include "zvproto.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "cloud_masks.h"
#include "ImageUtils.h"
#include "AsterManager.h"
#include "NACMA_Utils.h"

/******************************************************************************/
void openMaskFile(int *unit, char parmName[IU_MAX_FNAME_LEN], int nl, int ns, int inst)
{
   int cnt, def, status;
   char fname[250];

   if(!strcmp(parmName, "out"))
   {
      status = zvunit(unit, "out", 1, NULL);
      assert(status == 1);
   }
   else
   {
      status = zvparm(parmName, fname, &cnt, &def, 1, IU_MAX_FNAME_LEN);
      assert(status == 1);
      if(cnt)
      {
         status = zvunit(unit, "MASK", inst, "U_NAME", fname, NULL);
         assert(status == 1);
      }
      else
      {
         *unit = -1;
         return;
      }
   }

   status = zvopen(*unit, "OP", "WRITE", "U_FORMAT", "BYTE", "O_FORMAT", "BYTE", "U_NL", nl, "U_NS", ns, NULL);
   assert(status == 1);
}

/******************************************************************************/
void closeMaskFile(int unit)
{
   int status;

   status = zvclose(unit, NULL);
   assert(status == 1);
}

/******************************************************************************/
void openFile(int *unit, char *name, char *fname, char *mode, char *oformat, int inst, int nl, int ns)
{
   int status;

   if(!strcmp(name, "inp") || !strcmp(name, "out"))
   {
      status = zvunit(unit, name, inst, NULL);
      assert(status == 1);
   }
   else
   {
      status = zvunit(unit, name, inst, "U_NAME", fname, NULL);
      assert(status == 1);
   }

   if(!strcmp(name, "inp") || !strcmp(mode, "read"))
      status = zvopen(*unit, "OP", mode, "U_FORMAT", "DOUB", NULL);
   else
      status = zvopen(*unit, "OP", mode, "U_FORMAT", "DOUB", "O_FORMAT", oformat,
                      "U_NL", nl, "U_NS", ns, NULL);

   assert(status == 1);
}

/******************************************************************************/
VICAR_IMAGE* getInpFile(char *parmName, char *instName, char *fname, int inst)
{
   int unit, cnt, def, status;

   if(parmName != NULL && fname == NULL && strcmp(instName, "inp"))
   {
      status = zvparm(parmName, fname, &cnt, &def, 1, IU_MAX_FNAME_LEN);
      assert(status == 1);
   }
   openFile(&unit, instName, fname, "read", NULL, inst, 0, 0);
   return getImage(unit);
}

/******************************************************************************/
VICAR_IMAGE* getOutFile(char *parmName, char *fmt, char *name, int inst, int nl, int ns)
{
   int unit, cnt, def, status;
   char fname[250];

   status = zvparm(parmName, fname, &cnt, &def, 1, IU_MAX_FNAME_LEN);
   assert(status == 1);
   if(cnt)
   {
      openFile(&unit, name, fname, "write", fmt, inst, nl, ns);
      return getImage(unit);
   }

   return NULL;
}

/******************************************************************************/
NACMA_STRUCT* getNACMA()
{
   char fname[IU_MAX_FNAME_LEN];
   int status, cnt, def, i;

   int asterUnits[NACMA_BANDS_USED];
   int asterMasterUnits[ASTER_N_BANDS];
   int filtNL, filtNS;

   // cloud masks units
   NACMA_STRUCT *nacma;
   nacma = (NACMA_STRUCT*)malloc(sizeof(NACMA_STRUCT));

   // getting inp files
   for(i = 0; i < NACMA_BANDS_USED; i++)
      openFile(&asterUnits[i], "inp", "", "read", "NONE", i+1, 0, 0);

   // get AsterManager
   //   nacma->am = (ASTER_MANAGER*)malloc(sizeof(ASTER_MANAGER));
   for(i = 0; i < ASTER_N_BANDS; i++) asterMasterUnits[i] = -1;
   asterMasterUnits[ASTER_BAND1] = asterUnits[0];
   asterMasterUnits[ASTER_BAND2] = asterUnits[1];
   asterMasterUnits[ASTER_BAND3N] = asterUnits[2];
   asterMasterUnits[ASTER_BAND4] = asterUnits[3];
   asterMasterUnits[ASTER_BAND5] = asterUnits[4];
   asterMasterUnits[ASTER_BAND13] = asterUnits[5];
   asterMasterUnits[ASTER_BAND14] = asterUnits[6];

   // getting meta file
   status = zvparm("META", fname, &cnt, &def, 2, IU_MAX_FNAME_LEN);
   assert(status == 1);
   nacma->metafile = fopen(fname, "r");
   assert(nacma->metafile != 0x0);

   nacma->am = AM_getAsterManager(asterMasterUnits, nacma->metafile);

   // getting radiance files
   nacma->rad1Image = getOutFile("RAD1OUT", "REAL", "rad", 1,
                                 nacma->am->images[ASTER_BAND1]->nl, 
                                 nacma->am->images[ASTER_BAND1]->ns);
   nacma->rad2Image = getOutFile("RAD2OUT", "REAL", "rad", 2,
                                 nacma->am->images[ASTER_BAND2]->nl, 
                                 nacma->am->images[ASTER_BAND2]->ns);
   nacma->rad3Image = getOutFile("RAD3OUT", "REAL", "rad", 3,
                                 nacma->am->images[ASTER_BAND3N]->nl,
                                 nacma->am->images[ASTER_BAND3N]->ns);
   nacma->rad4Image = getOutFile("RAD4OUT", "REAL", "rad", 4,
                                 nacma->am->images[ASTER_BAND4]->nl,
                                 nacma->am->images[ASTER_BAND4]->ns);
   nacma->rad5Image = getOutFile("RAD5OUT", "REAL", "rad", 5,
                                 nacma->am->images[ASTER_BAND5]->nl,
                                 nacma->am->images[ASTER_BAND5]->ns);
   nacma->rad13Image = getOutFile("RAD13OUT", "REAL", "rad", 6,
                                 nacma->am->images[ASTER_BAND13]->nl,
                                 nacma->am->images[ASTER_BAND13]->ns);
   nacma->rad14Image = getOutFile("RAD14OUT", "REAL", "rad", 7,
                                 nacma->am->images[ASTER_BAND14]->nl,
                                 nacma->am->images[ASTER_BAND14]->ns);

   // getting reflectance files
   nacma->ref1Image = getOutFile("REF1OUT", "REAL", "ref", 1,
                                 nacma->am->images[ASTER_BAND1]->nl,
                                 nacma->am->images[ASTER_BAND1]->ns);
   nacma->ref2Image = getOutFile("REF2OUT", "REAL", "ref", 2,
                                 nacma->am->images[ASTER_BAND2]->nl,
                                 nacma->am->images[ASTER_BAND2]->ns);
   nacma->ref3Image = getOutFile("REF3OUT", "REAL", "ref", 3,
                                 nacma->am->images[ASTER_BAND3N]->nl,
                                 nacma->am->images[ASTER_BAND3N]->ns);
   nacma->ref4Image = getOutFile("REF4OUT", "REAL", "ref", 4,
                                 nacma->am->images[ASTER_BAND4]->nl,
                                 nacma->am->images[ASTER_BAND4]->ns);
   nacma->ref5Image = getOutFile("REF5OUT", "REAL", "ref", 5,
                                 nacma->am->images[ASTER_BAND5]->nl,
                                 nacma->am->images[ASTER_BAND5]->ns);

   // getting brightness temperature files
   nacma->bTemp13Image = getOutFile("BTEMP13OUT", "REAL", "btemp", 1,
                                    nacma->am->images[ASTER_BAND13]->nl,
                                    nacma->am->images[ASTER_BAND13]->ns);
   nacma->bTemp14Image = getOutFile("BTEMP14OUT", "REAL", "btemp", 2,
                                    nacma->am->images[ASTER_BAND14]->nl,
                                    nacma->am->images[ASTER_BAND14]->ns);

   filtNL = nacma->am->images[ASTER_BAND13]->nl;
   filtNS = nacma->am->images[ASTER_BAND13]->ns;

   // getting output file
   openMaskFile(&(nacma->CMcloudUnit), "out", filtNL, filtNS, 1);
   openMaskFile(&(nacma->CMsnowUnit), "MASKSNOW", filtNL, filtNS, 2);
   openMaskFile(&(nacma->CMdesertUnit), "MASKDESERT", filtNL, filtNS, 3);
   openMaskFile(&(nacma->CMcloud_warmUnit), "MASKWARMCLOUD", filtNL, filtNS, 4);
   openMaskFile(&(nacma->CMcloud_coldUnit), "MASKCOLDCLOUD", filtNL, filtNS, 5);
   openMaskFile(&(nacma->iceUnit), "MASKICE", filtNL, filtNS, 6);
   openMaskFile(&(nacma->filter_cirrusUnit), "MASKCIRRUS", filtNL, filtNS, 7);
   openMaskFile(&(nacma->ambigUnit), "MASKAMBIG", filtNL, filtNS, 8);
   openMaskFile(&(nacma->validUnit), "MASKVALID", filtNL, filtNS, 9);

   openMaskFile(&(nacma->filter1Unit), "FILTER1OUT", filtNL, filtNS, 10);
   openMaskFile(&(nacma->filter2Unit), "FILTER2OUT", filtNL, filtNS, 11);
   openMaskFile(&(nacma->filter3Unit), "FILTER3OUT", filtNL, filtNS, 12);
   openMaskFile(&(nacma->filter4Unit), "FILTER4OUT", filtNL, filtNS, 13);
   openMaskFile(&(nacma->filter5Unit), "FILTER5OUT", filtNL, filtNS, 14);
   openMaskFile(&(nacma->filter6Unit), "FILTER6OUT", filtNL, filtNS, 15);
   openMaskFile(&(nacma->filter7Unit), "FILTER7OUT", filtNL, filtNS, 16);
   openMaskFile(&(nacma->filter8Unit), "FILTER8OUT", filtNL, filtNS, 17);

   // getting downsampled reflectance files
   nacma->ref1dsImage = getVRI(nacma->ref1Image, getOutFile("REF1DSOUT", "REAL", "refds", 1, filtNL, filtNS), IU_BILINEAR_INTERP);
   nacma->ref2dsImage = getVRI(nacma->ref2Image, getOutFile("REF2DSOUT", "REAL", "refds", 2, filtNL, filtNS), IU_BILINEAR_INTERP);
   nacma->ref3dsImage = getVRI(nacma->ref3Image, getOutFile("REF3DSOUT", "REAL", "refds", 3, filtNL, filtNS), IU_BILINEAR_INTERP);
   nacma->ref4dsImage = getVRI(nacma->ref4Image, getOutFile("REF4DSOUT", "REAL", "refds", 4, filtNL, filtNS), IU_BILINEAR_INTERP);
   nacma->ref5dsImage = getVRI(nacma->ref5Image, getOutFile("REF5DSOUT", "REAL", "refds", 5, filtNL, filtNS), IU_BILINEAR_INTERP);

   // getting tempcomp file
   nacma->tempCompImage = getOutFile("TEMPCOMPOUT", "REAL", "tempcomp", 1, filtNL, filtNS);

   // getting ndsi file
   nacma->ndsiImage = getOutFile("NDSIOUT", "REAL", "ndsi", 1, filtNL, filtNS);

   // getting growing vegetation file
   nacma->gvImage = getOutFile("GVOUT", "REAL", "gv", 1, filtNL, filtNS);

   // getting senescing vegetation file
   nacma->svImage = getOutFile("SVOUT", "REAL", "Sv", 1, filtNL, filtNS);

   // getting reflective soil file
   nacma->rsImage = getOutFile("RSOUT", "REAL", "rs", 1, filtNL, filtNS);

   return nacma;
}

/******************************************************************************/
void deleteNACMA(NACMA_STRUCT *nacma)
{
   fclose(nacma->metafile);

   if(nacma->CMcloudUnit != -1) closeMaskFile(nacma->CMcloudUnit);
   if(nacma->CMsnowUnit != -1) closeMaskFile(nacma->CMsnowUnit);
   if(nacma->CMdesertUnit != -1) closeMaskFile(nacma->CMdesertUnit);
   if(nacma->CMcloud_warmUnit != -1) closeMaskFile(nacma->CMcloud_warmUnit);
   if(nacma->CMcloud_coldUnit != -1) closeMaskFile(nacma->CMcloud_coldUnit);
   if(nacma->iceUnit != -1) closeMaskFile(nacma->iceUnit);
   if(nacma->filter_cirrusUnit != -1) closeMaskFile(nacma->filter_cirrusUnit);
   if(nacma->ambigUnit != -1) closeMaskFile(nacma->ambigUnit);
   if(nacma->validUnit != -1) closeMaskFile(nacma->validUnit);

   if(nacma->filter1Unit != -1) closeMaskFile(nacma->filter1Unit);
   if(nacma->filter2Unit != -1) closeMaskFile(nacma->filter2Unit);
   if(nacma->filter3Unit != -1) closeMaskFile(nacma->filter3Unit);
   if(nacma->filter4Unit != -1) closeMaskFile(nacma->filter4Unit);
   if(nacma->filter5Unit != -1) closeMaskFile(nacma->filter5Unit);
   if(nacma->filter6Unit != -1) closeMaskFile(nacma->filter6Unit);
   if(nacma->filter7Unit != -1) closeMaskFile(nacma->filter7Unit);
   if(nacma->filter8Unit != -1) closeMaskFile(nacma->filter8Unit);

   if(nacma->rad1Image != NULL) deleteAndCloseImage(&(nacma->rad1Image));
   if(nacma->rad2Image != NULL) deleteAndCloseImage(&(nacma->rad2Image));
   if(nacma->rad3Image != NULL) deleteAndCloseImage(&(nacma->rad3Image));
   if(nacma->rad4Image != NULL) deleteAndCloseImage(&(nacma->rad4Image));
   if(nacma->rad5Image != NULL) deleteAndCloseImage(&(nacma->rad5Image));
   if(nacma->rad13Image != NULL) deleteAndCloseImage(&(nacma->rad13Image));
   if(nacma->rad14Image != NULL) deleteAndCloseImage(&(nacma->rad14Image));

   if(nacma->ref1Image != NULL) deleteAndCloseImage(&(nacma->ref1Image));
   if(nacma->ref2Image != NULL) deleteAndCloseImage(&(nacma->ref2Image));
   if(nacma->ref3Image != NULL) deleteAndCloseImage(&(nacma->ref3Image));
   if(nacma->ref4Image != NULL) deleteAndCloseImage(&(nacma->ref4Image));
   if(nacma->ref5Image != NULL) deleteAndCloseImage(&(nacma->ref5Image));
   if(nacma->bTemp13Image != NULL) deleteAndCloseImage(&(nacma->bTemp13Image));
   if(nacma->bTemp14Image != NULL) deleteAndCloseImage(&(nacma->bTemp14Image));
   
   if(nacma->tempCompImage != NULL) deleteAndCloseImage(&(nacma->tempCompImage));
   if(nacma->ndsiImage != NULL) deleteAndCloseImage(&(nacma->ndsiImage));
   if(nacma->gvImage != NULL) deleteAndCloseImage(&(nacma->gvImage));
   if(nacma->svImage != NULL) deleteAndCloseImage(&(nacma->svImage));
   if(nacma->rsImage != NULL) deleteAndCloseImage(&(nacma->rsImage));

   if(nacma->ref1dsImage->to != NULL) deleteAndCloseImage(&(nacma->ref1dsImage->to));
   if(nacma->ref2dsImage->to != NULL) deleteAndCloseImage(&(nacma->ref2dsImage->to));
   if(nacma->ref3dsImage->to != NULL) deleteAndCloseImage(&(nacma->ref3dsImage->to));
   if(nacma->ref4dsImage->to != NULL) deleteAndCloseImage(&(nacma->ref4dsImage->to));
   if(nacma->ref5dsImage->to != NULL) deleteAndCloseImage(&(nacma->ref5dsImage->to));

   if(nacma->ref1dsImage != NULL) deleteVRI(&(nacma->ref1dsImage));
   if(nacma->ref2dsImage != NULL) deleteVRI(&(nacma->ref2dsImage));
   if(nacma->ref3dsImage != NULL) deleteVRI(&(nacma->ref3dsImage));
   if(nacma->ref4dsImage != NULL) deleteVRI(&(nacma->ref4dsImage));
   if(nacma->ref5dsImage != NULL) deleteVRI(&(nacma->ref5dsImage));

   AM_deleteAsterManager(nacma->am);

   free(nacma);
}

/******************************************************************************/
void createRadianceFiles(NACMA_STRUCT *nacma)
{
   if(nacma->rad1Image != NULL) AM_createRadianceImage(nacma->am, nacma->rad1Image, ASTER_BAND1);
   if(nacma->rad2Image != NULL) AM_createRadianceImage(nacma->am, nacma->rad2Image, ASTER_BAND2);
   if(nacma->rad3Image != NULL) AM_createRadianceImage(nacma->am, nacma->rad3Image, ASTER_BAND3N);
   if(nacma->rad4Image != NULL) AM_createRadianceImage(nacma->am, nacma->rad4Image, ASTER_BAND4);
   if(nacma->rad5Image != NULL) AM_createRadianceImage(nacma->am, nacma->rad5Image, ASTER_BAND5);
   if(nacma->rad13Image != NULL) AM_createRadianceImage(nacma->am, nacma->rad13Image, ASTER_BAND13);
   if(nacma->rad14Image != NULL) AM_createRadianceImage(nacma->am, nacma->rad14Image, ASTER_BAND14);
}

/******************************************************************************/
void createReflectanceFiles(NACMA_STRUCT *nacma)
{
   if(nacma->ref1Image != NULL) AM_createReflectanceImage(nacma->am, nacma->ref1Image, ASTER_BAND1);
   if(nacma->ref2Image != NULL) AM_createReflectanceImage(nacma->am, nacma->ref2Image, ASTER_BAND2);
   if(nacma->ref3Image != NULL) AM_createReflectanceImage(nacma->am, nacma->ref3Image, ASTER_BAND3N);
   if(nacma->ref4Image != NULL) AM_createReflectanceImage(nacma->am, nacma->ref4Image, ASTER_BAND4);
   if(nacma->ref5Image != NULL) AM_createReflectanceImage(nacma->am, nacma->ref5Image, ASTER_BAND5);
}

/******************************************************************************/
void reopenReflectanceFiles(NACMA_STRUCT *nacma)
{
   char fname1[IU_MAX_FNAME_LEN], fname2[IU_MAX_FNAME_LEN], fname3[IU_MAX_FNAME_LEN],
        fname4[IU_MAX_FNAME_LEN], fname5[IU_MAX_FNAME_LEN];

   strcpy(fname1, nacma->ref1Image->fname);
   strcpy(fname2, nacma->ref2Image->fname);
   strcpy(fname3, nacma->ref3Image->fname);
   strcpy(fname4, nacma->ref4Image->fname);
   strcpy(fname5, nacma->ref5Image->fname);

   if(nacma->ref1Image != NULL) deleteAndCloseImage(&(nacma->ref1Image));
   if(nacma->ref2Image != NULL) deleteAndCloseImage(&(nacma->ref2Image));
   if(nacma->ref3Image != NULL) deleteAndCloseImage(&(nacma->ref3Image));
   if(nacma->ref4Image != NULL) deleteAndCloseImage(&(nacma->ref4Image));
   if(nacma->ref5Image != NULL) deleteAndCloseImage(&(nacma->ref5Image));

   nacma->ref1Image = getInpFile(NULL, "NONE", fname1, 1);
   nacma->ref2Image = getInpFile(NULL, "NONE", fname2, 2);
   nacma->ref3Image = getInpFile(NULL, "NONE", fname3, 3);
   nacma->ref4Image = getInpFile(NULL, "NONE", fname4, 4);
   nacma->ref5Image = getInpFile(NULL, "NONE", fname5, 5);
}

/******************************************************************************/
void reopenFilesForPass1(NACMA_STRUCT *nacma)
{
   char ref1fname[IU_MAX_FNAME_LEN], ref2fname[IU_MAX_FNAME_LEN],
        ref3fname[IU_MAX_FNAME_LEN], ref4fname[IU_MAX_FNAME_LEN],
        ref5fname[IU_MAX_FNAME_LEN];
   char bTemp13fname[IU_MAX_FNAME_LEN], bTemp14fname[IU_MAX_FNAME_LEN];

   strcpy(ref1fname, nacma->ref1dsImage->to->fname);
   strcpy(ref2fname, nacma->ref2dsImage->to->fname);
   strcpy(ref3fname, nacma->ref3dsImage->to->fname);
   strcpy(ref4fname, nacma->ref4dsImage->to->fname);
   strcpy(ref5fname, nacma->ref5dsImage->to->fname);

   strcpy(bTemp13fname, nacma->bTemp13Image->fname);
   strcpy(bTemp14fname, nacma->bTemp14Image->fname);

   deleteAndCloseImage(&(nacma->ref1dsImage->to));
   deleteAndCloseImage(&(nacma->ref2dsImage->to));
   deleteAndCloseImage(&(nacma->ref3dsImage->to));
   deleteAndCloseImage(&(nacma->ref4dsImage->to));
   deleteAndCloseImage(&(nacma->ref5dsImage->to));
   deleteAndCloseImage(&(nacma->bTemp13Image));
   deleteAndCloseImage(&(nacma->bTemp14Image));
   deleteAndCloseImage(&(nacma->ref1Image));
   deleteAndCloseImage(&(nacma->ref2Image));
   deleteAndCloseImage(&(nacma->ref3Image));
   deleteAndCloseImage(&(nacma->ref4Image));
   deleteAndCloseImage(&(nacma->ref5Image));

   nacma->ref1Image = getInpFile(NULL, "ref", ref1fname, 1);
   nacma->ref2Image = getInpFile(NULL, "ref", ref2fname, 2);
   nacma->ref3Image = getInpFile(NULL, "ref", ref3fname, 3);
   nacma->ref4Image = getInpFile(NULL, "ref", ref4fname, 4);
   nacma->ref5Image = getInpFile(NULL, "ref", ref5fname, 5);

   nacma->bTemp13Image = getInpFile(NULL, "btemp", bTemp13fname, 1);
   nacma->bTemp14Image = getInpFile(NULL, "btemp", bTemp14fname, 2);
}

/******************************************************************************/
void createDownSampledFiles(NACMA_STRUCT *nacma)
{
   if(nacma->ref1dsImage != NULL) nacma->ref1dsImage->from = nacma->ref1Image;
   if(nacma->ref2dsImage != NULL) nacma->ref2dsImage->from = nacma->ref2Image;
   if(nacma->ref3dsImage != NULL) nacma->ref3dsImage->from = nacma->ref3Image;
   if(nacma->ref4dsImage != NULL) nacma->ref4dsImage->from = nacma->ref4Image;
   if(nacma->ref5dsImage != NULL) nacma->ref5dsImage->from = nacma->ref5Image;

   if(nacma->ref1dsImage != NULL) createDownSampledImage(nacma->ref1dsImage);
   if(nacma->ref2dsImage != NULL) createDownSampledImage(nacma->ref2dsImage);
   if(nacma->ref3dsImage != NULL) createDownSampledImage(nacma->ref3dsImage);
   if(nacma->ref4dsImage != NULL) createDownSampledImage(nacma->ref4dsImage);
   if(nacma->ref5dsImage != NULL) createDownSampledImage(nacma->ref5dsImage);
}

/******************************************************************************/
void createBTempFiles(NACMA_STRUCT *nacma)
{
   if(nacma->bTemp13Image) AM_createBTempImage(nacma->am, nacma->bTemp13Image, ASTER_BAND13);
   if(nacma->bTemp14Image) AM_createBTempImage(nacma->am, nacma->bTemp14Image, ASTER_BAND14);
}

/******************************************************************************/
void createMaskFile(NACMA_STRUCT *nacma, CLOUD_MASKS *cm, int type)
{
   int i, status;

   for(i = 0; i < cm->vars->nl; i++)
   {
      switch(type)
      {
         case CM_SNOWMASK:
            status = zvwrit(nacma->CMsnowUnit, cm->CMsnow[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_VALIDMASK:
            status = zvwrit(nacma->validUnit, cm->valid[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_AMBIGMASK:
            status = zvwrit(nacma->ambigUnit, cm->ambig[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_CLOUDMASK:
            status = zvwrit(nacma->CMcloudUnit, cm->CMcloud[i], "LINE", i+1, NULL);
            assert(status == 1);

         case CM_FILTER1:
            status = zvwrit(nacma->filter1Unit, cm->filter1[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER2:
            status = zvwrit(nacma->filter2Unit, cm->filter2[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER3:
            status = zvwrit(nacma->filter3Unit, cm->filter3[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER4:
            status = zvwrit(nacma->filter4Unit, cm->filter4[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER5:
            status = zvwrit(nacma->filter5Unit, cm->filter5[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER6:
            status = zvwrit(nacma->filter6Unit, cm->filter6[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER7:
            status = zvwrit(nacma->filter7Unit, cm->filter7[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;
         case CM_FILTER8:
            status = zvwrit(nacma->filter8Unit, cm->filter8[i], "LINE", i+1, NULL);
            assert(status == 1);
            break;

         assert(0);
      }
   }
}

/******************************************************************************/
void createMaskFiles(NACMA_STRUCT *nacma, CLOUD_MASKS *cm)
{ 
   if(nacma->CMsnowUnit != -1) createMaskFile(nacma, cm, CM_SNOWMASK);
   if(nacma->validUnit != -1) createMaskFile(nacma, cm, CM_VALIDMASK);
   if(nacma->ambigUnit != -1) createMaskFile(nacma, cm, CM_AMBIGMASK);
   if(nacma->CMcloudUnit != -1) createMaskFile(nacma, cm, CM_CLOUDMASK);

   if(nacma->filter1Unit != -1) createMaskFile(nacma, cm, CM_FILTER1);
   if(nacma->filter2Unit != -1) createMaskFile(nacma, cm, CM_FILTER2);
   if(nacma->filter3Unit != -1) createMaskFile(nacma, cm, CM_FILTER3);
   if(nacma->filter4Unit != -1) createMaskFile(nacma, cm, CM_FILTER4);
   if(nacma->filter5Unit != -1) createMaskFile(nacma, cm, CM_FILTER5);
   if(nacma->filter6Unit != -1) createMaskFile(nacma, cm, CM_FILTER6);
   if(nacma->filter7Unit != -1) createMaskFile(nacma, cm, CM_FILTER7);
   if(nacma->filter8Unit != -1) createMaskFile(nacma, cm, CM_FILTER8);
}
