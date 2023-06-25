/*
  Copyright 2008-Present, California Institute of Technology. 
  ALL RIGHTS RESERVED.
  U.S. Government Sponsorship acknowledge.
*/

#ifndef M2020SUPERCAMIMGPDSLABEL_H_
#define M2020SUPERCAMIMGPDSLABEL_H_

//#include "M2020Exception.h"

#include "M2020DpMetaData.h"
#include "M2020DPOParser.h"
#include "M2020IdphHeader.h"
//#include "M2020ChemCamEdr.h"
//#include "M2020CheminEdr.h"
//#include "M2020MMMCamEdr.h"
#include "M2020ImgEdr.h"

#include "PigCameraModel.h"
#include "PigCoordSystem.h"

using namespace std;

#define IN_RANGE(val,min,max) ((val>=min && val<=max) ? true : false)

class M2020SuperCamImgPdsLabel {
      protected:

   M2020Env * env_;
   M2020ImgEdr *edr_;
   M2020DpMetaData *md_;
   string prodName_, instrId_, config_, venue_, prodType_, seqId_, who_, ver_, ext_, sclk_;
   int site_, drive_;
   //int sol_;
   string sol_;
   char *m2020FileName_;

   int planetDayNumber_;   // planet day number

   const M2020IdphHeader *sCamAncTempHdr_ = NULL;
   const M2020IdphHeader *sCamHdr_ = NULL;
   const M2020IdphHeader *sCamAncSendDataArgsHdr_ = NULL;
   const M2020IdphHeader *sCamAncPointHdr_ = NULL;

   PigCameraModel *cam_model;
   PigCoordSystem *rover_cs;
   PigCoordSystem *site_cs;
   PigVector look_;

      public:
    explicit M2020SuperCamImgPdsLabel(M2020DpMetaData *, M2020ImgEdr * edr, char *instr);
    virtual ~ M2020SuperCamImgPdsLabel();

   virtual void initialize(M2020ImgEdr * edr, char *instrumentId);
   virtual void writeCommonKeywords(char *labelFilename, char *instrumentId, M2020DpMetaData * m, int length);
   void createM2020FileName();
   char *getM2020FileName() {
      return m2020FileName_;
   };

  /**
   * Call individual create methods.
   */
   virtual void createLabels(int length) throw(exception);

   static void getParam(char *name, void *value, int *count, int maxcnt, int length, void *clientData);

   string fixNullDouble(double val);
   double *c_, *a_, *h_, *v_;
   int siteFixed;

};

#endif            /*M2020SUPERCAMIMGPDSLABEL_H_ */
