/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */

#ifndef M2020SUPERCAMVICARLABEL_
#define M2020SUPERCAMVICARLABEL_

#include "M2020EngCamVicarLabel.h"
#include "M2020SuperCamEdr.h"
#include "lbl_chem_request.h"
#include "lbl_chem_bin_table.h"
#include "lbl_scam_mini_header.h"

#include "PigCameraModel.h"
#include "PigCoordSystem.h"

#include <memory>

class M2020SuperCamVicarLabel:public M2020EngCamVicarLabel {

      protected:

   LblChemRequest_typ idChemReq_;
   LblChemBinTable_typ idChemIDPHTable_;
   LblChemBinTable_typ idChemAncTable_;
   LblChemBinTable_typ idChemCmdReplyFrameTable_[50];
   LblChemBinTable_typ idChemTakeImageTimeTable_;
   LblChemBinTable_typ idChemSOHSciDataColsTable_[2];
   LblChemBinTable_typ idChemSOHToRCETable_[2];
   LblChemBinTable_typ idChemSOHChecksumTable_[2];
   LblChemBinTable_typ idChemSOHBeforeTable_;
   LblChemBinTable_typ idChemSOHAfterTable_;
   LblChemBinTable_typ idChemAutoFocusTable_;
   LblChemBinTable_typ idChemMUHeaderTable_;
   LblChemBinTable_typ idChemMUFooterTable_;
   LblChemBinTable_typ idChemImageReplyTable_;
   LblChemBinTable_typ idChemImageHeaderFooterTable_;
   LblScamMiniHdr_typ idScamMiniHdr_;
   M2020SuperCamEdr *chemEdr_;

   const M2020IdphHeader *sCamAncHdr_;
   const M2020IdphHeader *sCamHdr_;
   const M2020IdphHeader *sCamAncTempHdr_;
   const M2020IdphHeader *sCamAncArgsHdr_;
   const M2020IdphHeader *sCamAncParamsHdr_;
   const M2020IdphHeader *sCamAncFocalDistHdr_;

   PigCameraModel *cam_model;
   PigCoordSystem *rover_cs;
   PigCoordSystem *site_cs;
   PigVector look_;

      public:

   const string RMI_ANC_DPO_NAME;

   explicit M2020SuperCamVicarLabel(M2020ImgEdr * m);
    virtual ~ M2020SuperCamVicarLabel() {
   }
   //virtual const char* getProductName(); 
   virtual void setProdName() throw(exception);
   virtual void setProdNameSample() throw(exception);
   virtual void setProdNameConfig(char) throw(exception);
   virtual void setProdNameProd() throw(exception);
   virtual void setAncHdr(M2020DpMetaData * dpMetadata) throw(exception);

   virtual void createLabels() throw(exception);
   virtual void initialize();
   virtual void writeLabels(string & labels) throw(exception) {
      M2020VicarLabel::writeLabels(labels);
   }
   virtual void createSuperRequestLabels() throw(exception);
   virtual void writeSuperRequestLabels(int) throw(exception);
   virtual void readSuperRequestLabels(int) throw(exception);

   virtual void createSuperMiniHdrLabels() throw(exception);
   virtual void writeSuperMiniHdrLabels(int) throw(exception);
   virtual void readSuperMiniHdrLabels(int) throw(exception);

   virtual void writeLabels(int unit) throw(exception);
   virtual void readLabels(int unit) throw(exception);

   virtual void createIdentificationLabels() throw(exception);
   virtual void createTelemetryLabels() throw(exception);
   virtual void createPdsHistoryLabels() throw(exception);
   virtual void createCoordinateLabels() throw(exception);
   virtual void createRsmArticulationLabels() throw(exception);
   virtual void createArmArticulationLabels() throw(exception);
   virtual void createChasArticulationLabels() throw(exception);
   virtual void createHgaArticulationLabels() throw(exception);
   virtual void createShaArticulationLabels() throw(exception);
   virtual void createScsArticulationLabels() throw(exception);
   virtual void createDrillArticulationLabels() throw(exception);
   virtual void createObservationRequestLabels() throw(exception);
   static void getParam(char *name, void *value, int *count, int maxcnt, int length, void *clientData);
   virtual void createInstrumentStateLabels() throw(exception);
   virtual void createImageLabels() throw(exception);
   virtual void createDerivedImageLabels() throw(exception);
   virtual void createDerivedGeometryLabels() throw(exception);
   virtual void createDerivedRoverGeometryLabels() throw(exception);
   virtual void createDerivedSiteGeometryLabels() throw(exception);
   //virtual void createImageHeaderLabels     () throw (exception);
   virtual void createProductRequestLabels() throw(exception);
   virtual void createCompressionLabels() throw(exception);
   virtual void set_cmod() throw(exception);
   virtual void createGeometricCameraModelLabels() throw(exception);
   //virtual void createCommandLabels() throw (exception);

   virtual void getInstCoordFrameId(LblApiIdItem_typ &);
   virtual const char *getInstrId() throw(exception) {
      return this->outputFileName_.instr;
   }

   double *c_, *a_, *h_, *v_, *o_, *r_, *e_;
   int siteFixed;

};

#endif            //M2020SUPERCAMVICARLABEL_
