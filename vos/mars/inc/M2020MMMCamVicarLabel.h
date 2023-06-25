/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 * @author Mauricio Hess-Flores {mauricio.a.hess.flores@jpl.nasa.gov}
 */

#ifndef M2020MMMCAMVICARLABEL_
#define M2020MMMCAMVICARLABEL_

#include "M2020EngCamVicarLabel.h"
#include "M2020MMMCamEdr.h"
#include "lbl_mini_header.h"
#include "lbl_aci_scanner_articulation_state.h"

#include "PigCameraModel.h"
#include "PigCoordSystem.h"
#include "PigMission.h"

#include <memory>

class M2020MMMCamVicarLabel:public M2020EngCamVicarLabel {

      protected:

   M2020MMMCamEdr * mmmCamEdr_;

   const M2020IdphHeader *mmmCamHdr_;
   const M2020IdphHeader *mmmCamAncHdr_;
   const M2020IdphHeader *mmmCamSupHdr_;
   LblMiniHeader_typ idMiniHeader_;
   LblAciScannerArticulationState_typ idAciScannerArticulationState_;

   PigCameraModel *camMdl_;
   PigCoordSystem *roverCs_;
   PigMission *m_;

      public:

   const string RMI_ANC_DPO_NAME;

   explicit M2020MMMCamVicarLabel(M2020ImgEdr * m);
    virtual ~ M2020MMMCamVicarLabel() {
   } virtual void setProdName() throw(exception);
   virtual void setProdNameSample() throw(exception);
   virtual void setProdNameConfig(char) throw(exception) {
   };
   virtual void setProdNameProd() throw(exception);
   virtual void setMMMAncHdr(M2020DpMetaData * dpMetadata, M2020IdphHeader * idph, M2020IdphHeader * ancidph,
              M2020IdphHeader * supidph) throw(exception);

   virtual void createLabels() throw(exception);
   virtual void initialize();
   virtual void writeLabels(string & labels) throw(exception) {
      M2020VicarLabel::writeLabels(labels);
   }

   virtual void writeLabels(int unit) throw(exception);
   virtual void readLabels(int unit) throw(exception);

   virtual void writeMiniHeaderLabels(int) throw(exception);
   virtual void readMiniHeaderLabels(int) throw(exception);

   virtual void writeAciScannerArticulationStateLabels(int) throw(exception);
   virtual void readAciScannerArticulationStateLabels(int) throw(exception);

   virtual void writeVideoParmsLabels(int) throw(exception);
   virtual void readVideoParmsLabels(int) throw(exception);

   virtual void createIdentificationLabels() throw(exception);
   virtual void createTelemetryLabels() throw(exception);
   virtual void createPdsHistoryLabels() throw(exception);
   virtual void createCoordinateLabels() throw(exception);
   virtual void createRsmArticulationLabels() throw(exception);
   virtual void createChasArticulationLabels() throw(exception);
   virtual void createShaArticulationLabels() throw(exception);
   virtual void createScsArticulationLabels() throw(exception);
   virtual void createDrillArticulationLabels() throw(exception);
   virtual void createObservationRequestLabels() throw(exception);
   virtual void createInstrumentStateLabels() throw(exception);
   virtual void createImageLabels() throw(exception);
   virtual void createDerivedImageLabels() throw(exception);
   virtual void createDerivedGeometryLabels() throw(exception);
   virtual void createDerivedRoverGeometryLabels() throw(exception);
   virtual void createDerivedSiteGeometryLabels() throw(exception);
   //virtual void createImageHeaderLabels     () throw (exception);
   virtual void createProductRequestLabels() throw(exception);
   virtual void createVideoParmsLabels() throw(exception);
   virtual void createCompressionLabels() throw(exception);
   virtual void createGeometricCameraModelLabels() throw(exception);
   virtual void createMiniHeaderLabels() throw(exception);
   //virtual void createCommandLabels() throw (exception);
   virtual void createAciScannerArticulationStateLabels() throw(exception);

   void computeRoverSiteAzEl() throw(exception);

   virtual void getInstCoordFrameId(LblApiIdItem_typ &);
   virtual const char *getInstrId() throw(exception) {
      return this->outputFileName_.instr;
   }

   //int planetDayNumber_;   // planet day number
   //double rover_az_, rover_el_, site_az_, site_el_, solar_az_, solar_el_;
   double *c_, *a_, *h_, *v_, *o_, *r_, *e_;

   int spiceLoaded;
   int siteFixed;

};

#endif            //M2020MMMCAMVICARLABEL_
