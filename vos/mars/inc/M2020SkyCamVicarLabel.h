/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */

#ifndef M2020SKYCAMVICARLABEL_
#define M2020SKYCAMVICARLABEL_

#include "M2020EngCamVicarLabel.h"
#include "M2020EngCamEdr.h"

#include <memory>

class M2020SkyCamVicarLabel:public M2020EngCamVicarLabel {

      protected:

   const M2020IdphHeader *skycamHdr_;

      public:

   explicit M2020SkyCamVicarLabel(M2020ImgEdr * m);
    virtual ~ M2020SkyCamVicarLabel() {
   }
   //virtual const char* getProductName(); 
   virtual void setProdName() throw(exception);
   virtual void setProdNameSample() throw(exception);
   virtual void setProdNameConfig(char) throw(exception);
   virtual void setProdNameProd() throw(exception);

   virtual void createLabels() throw(exception);
   virtual void initialize();
   virtual void writeLabels(string & labels) throw(exception) {
      M2020VicarLabel::writeLabels(labels);
   }
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
   virtual void createGeometricCameraModelLabels() throw(exception);
   //virtual void createCommandLabels() throw (exception);

   virtual void getInstCoordFrameId(LblApiIdItem_typ &);
   virtual const char *getInstrId() throw(exception) {
      return this->outputFileName_.instr;
   }

   double *c_, *a_, *h_, *v_;
   int siteFixed;

};

#endif            //M2020SKYCAMVICARLABEL_
