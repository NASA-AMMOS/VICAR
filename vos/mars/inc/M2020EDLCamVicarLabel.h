/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */

#ifndef M2020EDLCAMVICARLABEL_
#define M2020EDLCAMVICARLABEL_

#include "M2020EngCamVicarLabel.h"
#include "M2020EDLCamEdr.h"
//#include "lbl_scam_mini_header.h"

#include <memory>

class M2020EDLCamVicarLabel:public M2020EngCamVicarLabel {

      protected:

   M2020EDLCamEdr *edlEdr_;

      public:

   explicit M2020EDLCamVicarLabel(M2020ImgEdr * m);
    virtual ~ M2020EDLCamVicarLabel() {
   }
   //virtual const char* getProductName(); 
   virtual void getPlanetDayNumber(M2020DpMetaData * m);
   virtual void setProdName() throw(exception);
   virtual void setProdNameSample() throw(exception);
   virtual void setProdNameConfig(char) throw(exception);
   virtual void setProdNameProd() throw(exception);

   virtual void createLabels() throw(exception);
   virtual void initialize();
   virtual void writeLabels(string & labels) throw(exception) {
      M2020VicarLabel::writeLabels(labels);
   }
   //virtual void createEDLMiniHdrLabels() throw(exception);
   //virtual void writeEDLMiniHdrLabels(int) throw(exception);
   //virtual void readEDLMiniHdrLabels(int) throw(exception);

   virtual void writeLabels(int unit) throw(exception);
   virtual void readLabels(int unit) throw(exception);

   virtual void createIdentificationLabels() throw(exception);
   virtual void createTelemetryLabels() throw(exception);
   virtual void createPdsHistoryLabels() throw(exception);
   virtual void createVideoRequestLabels() throw(exception);
   virtual void createInstrumentStateLabels() throw(exception);
   virtual void createImageLabels() throw(exception);
   virtual void createProductRequestLabels() throw(exception);
   virtual void createCompressionLabels() throw(exception);

   //virtual const char *getInstrId() throw(exception) {
      //return this->outputFileName_.instr;
   //}

};

#endif            //M2020SUPERCAMVICARLABEL_
