/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020EDR_H_
#define M2020EDR_H_

#include "M2020Env.h"
#include "M2020DpMetaData.h"
#include "M2020Exception.h"

class M2020VicarLabel;

typedef struct {
   DPOMetaData md;
   unsigned int offsetInData;
   unsigned char *data;
} DataDPO;

class M2020Edr {
      protected:
   M2020Env * env_;
   M2020DpMetaData *metadata_;
   M2020VicarLabel *label_;
   //int edrType_;

   string dataFileName_;

   explicit M2020Edr(M2020DpMetaData *) throw(exception);
   virtual void setM2020VicarLabel(M2020VicarLabel * label);
   virtual void init() throw(exception) = 0;
   virtual void initializeImgData(unsigned int size) throw(exception) {
      TRACE;
      this->imgData_ = new unsigned char[size] ();
   } unsigned int imgHeight_, imgWidth_;
   unsigned char *imgData_;
   unsigned char *imgDataR_;
   unsigned char *imgDataG_;
   unsigned char *imgDataB_;

      public:

   virtual unsigned int getImgHeight() {
      return this->imgHeight_;
   }
   virtual unsigned int getImgWidth() {
      return this->imgWidth_;
   }
   virtual unsigned int getRawDataSize() {
      return (this->imgHeight_ * this->imgWidth_);
   }
   virtual string getDataFileName() {
      return dataFileName_;
   }

   //virtual int getEdrType() const { return edrType_; } 

   virtual void process() throw(exception) = 0;
   virtual void finalize() throw(exception) = 0;

   virtual void checkUPFParams() throw(exception);
   virtual ~ M2020Edr();
};
#endif            /*M2020EDR_H_ */
