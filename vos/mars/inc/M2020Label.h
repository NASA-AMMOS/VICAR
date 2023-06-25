/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020LABEL_H_
#define M2020LABEL_H_

#include "M2020MainInclude.h"
#include "M2020DpMetaData.h"
#include "M2020InstrumentHeader.h"
#include "M2020Exception.h"

#define M20_NUM_COORD_CS_NAMES	10

class M2020Label {
      protected:
   M2020DpMetaData * metadata_;
   M2020InstrumentHeader *header_;
      public:
    explicit M2020Label() {
      TRACE;
   }
   virtual ~ M2020Label() {
      TRACE;
   }

   virtual void setM2020DpMetaData(M2020DpMetaData * metadata) throw(exception);
   virtual void setHeader(M2020InstrumentHeader * header) throw(exception);
   virtual const M2020InstrumentHeader *getHeader() const throw(exception);
   virtual void createLabels() throw(exception) = 0;
   virtual void writeLabels(string & labels) throw(exception) = 0;
   virtual const char *getDataSetID() = 0;
   virtual const char *getProductID() = 0;
   virtual const char *getApplicationProcessName() = 0;
   virtual const char *getInstrumentName() = 0;
   virtual const char *getProductName() = 0;

   static const char *no_yes[];
   static const char *false_true[];
   static const char *coord_sys_names[];
};

#endif            /*M2020LABEL_H_ */
