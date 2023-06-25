/*
  Copyright 2008-Present, California Institute of Technology. 
  ALL RIGHTS RESERVED.
  U.S. Government Sponsorship acknowledge.
*/

#ifndef M2020EDLPDSLABEL_H_
#define M2020EDLPDSLABEL_H_

//#include "M2020Exception.h"

#include "M2020DpMetaData.h"
#include "M2020DPOParser.h"
#include "M2020IdphHeader.h"
//#include "M2020ChemCamEdr.h"
//#include "M2020CheminEdr.h"
//#include "M2020MMMCamEdr.h"
#include "M2020Edr.h"
#include "M2020ImgEdr.h"

using namespace std;

#define IN_RANGE(val,min,max) ((val>=min && val<=max) ? true : false)

class M2020EDLPdsLabel {
      protected:

   M2020Env * env_;
   M2020ImgEdr *edr_;
   M2020DpMetaData *md_;
   const M2020IdphHeader *srlcAncHdr_ = NULL;
   const M2020IdphHeader *srlcHdr_ = NULL;

      public:

   string prodName_, instrId_, config_, prodType_, seqId_, who_, ver_, ext_, sclk_;
   unsigned char venue_;
   int site_, drive_;
   string scSclkStCt_, scSclkStopCt_, startTime_, stopTime_;
   string edr_type_;

   //int sol_;
   string sol_;
   //string m2020FileName_;
   char *m2020FileName_;


   int planetDayNumber_;   // planet day number
   int planetDayNumberTemp_;
   int datFileLen_, datFileRecs_, datFileRecsTmp_;
   int experimentId_;   // For SAM only.

   explicit M2020EDLPdsLabel(M2020DpMetaData *, M2020ImgEdr * edr, char *instr, string edr_type);
   virtual ~ M2020EDLPdsLabel();

   virtual void initialize(M2020ImgEdr * edr, char *instrumentId);

   virtual void getPlanetDayNumber(M2020DpMetaData *);

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
};

#endif            /*M2020EDLPDSLABEL_H_ */
