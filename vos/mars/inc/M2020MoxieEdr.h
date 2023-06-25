/*
       Copyright 2017-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Stirling Algermissen (Stirling.S.Algermissen@jpl.nasa.gov)
 */
#ifndef M2020MOXIEEDR_H_
#define M2020MOXIEEDR_H_

#include "M2020Edr.h"
#include "M2020EngCamEdr.h"
#include "M2020MoxiePds.h"
#include <vector>
#include <list>
using namespace std;

class M2020MoxiePds;

class M2020MoxieEdr:public M2020Edr {
   protected:

   unsigned int rawDataLength_;
   unsigned int fileSize_;
   M2020MoxiePds *pdsLabel_;
   M2020IdphHeader *hdr_;
   M2020IdphHeader *moxiAncHdr_;

    vector <DataDPO *> datasegs_;
    list <string> edrList_;

   virtual void init() throw(exception);
   virtual void setRawDataSize();

   virtual bool isHeader(const string & dpoName, int vid) const {
      TRACE;
      //  NUL as it's dpo parser.
      if (dpoName.find("DpoCidph") != std::string::npos || dpoName.find("ImfIdphSubset1") != std::string::npos)
         return true;
      else
         return false;
   } 
   virtual bool isAncHeader(const string & dpoName, int vid)const {
      return (dpoName.find("AncillarydataFrame") != std::string::npos);
   } 
   virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser * dpoParser);


      public:
   M2020MoxieEdr(M2020DpMetaData *) throw(exception);
   virtual ~ M2020MoxieEdr();

   virtual void process() throw(exception);
   virtual void checkUPFParams() throw(exception);
   virtual void checkDataFile() throw(exception);
   virtual void processDPOs() throw(exception);
   virtual void updateUPF() throw(exception);
   virtual void writeEdr(unsigned char *edrData, int length) throw(exception);
   virtual void finalize() throw(exception);
};

#endif            /*M2020MOXIEEDR_H_ */
