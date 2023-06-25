/*
  Copyright 2017-Present, California Institute of Technology. 
  ALL RIGHTS RESERVED.
  U.S. Government Sponsorship acknowledge.
*/
#ifndef M2020MOXIEPDS_H_
#define M2020MOXIEPDS_H_

#include "M2020MainInclude.h"
#include "M2020DpMetaData.h"
#include "M2020DPOParser.h"
#include "M2020Edr.h"
#include <set>
using namespace std;

#ifndef ERR_SIZE
#define ERR_SIZE 321
#endif

//#define IN_RANGE(val,min,max) ((val>=min && val<=max) ? true : false)

class M2020Edr;
class M2020DpMetaData;

class M2020MoxiePds {

      public:
   M2020DpMetaData * _dpMeta;
   M2020Edr *_edr;
   M2020Env *_env;

   string prodName_, instrId_, config_, venue_, prodType_, seqId_, who_, ver_, ext_, sclk_;
   int site_, drive_;
   string scSclkStCt_, scSclkStopCt_, startTime_, stopTime_;
   
   // for MOXIE
   string rct;

   //int sol_;
   string sol_;
   string mslFileName_;
   set<string> tlmSrcSet_;
   string specflag;
   string compress;
   

   int planetDayNumber_;   // planet day number
   int planetDayNumberTemp_;
   int datFileLen_, datFileRecs_, datFileRecsTmp_;

   u_char *_fileName;
   u_int _apid;
   u_short *_partNumber;
   u_int _totalParts;
   u_int _pktNumber;
   int _subType;
   //u_int _seqId;
   u_int _seqVer;
   u_int _scftId;
   char *_minErt, *_maxErt;
   int _isSpice;
   int _isOldDp;

   ~M2020MoxiePds();
    M2020MoxiePds(M2020DpMetaData *, char *instr);

   int write();
   int write(vector <DataDPO *> dpos);
   int setEdr(M2020Edr*);
   const char *getProductName();

   virtual void initialize(M2020Edr * edr, char *instrumentId);
   void writeCommonKeywords(char *labelFilename, char *instrumentId, M2020DpMetaData * m);
   const char *getMslFileName() {
      return mslFileName_.c_str();
   };

  /**
   * Call individual create methods.
   */
   void createMslFileName();

   void createLabels() throw(exception);
   M2020DpMetaData *getEmdMetaData() {
      return _dpMeta;
   };
   const char *getScSclkStCt(M2020DpMetaData * m) {
      return scSclkStCt_.c_str();
   };
   const char *getScSclkStopCt(M2020DpMetaData * m) {
      return scSclkStopCt_.c_str();
   };
   const char *getStartTime(M2020DpMetaData * m) {
      return startTime_.c_str();
   };
   const char *getStopTime(M2020DpMetaData * m) {
      return stopTime_.c_str();
   };

   string fixNullDouble(double val);
};

#endif
