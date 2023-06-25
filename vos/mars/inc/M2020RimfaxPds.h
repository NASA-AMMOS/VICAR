/*
 * Copyright 2017-Present, California Institute of Technology. 
 * ALL RIGHTS RESERVED.
 * U.S. Government Sponsorship acknowledge.
 *
 * This is a temporary RIMFAX PDS generation software that simply takes
 * an engineering EDR labels and slaps them onto Pixl science frame
 * data
 *
 * @author Hyun Lee { Hyun.H.Lee@jpl.nasa.gov }
 * @version 1.0
 */
#ifndef M2020RIMFAXPDS_H_
#define M2020RIMFAXPDS_H_

#include "M2020MainInclude.h"
#include "M2020DpMetaData.h"
#include "M2020DPOParser.h"
#include "M2020Edr.h"
#include "M2020RimfaxEdr.h"
#include <set>
using namespace std;

#ifndef ERR_SIZE
#define ERR_SIZE 321
#endif

class M2020Edr;
class M2020DpMetaData;

class M2020RimfaxPds {

   private:
      string fname_sol_;
      string fname_sclk_;

   protected:
      int writeIdphKeywords();
      string getScetTime(string);
      static void toCharDec(unsigned int val, unsigned int maxStrSize, string & output) throw(exception);

   public:
      M2020DpMetaData * _dpMeta;
      M2020Edr *_edr;
      M2020Env *_env;

      string prodName_, instrId_, config_, venue_, prodType_, seqId_, who_, ver_, ext_, sclk_;
      int site_, drive_;
      string scSclkStCt_, scSclkStopCt_, startTime_, stopTime_;

      // for RIMFAX
      string specflag_, parmver_, parmnum_, bitsamp_, proctype_, compress_;
      int sep_, procver_;

      string sol_;
      string outFileName_;
      set<string> tlmSrcSet_;

      int planetDayNumber_;   // planet day number
      int datFileLen_, datFileRecs_, datFileRecsTmp_;

      int decimation_;

      u_char *_fileName;
      u_int _apid;
      u_short *_partNumber;
      u_int _totalParts;
      u_int _pktNumber;
      int _subType;
      u_int _seqVer;
      u_int _scftId;
      char *_minErt, *_maxErt;
      int _isSpice;
      bool _isHK;
      bool _mobility;
      int numSamples_;
      int numSoundings_;
      int numSweeps_;
      int lisNumSoundings_; 
      int minSclk_;
      int maxSclk_;
      bool priorToS7_;

      ~M2020RimfaxPds();
       M2020RimfaxPds(M2020DpMetaData *, char *instr);

      int write();
      int write(vector <DataDPO *> dpos);
      int setEdr(M2020Edr*);
      const char *getProductName();
      void writeCommonKeywords(char *labelFilename, char *instrumentId, M2020DpMetaData * m);

      virtual void initialize(M2020Edr * edr, char *instrumentId);
      void setProdSclk();

     /**
      * Call individual create methods.
      */
      string createOutFileName();
      void createLabels() throw(exception);

      M2020DpMetaData *getEmdMetaData() { return _dpMeta; };
      const char *getScSclkStCt(M2020DpMetaData * m) {
         return scSclkStCt_.c_str();
      };
      const char *getScSclkStopCt(M2020DpMetaData * m) {
         return scSclkStopCt_.c_str();
      };
      //const char *getStartTime(M2020DpMetaData * m) {
      //   return startTime_.c_str();
      //};
      const char *getStopTime(M2020DpMetaData * m) {
         return stopTime_.c_str();
      };

      void setProdType(string ptype) {
         prodType_ = ptype;
      }

      string fixNullDouble(double val);
};

#endif
