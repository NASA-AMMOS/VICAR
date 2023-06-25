/*
 * Copyright 2017-Present, California Institute of Technology. 
 * ALL RIGHTS RESERVED.
 * U.S. Government Sponsorship acknowledge.
 *
 * This is a temporary Pixl PDS generation software that simply takes
 * an engineering EDR labels and slaps them onto Pixl science frame
 * data
 *
 * @author Hyun Lee { Hyun.H.Lee@jpl.nasa.gov }
 * @version 1.0
 */
#ifndef M2020PIXLPDS_H_
#define M2020PIXLPDS_H_

#include "M2020MainInclude.h"
#include "M2020DpMetaData.h"
#include "M2020DPOParser.h"
#include "M2020IdphHeader.h"
#include "M2020Edr.h"
#include "M2020PixlIdph.h"
using namespace std;

class M2020PixlEdr;
class M2020DpMetaData;
class M2020PixlDpMetaData;

#ifndef ERR_SIZE
#define ERR_SIZE 321
#endif

class M2020PixlPds {
   protected:
      M2020DpMetaData *_dpMeta;
      M2020Edr *_edr;
      M2020Env *_env;
      
      IdphSet4 idph_;
      bool hasIdph_;
      bool v7Data_;
      bool testMode_;

      string prodName_, instrId_, config_, venue_, prodType_, seqId_, who_, ver_, ext_;
      string specflag_;
      int site_, drive_;
      int dpCat_;
      int rtt_, pmc_;
      int jpegQual_;
      int pdpSize_;
      int pdpNvmUsn_;
      int pdpNvmChunks_;
      int pdpNvmBlock_;
      int pdpNvmPage_;

      // for HISTOGRAM EDR
      int histNumBins_;
      float realtime_;
      float livetimeDSPC_;
      int goodEvts_;
      int triggers_;
      int preampResets_;
      int overflows_;
      int underflows_;
      int baseEvts_;
      int saturates_;

      int histDataOffset_;
      int histNums_;
      
      string scSclkStCt_, scSclkStopCt_, startTime_, stopTime_;
      char *outFileName_;
      int planetDayNumber_;   // planet day number
      int planetDayNumberTemp_;
       
      const M2020IdphHeader *idphHdr_;

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

      int writeIdphKeywords();
      string getScetTime(string);
      static void toCharDec(unsigned int val, unsigned int maxStrSize, string & output) throw(exception);
      
   public:
      M2020PixlPds(M2020DpMetaData *, M2020Edr *, int);
      //explicit M2020PixlPds(M2020DpMetaData *, M2020Edr * edr, int dpCategory);
      ~M2020PixlPds();

      int datFileLen_, datFileRecs_, datFileRecsTmp_;
      string sclk_, sol_;

      void initialize(M2020Edr * edr, int dpCategory);
      void writeCommonKeywords(char *labelFilename, char *instrumentId, M2020DpMetaData *, int);
      void writeIlluminationKeywords(char *labelFilename, char *instrumentId, M2020DpMetaData *, int);

      void createOutFileName();
      const char *getOutFileName() {
         return outFileName_;
      };

      string getConfig() {
         return this->config_;
      }
      void setConfig(string filter) { 
         this->config_ = filter; 
      };
      void setJPEGQual(int qual) { this->jpegQual_ = qual; };
      void setSclk(string sclk) { this->sclk_ = sclk; };
      void setRTT(int rtt) { this->rtt_ = rtt; };
      void setPMC(int pmc) { this->pmc_ = pmc; };
      void setPdpSize(int size) { this->pdpSize_ = size; };
      void setPdpNvmUsn(int usn) { this->pdpNvmUsn_ = usn; };
      void setPdpNvmChunks(int chunks) { this->pdpNvmChunks_ = chunks; };
      void setPdpNvmBlock(int block) { this->pdpNvmBlock_ = block; };
      void setPdpNvmPage(int page) { this->pdpNvmPage_ = page; };
      
      void setHistNumBins(int bins) { this->histNumBins_ = bins; };
      void setRealtime(float rt) { this->realtime_ = rt; };
      void setLivetimeDSPC(float ltdspc) { this->livetimeDSPC_ = ltdspc; };
      void setGoodEvts(int evts) { this->goodEvts_ = evts; };
      void setTriggers(int triggers) { this->triggers_ = triggers; };
      void setPreampResets(int preamp) { this->preampResets_ = preamp; };
      void setOverflows(int of) { this->overflows_ = of; };
      void setUnderflows(int uf) { this->underflows_ = uf; };
      void setBaseEvts(int base) { this->baseEvts_ = base; };
      void setSaturates(int st) { this->saturates_ = st; };
      void setV7Data(bool v7) { this->v7Data_ = v7; };
      void setHistDataOffset(int offset) { this->histDataOffset_ = offset; };
      void setHistNums(int nums) { this->histNums_ = nums; };

      const char *getExtension() { return ext_.c_str(); };

      virtual void createLabels(int length) throw(exception);
      //static void getParam(char *name, void *value, int *count, int maxcnt, int length, void *clientData);
      string fixNullDouble(double val);

      int write(int);
      int setEdr(M2020Edr*);
      char *getPdsProductName();

};
#endif
