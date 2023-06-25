/*
 * Copyright 2017-Present, California Institute of Technology. 
 * ALL RIGHTS RESERVED.
 * U.S. Government Sponsorship acknowledge.
 *
 * This is a temporary Pixl EDR generation software that simply takes
 * an engineering EDR labels and slaps them onto Pixl science frame
 * data
 *
 * @author Hyun Lee { Hyun.H.Lee@jpl.nasa.gov }
 * @version 1.0
 */
#ifndef M2020PIXLEDR_H_
#define M2020PIXLEDR_H_

#include "M2020Edr.h"
#include "M2020IdphHeader.h"
#include "M2020PixlPds.h"
#include "M2020PixlVicarLabel.h"
#include "M2020PixlIdph.h"
#include <list>
#include <vector>
#include <stdio.h>
#include <stdint.h>
#include <cstring>
using namespace std;

#ifndef HIST_HEADER_SIZE
#define HIST_HEADER_SIZE 321
#endif

#ifndef MCC_HEADER_SIZE
#define MCC_HEADER_SIZE 34
#endif

#ifndef SENSOR_HEAD_SIZE
#define SENSOR_HEAD_SIZE 32
#endif

#ifndef ROI_HEADER_SIZE
#define ROI_HEADER_SIZE 8
#endif

#ifndef SLI_HEADER_SIZE
#define SLI_HEADER_SIZE 26
#endif

#ifndef DP_HEADER_SIZE   // data piece header size
#define DP_HEADER_SIZE 8 
#endif

#ifndef CENTROIDS_SIZE
#define CENTROIDS_SIZE 10
#endif

#ifndef EVR_SIZE
#define EVR_SIZE 68
#endif

#ifndef HK_SIZE
#define HK_SIZE 160
#endif

// immediate data piece header size
#ifndef IM_DP_HEADER_SIZE
#define IM_DP_HEADER_SIZE 20
#endif

// retrieved data piece header size
#ifndef RET_DP_HEADER_SIZE
#define RET_DP_HEADER_SIZE 36
#endif

#define MAX_LEN        600000 // Max Size of an immediate piece should accomodate a MCC image, plus Housekeeping data. 
#define HIST_MAX       4096
//#define HIST_RAW_MAX   16388

//#define COMMA ","
//#define CRLF "\r\n"

typedef  int                 DEF_I32;
typedef  unsigned char       DEF_U08;
typedef  unsigned int        DEF_U32;

enum SCAN_LOG_TYPE {
   MCC_SLI_SpotList_BF = 0,
   MCC_SLI_SpotList_SF, //1
   Grand_Scan_Plan,     //2
   Scan_Log_MD,         //3
   Grand_Scan_Log       //4
};

typedef struct 
{
   int dacOffset;
   int dacGain;
   char integrationTime;
   char compression;
   char roi;
   char jpegQual;
   char compThreshold;
   char info;
   short valid;
   short status;
   unsigned int codeStart;
   unsigned int codeEnd;
   unsigned int sclkSub;
   unsigned int sclk;
   short imgFieldHeight;
   short imgHeight;
   short imgWidth;  
   short imod;
   bool imgMdAvailable;
} MccHeaderType;

typedef struct 
{
   int dpCategory;
   int rtt;
} MergeType;

typedef struct 
{
   unsigned int rtt;
   unsigned int pmc;
   unsigned int sclk;
   int scanLogType;
   double scanX;
   double scanY;
   double scanZ;
   int taskMask;
   int word1;
   int word2;
   int word3;
   int gvPmc;
   // added for PDP 34 (ScanLog)
   double xCenterBF;  
   double yCenterBF;
   double zCenterBF;
   double xPivot;
   double yPivot;
   double zPivot;
   double rFocus;
} ScanLogType;


typedef std::map <string, M2020IdphHeader *> IDPHS;
class M2020PixlVicarLabel;
class M2020PixlEdr:public M2020Edr {

   private:
      string EdrName_;
      IDPHS idphs_;
      vector <MergeType> mergeDPs_;
      sql::Connection *dbCon_;

   protected:
      M2020IdphHeader *hdr_ = NULL;

      M2020PixlPds *pdsLabel_;
      M2020PixlVicarLabel *label_;

      vector <DataDPO *> datasegs_;
      list <string> edrList_;

      unsigned int rawDataLength_;
      unsigned int fileSize_;
      unsigned char *sdfHeader_;
      unsigned int product_id_;
      unsigned int sclk_, minSclk_, maxSclk_;
      unsigned int rtt_;
      unsigned int pmc_;

      unsigned int totalDataOffset_;
      unsigned int nErrorPixels_;
      unsigned int pixOut_;
      float bytIn_;

      string format_;

      unsigned int _idphDPOPdsObjectPtr;
      unsigned int _idphDPOBytes;
      unsigned int _idphDPORows;
      unsigned int _idphDPORowBytes;

      M2020DPOParser *dpoParser_;
      M2020DPOMetaData *dpoMetaData_;

      // MCC IMAGE METADATA 
      bool sliCfgA_;
      bool sliCfgB_;
      short cycleType_;
      int ledA_, ledB_;
      int* pixlFliCurrent = new int[17];
      float* pixlFliDuration = new float[9];
      int* pixlSliCurrent = new int[2];
      float* pixlSliDuration = new float[2];
      float* bankVoltagesPre = new float[8];
      float* bankVoltagesPost = new float[8];
      float* temperature = new float[2];
      unsigned int* adcOffset = new unsigned int[2];

      bool v7Data_;

      virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser *);
      virtual void finalize() throw(exception);
      virtual void process() throw(exception);
      void processDPOs() throw(exception);
      void processIdph(int idphVid) throw(exception);
      void processPixlDP(unsigned char* compData, int runningDataLenTotal) throw(exception);
      void processMergeDPs() throw(exception);

      bool isHeader(const string & dpoName, int vid) const {
         TRACE;
         //  NUL as it's dpo parser.
         if (dpoName.find("ImfIdphSubset4") != std::string::npos)
            return true;
         else
            return false;
      } 

      virtual void updateUPF() throw(exception);

      // may need to throw excpetion
      void callUpdateOdlScript(string, string);

      void writeEdr(unsigned int edrData[], int length, int dpCategory) throw(exception);
      void writeEVRs(unsigned int edrData[], int length) throw(exception);
      void writeHK(unsigned int edrData[], int length, int sclk, int rtt, int pmc) throw(exception);
      void writeInventory(unsigned int edrData[], int length, int dpCategory) throw(exception);
      void writeFullHistogram(unsigned int edrData[], int length, int sclk, int pmc) throw(exception);
      void writeSummaryHistogram(unsigned int edrData[], int length, int sclk, int pmc) throw(exception);
      void writePseudointensity(unsigned int edrData[], int length, int sclk, int pmc) throw(exception);
      void writeScanLog(unsigned int edrData[], int length, int sclk, int pmc) throw(exception);

      // to merge
      void ingestEVRs(unsigned int edrData[], int length, int sclk, int rtt, int pmc) throw(exception);
      void writeEVRs(int rtt, int dpCategory) throw(exception);

      void ingestHK(unsigned int edrData[], int length, int sclk, int rtt, int pmc, int dpCategory) throw(exception);
      void writeHKs(int rtt, int dpCategory) throw(exception);

      void ingestRemark(unsigned int edrData[], int length, int sclk, int rtt, int pmc) throw(exception);
      void writeRemarks(int rtt, int dpCategory) throw(exception);
      
      void writeFullHistograms(int rtt, int dpCategory) throw(exception);
      void ingestFullHistogram(unsigned int edrData[], int length, int sclk, int rtt, int pmc, int dpCategory) throw(exception);

      void ingestPseudointensity(unsigned int edrData[], int length, int sclk, int rtt, int pmc, int dpCategory) throw(exception);
      void writePseudointensity(int rtt, int dpCategory) throw(exception);

      bool ingestGVScanLog(unsigned int edrData[], int length, int sclk, int rtt, int pmc) throw(exception);
      void ingestGVScanLogRecord(vector<ScanLogType> scanLogList);
      bool ingestScanLog(unsigned int edrData[], int length, int sclk, int rtt, int pmc) throw(exception);

      // need to remove this after testing ingestScanLogRecord with list object   
      void ingestGVScanLogRecord(int rtt, int pmc, int sclk, int scanLogType, float scanX, float scanY, float scanZ,
                               int taskMask, int word1, int word2, int word3, int gvPmc);
      void writeGVScanLog(int rtt, int dpCategory) throw(exception);

      void writeTRN(unsigned int edrData[], int length, int sclk, int pmc) throw(exception);
      void writeBitmap(unsigned int edrData[], int length, int dpCategory) throw(exception);
      void writeROIs(unsigned int edrData[], int length) throw(exception);
      void writeCentroids(unsigned int edrData[], int length) throw(exception);
      void writeFswSOH(unsigned int edrData[], int length) throw(exception);
      void writeImage(int dpCategory) throw(exception);
      void writeImageMetadata(unsigned char* imgMetadata, int metaSize);
      void writeJPEG(unsigned int edrData[], int length, int dpCategory) throw(exception);
      void writeSLIStruct(unsigned int edrData[], int length, int sclk, int pmc) throw(exception);

      void writeV7SummaryHistogram(unsigned int edrData[], int length, int sclk, int pmc) throw(exception);
      //void writeV7Edr(unsigned int edrData[], int length) throw(exception);

      MccHeaderType getMccHeader(unsigned int edrData[]) throw(exception);
      int getFliCurrent(int value);

      float intBitsToFloat(const unsigned int); 
      float getPointSlope(int dn, float slope, float offset);
      int getSignExtend16Bit(int input);
      float getSddLogFunc(int dn);
      float getLvcmLogFunc(int dn);

      bool inMergeDPs(int dpCategory, int rtt) ;
      
      void setM2020PixlVicarLabel(M2020PixlVicarLabel *label) {
         TRACE;
         this->label_ = label;
      } 

      M2020IdphHeader *getIdph(const string dpoName);
      M2020IdphHeader *getNavMapIdph(const string dpoName, int startIdx);

   public:
      M2020PixlEdr(M2020DpMetaData * m) throw(exception);
      ~ M2020PixlEdr();

      virtual void checkUPFParams() throw(exception);
      virtual void checkDataFile() throw(exception);
      virtual void init() throw(exception);
      virtual void setRawDataSize();
      virtual int decompressJPEG(unsigned char *, int, int) throw(exception);
      virtual unsigned int getProductId() const {
         return this->product_id_;
      } 
      virtual unsigned int getSclk() const {
         return this->sclk_;
      } 
      bool hasIdph(unsigned int rtt) throw(exception);
      unsigned int getRTT() {
         return this->rtt_;
      }
      unsigned int getPMC() {
         return this->pmc_;
      }
      IdphSet4 getIdphFromDB(unsigned int rtt, unsigned int sclk) throw(exception);

      bool isV7Data() {
         return this->v7Data_;
      }

      short getCycleType() {
         return this->cycleType_;
      }
      int* getPixlFliCurrent() {
         return this->pixlFliCurrent;
      }
      float* getPixlFliDuration() {
         return this->pixlFliDuration;
      }
      int* getPixlSliCurrent() {
         return this->pixlSliCurrent;
      }
      float* getPixlSliDuration() {
         return this->pixlSliDuration;
      }
      float* getPreVoltage() {
         return this->bankVoltagesPre;
      }
      float* getPostVoltage() {
         return this->bankVoltagesPost;
      }
      float* getTemp() {
         return this->temperature;
      }
      unsigned int* getAdcOffset() {
         return this->adcOffset;
      }
      bool getSliCfgA() {
         return this->sliCfgA_;
      }
      bool getSliCfgB() {
         return this->sliCfgB_;
      }
      int getLedA() {
         return this->ledA_;
      }
      int getLedB() {
         return this->ledB_;
      }

      M2020DPOMetaData *getDPOMetaData(int vid) const {
         return new M2020DPOMetaData(this->metadata_, vid);
      }
   
      static const std::string ImfIdph4HdrName;

};
#endif            //M2020PIXLEDR_H_
