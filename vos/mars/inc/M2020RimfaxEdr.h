/*
 * Copyright 2017-Present, California Institute of Technology. 
 * ALL RIGHTS RESERVED.
 * U.S. Government Sponsorship acknowledge.
 *
 * This is a temporary RIMFAX EDR generation software that simply takes
 * an engineering EDR labels and slaps them onto Pixl science frame
 * data
 *
 * @author Hyun Lee { Hyun.H.Lee@jpl.nasa.gov }
 * @version 1.0
 */
#ifndef M2020RIMFAXEDR_H_
#define M2020RIMFAXEDR_H_

#include "M2020Edr.h"
#include "M2020IdphHeader.h"
#include "M2020RimfaxPds.h"
#include <vector>
#include <list>
#include <stdio.h>
#include <stdint.h>
#include <cstring>
using namespace std;

#define HK_LENGTH 50
#define FM_SETUP_FILE_SIZE 130  // flight mode
#define TU_SETUP_FILE_SIZE 173  // testbed mode
#define IDPH_SET2_SIZE 92  
#define HK_PROD_TAG    160
#ifndef COMMA
#define COMMA ","
#endif

// 16MB
#define MAX_SOUNDING_SIZE 16000000

typedef struct 
{
   IDPH_U32 seconds;
   IDPH_U32 subseconds;
   IDPH_U32 sounding_count;
   IDPH_U8 setup_idx;
   IDPH_U8 config_id;
   IDPH_U8 calibration_mode;
   IDPH_U8 sounding_type;
   IDPH_U8 decimation_factor;
} RfaxAncSounding; // 17 bytes

typedef struct 
{
   IDPH_F64 avg_temp;
   IDPH_I8 avg_temp_status;
   IDPH_I32 pwr_switch_state;
   IDPH_F64 bcb1_rpam_a_i_eu;
   IDPH_F64 bcb2_rpam_a_i_eu;
   IDPH_F64 bcb1_rpam_b_i_eu;
   IDPH_F64 bcb2_rpam_b_i_eu;
} RfaxAncTempsAmps;  // 45 bytes

typedef struct 
{
   string activityFname;
   int rfax_group_spacing;  // will add in S7
   M2020IdphHeader* idph;
   RfaxAncSounding anc;
   unsigned char* soundingSDF;
   int sdfLen;
} RfaxSoundingDataType;

typedef struct 
{
   M2020IdphHeader* idph;
   RfaxAncTempsAmps ancTemp;
   unsigned char* sciEngSDF;
   int sdfLen;
} RfaxSciEngType;   // minimum 513 bytes 

// for cruise ancillary data
typedef struct
{
   IDPH_U32 sclk_seconds;
   IDPH_U32 sclk_subseconds;
   IDPH_F64 avg_temp;
   IDPH_U32 sounding_count;
   IDPH_U8 config_id;
   IDPH_U8 calibration_mode;
} RfaxAncDpo;

typedef struct 
{
    float value;
    string unit;
} keywordType;

typedef struct 
{
   keywordType sweepStartFreq;
   keywordType sweepStopFreq;
   keywordType sweepTime;
   keywordType numSweeps;
   keywordType numSamples;
   keywordType gateFreq;
   keywordType txDelay;
   keywordType rxDelay;
   keywordType txAttenuation;
   keywordType rxAttenuation;
   keywordType calibration;
   keywordType receiveOnly;
   keywordType configId;
   string setupFilename;
   string sinetableFilename;
} setupKewordStruct;

typedef struct 
{
   int configId;
   //int cmdCoarse;
   string productName;
   string pdsConfig;
   int decimationFactor;
} RfaxSummaryType;

class M2020RimfaxPds;
class M2020RimfaxEdr:public M2020Edr {

   protected:
      RfaxAncDpo *rfaxAnc;
  
      M2020RimfaxPds *pdsLabel_;
      M2020RimfaxPds *mdPdsLabel_;
      M2020DPOMetaData *dpoMetaData_;

      sql::Connection *dbCon_;

      unsigned int rawDataLength_;
      unsigned int fileSize_;
      char *setupData_;
      bool useSetupData_;
      bool useFM_;
      bool cruiseData_;
      bool oldSDF_;

      unsigned int config_;
      unsigned int cmdLaunchTimeCoarse_;

      string pname_;
      int productTag_;
      string activityFname;

      vector <DataDPO *> datasegs_;
      list <string> edrList_;
      setupKewordStruct setupKws_;
      //int numSamples_;
      //int numSoundings_;
      int lisNumSoundings_; 

      vector <RfaxSummaryType> rfaxSummaryTypes_;
      vector <RfaxSoundingDataType *> rfaxSoundings;
      vector <RfaxSciEngType *> rfaxSciEngsTemps;

      virtual void init() throw(exception);
      virtual void setRawDataSize();

      virtual bool isHeader(const string & dpoName, int vid) const {
         TRACE;
         //  NUL as it's dpo parser.
         if (dpoName.find("ImfIdph") != std::string::npos)
            return true;
         else
            return false;
      } 
     
      virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser * dpoParser);

      // may need to throw excpetion
      void callUpdateOdlScript(string, string);

      string findSetupDir(string setupDir, char *name);

      void parseSetupFile(unsigned int);
      bool parseXML(const char *, unsigned int);
      bool parseSurfaceXML(const char *, unsigned int);
      void parseSetupFileXML(unsigned int);
      void getSolFromSclk() throw(exception);
      RfaxAncSounding getAncSounding(unsigned char *edrData, int length) throw(exception);
      RfaxAncTempsAmps getAncTempAmps(unsigned char *data, int length) throw(exception);

      void ingestSoundingEdr(string dbTable) throw(exception);
      void ingestSoundingMetadata(RfaxSoundingDataType * sounding, int configId, const char* dpName, string dbTable) throw(exception);
      void writeSoundingEdr() throw(exception);

      void writeSurfaceEdr() throw(exception);
      void writeSurfaceSummaryEdr() throw(exception); // delete this after testing with DB
      void writeSurfaceSummaryEdrFromDB() throw(exception);
      void writeSurfaceSoundingEdrFromDB(string dbTable) throw(exception);

      void writeSurfaceHK() throw(exception);
      void computeRfaxAntennaValues(float rover_p[3], float rover_q[4], 
         double *rfax_antt_x, double *rfax_antt_y, double *rfax_antt_z, 
         double *rfax_antt_Az, double *rfax_antt_pitch, double *rfax_antt_roll);

      int getFileSize(string);

      string getPDSConfig(); 

      bool inRfaxSummaryTypes(int);
      string getRfaxSummaryProdName(int);

      IDPH_U8 getU8(unsigned char *data, int offset);
      IDPH_U16 getU16(unsigned char *data, int offset);
      IDPH_U32 getU32(unsigned char *data, int offset);
      IDPH_U64 getU64(unsigned char *data, int offset);

      IDPH_I8 getI8(unsigned char *data, int offset);
      IDPH_I16 getI16(unsigned char *data, int offset);
      IDPH_I32 getI32(unsigned char *data, int offset);

      //IDPH_F32 getF32(unsigned char *data, int offset);
      IDPH_F64 getF64(unsigned char *data, int offset);

      double hexstr2double(const string& hexstr);

   public:
      M2020RimfaxEdr(M2020DpMetaData *) throw(exception);
      virtual ~ M2020RimfaxEdr();

      virtual void process() throw(exception);
      virtual void finalize() throw(exception);
      virtual void checkUPFParams() throw(exception);

      void checkDataFile() throw(exception);
      void processDPOs() throw(exception);
      void updateUPF() throw(exception);
      void writeEdr(unsigned char *edrData, int length) throw(exception);

      void processData();
      void processHK(unsigned char *edrData, int length) throw(exception);
      char * getSetupData() { return setupData_; }
      bool getUseSetupData() { return useSetupData_; }
      bool getUseFM() { return useFM_; }
      string getDPName() { return this->pname_; };
      unsigned int getConfigId() { return this->config_; };

      //int getNumSoundings() { return this->numSoundings_; };
      //int getNumSamples() { return this->numSamples_; };

      vector <RfaxSoundingDataType *> getSoundings() { return rfaxSoundings; }
      vector <RfaxSciEngType *> getAncEngs() { return rfaxSciEngsTemps; }

      setupKewordStruct getSetupKws() { return setupKws_; }
};

#endif            /*M2020RIMFAXEDR_H_ */
