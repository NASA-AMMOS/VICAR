/*
       Copyright 2017-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Stirling Algermissen (Stirling.S.Algermissen@jpl.nasa.gov)
 */
#ifndef M2020MEDAEDR_H_
#define M2020MEDAEDR_H_
#include "M2020Edr.h"
#include "M2020EngCamEdr.h"
#include "M2020MedaPds.h"
#include <vector>
#include <list>
#include <stdio.h>
#include <stdint.h>
#include <cstring>
#include "M2020Env.h"
#include <sstream>
#include <iomanip>
#include <boost/uuid/uuid.hpp>            // uuid class
#include <boost/uuid/uuid_generators.hpp> // generators
#include <boost/uuid/uuid_io.hpp>         // streaming operators etc.

// set the first n bits to 1, rest to 0
#define BITMASK1(n) ((1ULL << (n)) - 1ULL)
// set bits [k+1, n] to 1, rest to 0
#define BITMASK(n, k) ((BITMASK1(n) >> k) << k)


using namespace std;

class M2020MedaPds;

class M2020MedaEdr:public M2020Edr {
   protected:
  string lastDbTableInsert = "";
  int lastDbTableInsertChannel = -1;
  bool closeLastInsert = false;
  sql::PreparedStatement *lastInsertStatement;
  
  string lastDbTableUpdate = "";
  int lastDbTableUpdateChannel = -1;
  bool closeLastUpdate = false;
  sql::PreparedStatement *lastUpdateStatement;
  
  sql::PreparedStatement *lastSelectStatement;

  unsigned int rawDataLength_;
  unsigned int fileSize_;
  M2020MedaPds *pdsLabel_;
  M2020IdphHeader *hdr_;
  M2020IdphHeader *medaAncHdr_;

  boost::uuids::random_generator generator;
  
  std::map<std::string, std::vector<string>> sensorTableToSol;
  
  vector <DataDPO *> datasegs_;
  list <string> edrList_;
  
  virtual void init() throw(exception);
  virtual void setRawDataSize();
  
  virtual bool isHeader(const string & dpoName, int vid) const {
    TRACE;
    //  NUL as it's dpo parser.
    if (dpoName.find("DpoCidph") != std::string::npos)
      return true;
    else
      return false;
  } 
  virtual bool isAncHeader(const string & dpoName, int vid)const {
    return (dpoName.find("AncillarydataFrame") != std::string::npos);
  } 
  virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser * dpoParser);
  virtual uint32_t fletcher32(unsigned char *data, size_t len, int offset);
  virtual void insertRecord(double acq_time, char* sourceProductId, int channelId, int value);
  //  virtual void insertRecord(double acq_time, char* sourceProductId, int channelId, string value, std::vector<std::vector<std::string>> lookupTable, string uuid);
  virtual void insertRecord(double acq_time, char* sourceProductId, int channelId, string value, std::vector<std::vector<std::string>> lookupTable, string uuid, string dataSource, int real_timestamp);
  virtual void writeMedaCsv(string dbTable, string sol);
  virtual void setInstrument(string dbTable, string sol);
  virtual string processErrorVal(unsigned char *data, std::vector<std::string> processArray);

  virtual string exec(const char* cmd);

 public:
  M2020MedaEdr(M2020DpMetaData *) throw(exception);
  virtual ~ M2020MedaEdr();
  
  virtual void process() throw(exception);
  virtual void checkUPFParams() throw(exception);
  virtual void checkDataFile() throw(exception);
  virtual void processDPOs() throw(exception);
  virtual void updateUPF() throw(exception);
  virtual void writeEdr(unsigned char *edrData, int length) throw(exception);
  virtual void finalize() throw(exception);
  //  template< typename T > string hexify(T i);
  template< typename T >
    std::string hexify(T i)
    {
      std::stringbuf buf;
      std::ostream os(&buf);
      // removed "0x" here before setfill due to MEDA request 
      os << std::setfill('0') << std::setw(sizeof(T) * 2)
	 << std::hex << i;
      
      return buf.str().c_str();
    }

  // which tables to enforce specific primary key ordering (EER products have multiple values per sclk)
  std::map<std::string, int> sensorToEer = {
    {"meda_ats", 0},
    {"meda_edac_def", 1},
    {"meda_edac_sef", 1},
    {"meda_flash_error", 1},
    {"meda_frangibolt_fire", 1},
    {"meda_hardfault_interrupt", 1},
    {"meda_hs", 0},
    {"meda_hs_maintenance", 0},
    {"meda_icu", 0},
    {"meda_ot_entry_fail", 1},
    {"meda_ps", 0},
    {"meda_rds", 0},
    {"meda_rds_telemetry", 1},
    {"meda_rds_uart_error", 1},
    {"meda_reset", 1},
    {"meda_rover_uart_error", 1},
    {"meda_skycam_uart_error", 1},
    {"meda_tirs", 0},
    {"meda_tirs_heaters_current_monitor", 1},
    {"meda_ws_asic_fail", 1},
    {"meda_ws_boom1", 0},
    {"meda_ws_boom2", 0},
    {"meda_ws_uart_error", 1},       
  };
  
  // map sensor table to select query for csv generation
  std::map<std::string, std::string> sensorToSelect = {
    {"meda_ws_boom1", "SELECT meda_ws_boom1.SCLK,"
     "meda_ws_boom1.LMST,"
     "meda_ws_boom1.LTST,"
     "meda_ws_boom1.sourceProductId,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_1,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_2,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_3,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_4,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_5,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_6,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_7,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_8,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_9,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_10,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_11,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_Sigma_Delta_12,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_1,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_2,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_3,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_4,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_5,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_6,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_7,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_8,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_9,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_LowGainCal_10,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_LowGainCal_50,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_LowGainCal_90,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_HighGainCal_10,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_HighGainCal_50,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_HighGainCal_90,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_DAC1,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_DAC2,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_DAC3,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_AVDD,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_IntTemp,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_1_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_2_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_3_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_4_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_5_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_6_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_7_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_8_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_ExtCh_9_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_LwGc_10_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_LwGc_50_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_LwGc_90_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_HhGc_10_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_HhGc_50_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_HhGc_90_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_DAC1_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_DAC2_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_DAC3_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_AVDD_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_IntTemp_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_WIND_DAC1,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_WIND_DAC2,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_WIND_DAC3,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_1,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_2,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_3,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_4,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_5,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_6,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_7,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_8,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_9,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_10,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_11,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_Sigma_Delta_12,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_1,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_2,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_3,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_4,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_5,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_6,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_7,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_8,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_9,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_LowGainCal_10,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_LowGainCal_50,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_LowGainCal_90,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_HighGainCal_10,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_HighGainCal_50,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_HighGainCal_90,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_DAC1,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_DAC2,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_DAC3,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_AVDD,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_IntTemp,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_1_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_2_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_3_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_4_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_5_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_6_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_7_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_8_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_ExtCh_9_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_LwGc_10_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_LwGc_50_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_LwGc_90_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_HhGc_10_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_HhGc_50_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_HhGc_90_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_DAC1_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_DAC2_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_DAC3_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_AVDD_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_IntTemp_status,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_WIND_DAC1,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_WIND_DAC2,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_WIND_DAC3,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_TR1_Ctrl_Alg_State,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_TR2_Ctrl_Alg_State,"
     "meda_ws_boom1.WS_BOOM1_ASIC1_TR3_Ctrl_Alg_State,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_TR1_Ctrl_Alg_State,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_TR2_Ctrl_Alg_State,"
     "meda_ws_boom1.WS_BOOM1_ASIC2_TR3_Ctrl_Alg_State"
     " FROM meda_ws_boom1 where sol="},
    {"meda_ws_boom2", "SELECT meda_ws_boom2.SCLK,"
     "meda_ws_boom2.LMST,"
     "meda_ws_boom2.LTST,"
     "meda_ws_boom2.sourceProductId,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_1,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_2,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_3,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_4,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_5,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_6,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_7,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_8,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_9,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_10,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_11,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_Sigma_Delta_12,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_1,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_2,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_3,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_4,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_5,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_6,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_7,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_8,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_9,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_LowGainCal_10,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_LowGainCal_50,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_LowGainCal_90,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_HighGainCal_10,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_HighGainCal_50,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_HighGainCal_90,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_DAC1,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_DAC2,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_DAC3,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_AVDD,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_IntTemp,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_1_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_2_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_3_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_4_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_5_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_6_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_7_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_8_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_ExtCh_9_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_LwGc_10_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_LwGc_50_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_LwGc_90_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_HhGc_10_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_HhGc_50_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_HhGc_90_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_DAC1_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_DAC2_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_DAC3_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_AVDD_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_IntTemp_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_WIND_DAC1,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_WIND_DAC2,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_WIND_DAC3,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_1,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_2,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_3,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_4,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_5,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_6,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_7,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_8,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_9,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_10,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_11,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_Sigma_Delta_12,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_1,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_2,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_3,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_4,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_5,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_6,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_7,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_8,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_9,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_LowGainCal_10,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_LowGainCal_50,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_LowGainCal_90,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_HighGainCal_10,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_HighGainCal_50,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_HighGainCal_90,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_DAC1,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_DAC2,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_DAC3,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_AVDD,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_IntTemp,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_1_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_2_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_3_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_4_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_5_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_6_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_7_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_8_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_ExtCh_9_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_LwGc_10_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_LwGc_50_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_LwGc_90_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_HhGc_10_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_HhGc_50_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_HhGc_90_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_DAC1_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_DAC2_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_DAC3_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_AVDD_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_IntTemp_status,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_WIND_DAC1,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_WIND_DAC2,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_WIND_DAC3,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_TR1_Ctrl_Alg_State,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_TR2_Ctrl_Alg_State,"
     "meda_ws_boom2.WS_BOOM2_ASIC1_TR3_Ctrl_Alg_State,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_TR1_Ctrl_Alg_State,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_TR2_Ctrl_Alg_State,"
     "meda_ws_boom2.WS_BOOM2_ASIC2_TR3_Ctrl_Alg_State"
     " FROM meda_ws_boom2 where sol="},
    {"meda_ats", "SELECT meda_ats.SCLK,"
      "meda_ats.LMST,"
      "meda_ats.LTST,"
      "meda_ats.sourceProductId,"
      "meda_ats.ATS_ATS1,"
      "meda_ats.ATS_ATS2,"
      "meda_ats.ATS_ATS3,"
      "meda_ats.ATS_ATS4,"
      "meda_ats.ATS_ATS5,"
      "meda_ats.ATS_ATS1_PRT,"
      "meda_ats.ATS_ATS2_PRT,"
      "meda_ats.ATS_ATS3_PRT,"
      "meda_ats.ATS_ATS4_PRT,"
      "meda_ats.ATS_ATS5_PRT"
      " FROM meda_ats where sol="},
     {"meda_hs", "SELECT meda_hs.SCLK,"
      "meda_hs.LMST,"		       
      "meda_hs.LTST,"		       
      "meda_hs.sourceProductId,"		       
      "meda_hs.HS_H_CHANNEL_CH1,"	       
      "meda_hs.HS_H_CHANNEL_CH2,"	       
      "meda_hs.HS_H_CHANNEL_CH3,"	       
      "meda_hs.HS_H_CHANNEL_CH4,"	       
      "meda_hs.HS_H_CHANNEL_CH5,"	       
      "meda_hs.HS_H_CHANNEL_CH6,"	       
      "meda_hs.HS_H_CHANNEL_CH7,"	       
      "meda_hs.HS_H_CHANNEL_CH8,"	       
      "meda_hs.HS_H_H_CONF1,"	       
      "meda_hs.HS_H_H_CONF2"
      " FROM meda_hs where sol="},
     {"meda_icu", "SELECT meda_icu.SCLK,"
      "meda_icu.LMST,"
      "meda_icu.LTST,"
      "meda_icu.sourceProductId,"
      "meda_icu.ICU_Analog_TH_HK_TM,"
      "meda_icu.ICU_Analog_H_PRT_TEMP_1,"
      "meda_icu.ICU_Analog_TIRS_ACQ_TEMP_TM,"	
      "meda_icu.ICU_Analog_5V_CURRENT_TM,"
      "meda_icu.ICU_Analog_8V_CURRENT_TM,"
      "meda_icu.ICU_Analog_11VN_CURRENT_TM,"
      "meda_icu.ICU_Analog_3V3_HK_MUX,"
      "meda_icu.ICU_Analog_POWER_H_SENSE,"
      "meda_icu.ICU_Analog_POWER_P1,"
      "meda_icu.ICU_Analog_R1_CAL_PRT,"
      "meda_icu.ICU_Analog_POWER_P2,"
      "meda_icu.ICU_Analog_R2_CAL_PRT,"
      "meda_icu.ICU_Analog_8V_FRANGI_TM,"
      "meda_icu.ICU_Analog_MEDA_RDS_TEMP,"
      "meda_icu.ICU_Analog_B1_SEC_PWR_TLM,"
      "meda_icu.ICU_Analog_CAM_PCB_PRT,"
      "meda_icu.ICU_Analog_B2_SEC_PWR_TLM,"
      "meda_icu.ICU_Analog_CAM_CCD_PRT,"
      "meda_icu.ICU_Analog_8V_H_PRT_TM,"
      "meda_icu.ICU_Analog_H_PRT_TEMP_2,"
      "meda_icu.ICU_Analog_12V_HTR_CURRENT_TM,"
      "meda_icu.ICU_Analog_WS1_PRT,"
      "meda_icu.ICU_Analog_WS2_PRT"
      " FROM meda_icu where sol="},
     {"meda_ps", "SELECT meda_ps.SCLK,"
      "meda_ps.LMST,"		       
      "meda_ps.LTST,"		       
      "meda_ps.sourceProductId,"		       
      "meda_ps.PS_P_CHANNEL_CH1,"	       
      "meda_ps.PS_P_CHANNEL_CH2,"	       
      "meda_ps.PS_P_CHANNEL_CH3,"	       
      "meda_ps.PS_P_CHANNEL_CH4,"	       
      "meda_ps.PS_P_CHANNEL_CH5,"	       
      "meda_ps.PS_P_CHANNEL_CH6,"
      "meda_ps.PS_P_CHANNEL_CH7,"
      "meda_ps.PS_P_CHANNEL_CH8,"
      "meda_ps.PS_PCONF_1,"
      "meda_ps.PS_PCONF_2"
      " FROM meda_ps where sol="
     },
     {"meda_rds", "SELECT meda_rds.SCLK,"
      "meda_rds.LMST,"
      "meda_rds.LTST,"
      "meda_rds.sourceProductId,"
      "meda_rds.RDS_CUMULATE_NUMBER,"
      "meda_rds.RDS_LAT_SE_1_PSTD,"
      "meda_rds.RDS_LAT_SE_1_ACCUM,"
      "meda_rds.RDS_LAT_SE_2_PSTD,"
      "meda_rds.RDS_LAT_SE_2_ACCUM,"
      "meda_rds.RDS_LAT_SE_3_PSTD,"
      "meda_rds.RDS_LAT_SE_3_ACCUM,"
      "meda_rds.RDS_LAT_SE_4_PSTD,"
      "meda_rds.RDS_LAT_SE_4_ACCUM,"
      "meda_rds.RDS_LAT_SE_5_PSTD,"
      "meda_rds.RDS_LAT_SE_5_ACCUM,"
      "meda_rds.RDS_LAT_SE_6_PSTD,"
      "meda_rds.RDS_LAT_SE_6_ACCUM,"
      "meda_rds.RDS_LAT_SE_7_PSTD,"
      "meda_rds.RDS_LAT_SE_7_ACCUM,"
      "meda_rds.RDS_LAT_SE_8_PSTD,"
      "meda_rds.RDS_LAT_SE_8_ACCUM,"
      "meda_rds.RDS_TOP_SE_1_PSTD,"
      "meda_rds.RDS_TOP_SE_1_ACCUM,"
      "meda_rds.RDS_TOP_SE_2_PSTD,"
      "meda_rds.RDS_TOP_SE_2_ACCUM,"
      "meda_rds.RDS_TOP_SE_3_PSTD,"
      "meda_rds.RDS_TOP_SE_3_ACCUM,"
      "meda_rds.RDS_TOP_SE_4_PSTD,"
      "meda_rds.RDS_TOP_SE_4_ACCUM,"
      "meda_rds.RDS_TOP_SE_5_PSTD,"
      "meda_rds.RDS_TOP_SE_5_ACCUM,"
      "meda_rds.RDS_TOP_SE_6_PSTD,"
      "meda_rds.RDS_TOP_SE_6_ACCUM,"
      "meda_rds.RDS_TOP_SE_7_PSTD,"
      "meda_rds.RDS_TOP_SE_7_ACCUM,"
      "meda_rds.RDS_TOP_SE_8_PSTD,"
      "meda_rds.RDS_TOP_SE_8_ACCUM,"
      "meda_rds.RDS_LAT_SE_1_HG_PSTD,"
      "meda_rds.RDS_LAT_SE_1_HG_ACCUM,"
      "meda_rds.RDS_LAT_SE_2_HG_PSTD,"
      "meda_rds.RDS_LAT_SE_2_HG_ACCUM,"
      "meda_rds.RDS_LAT_SE_3_HG_PSTD,"
      "meda_rds.RDS_LAT_SE_3_HG_ACCUM,"
      "meda_rds.RDS_LAT_SE_4_HG_PSTD,"
      "meda_rds.RDS_LAT_SE_4_HG_ACCUM,"
      "meda_rds.RDS_LAT_SE_5_HG_PSTD,"
      "meda_rds.RDS_LAT_SE_5_HG_ACCUM,"
      "meda_rds.RDS_LAT_SE_6_HG_PSTD,"
      "meda_rds.RDS_LAT_SE_6_HG_ACCUM,"
      "meda_rds.RDS_LAT_SE_7_HG_PSTD,"
      "meda_rds.RDS_LAT_SE_7_HG_ACCUM,"
      "meda_rds.RDS_LAT_SE_8_HG_PSTD,"
      "meda_rds.RDS_LAT_SE_8_HG_ACCUM,"
      "meda_rds.RDS_TOP_SE_1_HG_PSTD,"
      "meda_rds.RDS_TOP_SE_1_HG_ACCUM,"
      "meda_rds.RDS_TOP_SE_2_HG_PSTD,"
      "meda_rds.RDS_TOP_SE_2_HG_ACCUM,"
      "meda_rds.RDS_TOP_SE_3_HG_PSTD,"
      "meda_rds.RDS_TOP_SE_3_HG_ACCUM,"
      "meda_rds.RDS_TOP_SE_4_HG_PSTD,"
      "meda_rds.RDS_TOP_SE_4_HG_ACCUM,"
      "meda_rds.RDS_TOP_SE_5_HG_PSTD,"
      "meda_rds.RDS_TOP_SE_5_HG_ACCUM,"
      "meda_rds.RDS_TOP_SE_6_HG_PSTD,"
      "meda_rds.RDS_TOP_SE_6_HG_ACCUM,"
      "meda_rds.RDS_TOP_SE_7_HG_PSTD,"
      "meda_rds.RDS_TOP_SE_7_HG_ACCUM,"
      "meda_rds.RDS_TOP_SE_8_HG_PSTD,"
      "meda_rds.RDS_TOP_SE_8_HG_ACCUM,"
      "meda_rds.RDS_LAT_TMP_2_PSTD,"
      "meda_rds.RDS_LAT_TMP_2_ACCUM,"
      "meda_rds.RDS_LAT_TMP_4_PSTD,"
      "meda_rds.RDS_LAT_TMP_4_ACCUM,"
      "meda_rds.RDS_LAT_TMP_6_PSTD,"
      "meda_rds.RDS_LAT_TMP_6_ACCUM,"
      "meda_rds.RDS_LAT_TMP_8_PSTD,"
      "meda_rds.RDS_LAT_TMP_8_ACCUM,"
      "meda_rds.RDS_TOP_TMP_1_PSTD,"
      "meda_rds.RDS_TOP_TMP_1_ACCUM,"
      "meda_rds.RDS_TOP_TMP_3_PSTD,"
      "meda_rds.RDS_TOP_TMP_3_ACCUM,"
      "meda_rds.RDS_TOP_TMP_5_PSTD,"
      "meda_rds.RDS_TOP_TMP_5_ACCUM,"
      "meda_rds.RDS_TOP_TMP_7_PSTD,"
      "meda_rds.RDS_TOP_TMP_7_ACCUM,"
      "meda_rds.RDS_INT_SENSE_5V_IN_PSTD,"
      "meda_rds.RDS_INT_SENSE_5V_IN_ACCUM,"
      "meda_rds.RDS_INT_SENSE_5V_DIG_PSTD,"
      "meda_rds.RDS_INT_SENSE_5V_DIG_ACCUM,"
      "meda_rds.RDS_INT_TMP_DD_PSTD,"
      "meda_rds.RDS_INT_TMP_DD_ACCUM,"
      "meda_rds.RDS_INT_SE_DD_PSTD,"
      "meda_rds.RDS_INT_SE_DD_ACCUM,"
      "meda_rds.RDS_INT_REF_DD_PSTD,"
      "meda_rds.RDS_INT_REF_DD_ACCUM,"
      "meda_rds.RDS_INT_REF_TIA_PSTD,"
      "meda_rds.RDS_INT_REF_TIA_ACCUM,"
      "meda_rds.RDS_INT_TMP_PE_PSTD,"
      "meda_rds.RDS_INT_TMP_PE_ACCUM"
      " FROM meda_rds where sol="},
    {"meda_tirs", "SELECT meda_tirs.SCLK,"
     "meda_tirs.LMST,"
     "meda_tirs.LTST,"
     "meda_tirs.sourceProductId,"
     "meda_tirs.TIRS_TIRS1_avg,"
     "meda_tirs.TIRS_TIRS2_avg,"
     "meda_tirs.TIRS_TIRS3_avg,"
     "meda_tirs.TIRS_TIRS4_avg,"
     "meda_tirs.TIRS_TIRS5_avg,"
     "meda_tirs.TIRS_SUP_1_PLATE_PRT_avg,"
     "meda_tirs.TIRS_SUP_2_PLATE_PRT_avg,"
     "meda_tirs.TIRS_CAL_PLATE_PRT_avg,"
     "meda_tirs.TIRS_HEATER_PWM"
     " FROM meda_tirs where sol="},
    {"meda_reset", "SELECT `meda_reset`.`SCLK`,\n"
"    `meda_reset`.`LMST`,\n"
"    `meda_reset`.`LTST`,\n"
"    IF(`meda_reset`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_reset`.`sourceProductId`,\n"
"    `meda_reset`.`Data_Source`,\n"
"    `meda_reset`.`Reset_Source`,\n"
"    `meda_reset`.`SUSW_Test_Performed`,\n"
"    `meda_reset`.`SUSW_Test_Result`,\n"
"    `meda_reset`.`APSW_Test_Performed`,\n"
"    `meda_reset`.`APSW_Test_Result`,\n"
"    `meda_reset`.`Proc_RAM_test_first_fail_address`,\n"
"    `meda_reset`.`Proc_RAM_test_first_fail_expected_value`,\n"
"    `meda_reset`.`Proc_RAM_test_first_fail_value_read`,\n"
"    `meda_reset`.`PROM_expected_checksum`,\n"
"    `meda_reset`.`PROM_calculated_checksum`,\n"
"    `meda_reset`.`MMP_expected_checksum`,\n"
"    `meda_reset`.`MMP_calculated_checksum`,\n"
"    `meda_reset`.`MMP_Voted_Address`,\n"
"    `meda_reset`.`APSW_Image1_expected_checksum`,\n"
"    `meda_reset`.`APSW_Image1_calculated_checksum`,\n"
"    `meda_reset`.`APSW_Image1_Version`,\n"
"    `meda_reset`.`APSW_Image2_expected_checksum`,\n"
"    `meda_reset`.`APSW_Image2_calculated_checksum`,\n"
"    `meda_reset`.`APSW_Image2_Version`,\n"
"    `meda_reset`.`Default_APSW_image`,\n"
"    `meda_reset`.`APSW_RAM_Checksum_Test_Performed`,\n"
"    `meda_reset`.`APSW_RAM_Checksum_Test_Result`,\n"
"    `meda_reset`.`Executable_APSW_image`,\n"
"    `meda_reset`.`SP_expected_checksum`,\n"
"    `meda_reset`.`SP_calculated_checksum`,\n"
"    `meda_reset`.`Persistent_Data_expected_checksum`,\n"
"    `meda_reset`.`Persistent_Data_calculated_checksum`,\n"
"    `meda_reset`.`OT1_expected_checksum`,\n"
"    `meda_reset`.`OT1_calculated_checksum`,\n"
"    `meda_reset`.`OT2_expected_checksum`,\n"
"    `meda_reset`.`OT2_calculated_checksum`,\n"
"    `meda_reset`.`OT3_expected_checksum`,\n"
"    `meda_reset`.`OT3_calculated_checksum`,\n"
"    `meda_reset`.`OT4_expected_checksum`,\n"
"    `meda_reset`.`OT4_calculated_checksum`,\n"
"    `meda_reset`.`OT5_expected_checksum`,\n"
"    `meda_reset`.`OT5_calculated_checksum`,\n"
"    `meda_reset`.`OT6_expected_checksum`,\n"
"    `meda_reset`.`OT6_calculated_checksum`,\n"
"    `meda_reset`.`OT7_expected_checksum`,\n"
"    `meda_reset`.`OT7_calculated_checksum`,\n"
"    `meda_reset`.`OT8_expected_checksum`,\n"
"    `meda_reset`.`OT8_calculated_checksum`,\n"
"    `meda_reset`.`OT9_expected_checksum`,\n"
"    `meda_reset`.`OT9_calculated_checksum`,\n"
"    `meda_reset`.`OT10_expected_checksum`,\n"
"    `meda_reset`.`OT10_calculated_checksum`,\n"
"    `meda_reset`.`OT11_expected_checksum`,\n"
"    `meda_reset`.`OT11_calculated_checksum`,\n"
"    `meda_reset`.`OT12_expected_checksum`,\n"
"    `meda_reset`.`OT12_calculated_checksum`,\n"
"    `meda_reset`.`OT13_expected_checksum`,\n"
"    `meda_reset`.`OT13_calculated_checksum`,\n"
"    `meda_reset`.`OT14_expected_checksum`,\n"
"    `meda_reset`.`OT14_calculated_checksum`,\n"
"    `meda_reset`.`OT15_expected_checksum`,\n"
"    `meda_reset`.`OT15_calculated_checksum`,\n"
"    `meda_reset`.`OT16_expected_checksum`,\n"
"    `meda_reset`.`OT16_calculated_checksum`,\n"
"    `meda_reset`.`SUSW_EDAC_SEF_Counter`,\n"
"    `meda_reset`.`SUSW_Last_SEF_Address`,\n"
"    `meda_reset`.`SUSW_EDAC_DEF_Counter`,\n"
"    `meda_reset`.`SUSW_Last_DEF_Address`,\n"
"    `meda_reset`.`SUSW_Hardfault_Interrupt_Counter`,\n"
"    `meda_reset`.`SUSW_Last_Hardfault_Error_Address`,\n"
"    `meda_reset`.`SUSW_Hardfault_ID`,\n"
"    `meda_reset`.`SUSWMaxCycle`,\n"
"    `meda_reset`.`SUSWMaxLoad`,\n"
"    `meda_reset`.`APSWMaxCycle0`,\n"
"    `meda_reset`.`APSWMaxCycle1`,\n"
"    `meda_reset`.`APSWMaxCycle2`,\n"
"    `meda_reset`.`APSWMaxCycle3`,\n"
"    `meda_reset`.`APSWMaxCycle4`,\n"
"    `meda_reset`.`APSWMaxCycle5`,\n"
"    `meda_reset`.`APSWMaxCycle6`,\n"
"    `meda_reset`.`APSWMaxCycle7`,\n"
"    `meda_reset`.`APSWMaxCycle8`,\n"
"    `meda_reset`.`APSWMaxCycle9`,\n"
"    `meda_reset`.`APSWMaxLoad`\n"
     "FROM `m2020edrgen_meda`.`meda_reset` where sol="},
    {"meda_ot_entry_fail", "SELECT `meda_ot_entry_fail`.`SCLK`,\n"
"    `meda_ot_entry_fail`.`LMST`,\n"
"    `meda_ot_entry_fail`.`LTST`,\n"
"    IF(`meda_ot_entry_fail`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_ot_entry_fail`.`sourceProductId`,\n"
"    `meda_ot_entry_fail`.`Failure_ID`,\n"
"    `meda_ot_entry_fail`.`OT_Entry`,\n"
"    `meda_ot_entry_fail`.`Failure_Cause`\n"
     "FROM `m2020edrgen_meda`.`meda_ot_entry_fail` WHERE sol="},
    {"meda_rover_uart_error", "SELECT `meda_rover_uart_error`.`SCLK`,\n"
"    `meda_rover_uart_error`.`LMST`,\n"
"    `meda_rover_uart_error`.`LTST`,\n"
"    IF(`meda_rover_uart_error`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_rover_uart_error`.`Real_Timestamp`,\n"
"    `meda_rover_uart_error`.`sourceProductId`,\n"
"    `meda_rover_uart_error`.`Rover_UART_Rx_Status_ORed`,\n"
"    `meda_rover_uart_error`.`Rover_UART_Tx_Status_ORed`\n"
     "FROM `m2020edrgen_meda`.`meda_rover_uart_error` WHERE sol="},
    {"meda_ws_uart_error", "SELECT `meda_ws_uart_error`.`SCLK`,\n"
"    `meda_ws_uart_error`.`LMST`,\n"
"    `meda_ws_uart_error`.`LTST`,\n"
"    IF(`meda_ws_uart_error`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_ws_uart_error`.`sourceProductId`,\n"
"    `meda_ws_uart_error`.`WS_ASIC_ID`,\n"
"    `meda_ws_uart_error`.`WS_UART_Status`,\n"
"    `meda_ws_uart_error`.`BOOM_SwitchedOFF_flag`\n"
     "FROM `m2020edrgen_meda`.`meda_ws_uart_error` WHERE sol="},
    {"meda_rds_uart_error", "SELECT `meda_rds_uart_error`.`SCLK`,\n"
"    `meda_rds_uart_error`.`LMST`,\n"
"    `meda_rds_uart_error`.`LTST`,\n"
"    IF(`meda_rds_uart_error`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_rds_uart_error`.`sourceProductId`,\n"
"    `meda_rds_uart_error`.`RDS_UART_Status`,\n"
"    `meda_rds_uart_error`.`RDS_SwitchedOFF_flag`,\n"
"    `meda_rds_uart_error`.`RDS_TCResponse`,\n"
"    `meda_rds_uart_error`.`RDS_Timeout`\n"
     "FROM `m2020edrgen_meda`.`meda_rds_uart_error` WHERE sol="},
    {"meda_skycam_uart_error", "SELECT `meda_skycam_uart_error`.`SCLK`,\n"
"    `meda_skycam_uart_error`.`LMST`,\n"
"    `meda_skycam_uart_error`.`LTST`,\n"
"    IF(`meda_skycam_uart_error`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_skycam_uart_error`.`sourceProductId`,\n"
"    `meda_skycam_uart_error`.`SkyCam_UART_Status`\n"
     "FROM `m2020edrgen_meda`.`meda_skycam_uart_error` WHERE sol="},
    {"meda_rds_telemetry", "SELECT `meda_rds_telemetry`.`SCLK`,\n"
"    `meda_rds_telemetry`.`LMST`,\n"
"    `meda_rds_telemetry`.`LTST`,\n"
"    IF(`meda_rds_telemetry`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_rds_telemetry`.`sourceProductId`,\n"
"    `meda_rds_telemetry`.`RDS_Telecommand`,\n"
"    `meda_rds_telemetry`.`RDS_TM`\n"
     "FROM `m2020edrgen_meda`.`meda_rds_telemetry` WHERE sol="},
    {"meda_edac_sef", "SELECT `meda_edac_sef`.`SCLK`,\n"
"    `meda_edac_sef`.`LMST`,\n"
"    `meda_edac_sef`.`LTST`,\n"
"    IF(`meda_edac_sef`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_edac_sef`.`sourceProductId`,\n"
"    `meda_edac_sef`.`SEF_Address`\n"
     "FROM `m2020edrgen_meda`.`meda_edac_sef` WHERE sol="},
    {"meda_edac_def", "SELECT `meda_edac_def`.`SCLK`,\n"
"    `meda_edac_def`.`LMST`,\n"
"    `meda_edac_def`.`LTST`,\n"
"    IF(`meda_edac_def`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_edac_def`.`sourceProductId`,\n"
"    `meda_edac_def`.`DEF_Address`\n"
     "FROM `m2020edrgen_meda`.`meda_edac_def` WHERE sol="},
    {"meda_hardfault_interrupt", "SELECT `meda_hardfault_interrupt`.`SCLK`,\n"
"    `meda_hardfault_interrupt`.`LMST`,\n"
"    `meda_hardfault_interrupt`.`LTST`,\n"
"    IF(`meda_hardfault_interrupt`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_hardfault_interrupt`.`sourceProductId`,\n"
"    `meda_hardfault_interrupt`.`Error_Address`\n"
     "FROM `m2020edrgen_meda`.`meda_hardfault_interrupt` WHERE sol="},
    {"meda_frangibolt_fire", "SELECT `meda_frangibolt_fire`.`SCLK`,\n"
"    `meda_frangibolt_fire`.`LMST`,\n"
"    `meda_frangibolt_fire`.`LTST`,\n"
"    IF(`meda_frangibolt_fire`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_frangibolt_fire`.`sourceProductId`,\n"
"    `meda_frangibolt_fire`.`Firing_Time`,\n"
"    `meda_frangibolt_fire`.`Frangibolt_Temperature`\n"
     "FROM `m2020edrgen_meda`.`meda_frangibolt_fire` WHERE sol="},
    {"meda_hs_maintenance", "SELECT `meda_hs_maintenance`.`SCLK`,\n"
"    `meda_hs_maintenance`.`LMST`,\n"
"    `meda_hs_maintenance`.`LTST`,\n"
"    IF(`meda_hs_maintenance`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_hs_maintenance`.`sourceProductId`,\n"
"    `meda_hs_maintenance`.`HS_Initial_TempRange_Min`,\n"
"    `meda_hs_maintenance`.`HS_Initial_TempRange_Max`,\n"
"    `meda_hs_maintenance`.`HS_PRT_Heater_Timeout`,\n"
"    `meda_hs_maintenance`.`HS_Max_Temp`,\n"
"    `meda_hs_maintenance`.`Max_Value_H_PRT_TEMP_1`,\n"
"    `meda_hs_maintenance`.`Time_Start_Maintenance`,\n"
"    `meda_hs_maintenance`.`Time_End_Maintenance`,\n"
"    `meda_hs_maintenance`.`Maintenance_End_Status`\n"
     "FROM `m2020edrgen_meda`.`meda_hs_maintenance` WHERE sol="},
    {"meda_flash_error", "SELECT `meda_flash_error`.`SCLK`,\n"
"    `meda_flash_error`.`LMST`,\n"
"    `meda_flash_error`.`LTST`,\n"
"    IF(`meda_flash_error`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_flash_error`.`sourceProductId`,\n"
"    `meda_flash_error`.`Logical_block`,\n"
"    `meda_flash_error`.`Physical_block`,\n"
"    `meda_flash_error`.`Chip`,\n"
"    `meda_flash_error`.`Block_ID`,\n"
"    `meda_flash_error`.`Physical_Page`\n"
     "FROM `m2020edrgen_meda`.`meda_flash_error` WHERE sol="},
    {"meda_tirs_heaters_current_monitor", "SELECT `meda_tirs_heaters_current_monitor`.`SCLK`,\n"
"    `meda_tirs_heaters_current_monitor`.`LMST`,\n"
"    `meda_tirs_heaters_current_monitor`.`LTST`,\n"
"    IF(`meda_tirs_heaters_current_monitor`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_tirs_heaters_current_monitor`.`sourceProductId`,\n"
"    `meda_tirs_heaters_current_monitor`.`Reading_12V_HTR_CURRENT_TM`\n"
     "FROM `m2020edrgen_meda`.`meda_tirs_heaters_current_monitor` WHERE sol="},
    {"meda_ws_asic_fail", "SELECT `meda_ws_asic_fail`.`SCLK`,\n"
"    `meda_ws_asic_fail`.`LMST`,\n"
"    `meda_ws_asic_fail`.`LTST`,\n"
"    IF(`meda_ws_asic_fail`.`Real_Timestamp`, 'true', 'false') Real_Timestamp,\n"
"    `meda_ws_asic_fail`.`sourceProductId`,\n"
"    `meda_ws_asic_fail`.`WS_ASIC_ID`,\n"
"    `meda_ws_asic_fail`.`WS_Response_Status`\n"
     "FROM `m2020edrgen_meda`.`meda_ws_asic_fail` WHERE sol="}
   };
   
   
// Map channel numbers->instrument table, channel name
  std::vector<std::vector<std::string>> channelToDbLoc = {
     {"ICU_Analog_TH_HK_TM", "meda_icu"},
     {"ICU_Analog_H_PRT_TEMP_1", "meda_icu"},
     {"ICU_Analog_TIRS_ACQ_TEMP_TM", "meda_icu"},
     {"ICU_Analog_5V_CURRENT_TM", "meda_icu"},
     {"ICU_Analog_8V_CURRENT_TM", "meda_icu"},
     {"ICU_Analog_11VN_CURRENT_TM", "meda_icu"},
     {"ICU_Analog_3V3_HK_MUX", "meda_icu"},
     {"ICU_Analog_POWER_H_SENSE", "meda_icu"},
     {"ICU_Analog_POWER_P1", "meda_icu"},
     {"ICU_Analog_R1_CAL_PRT", "meda_icu"},
     {"ICU_Analog_POWER_P2", "meda_icu"},
     {"ICU_Analog_R2_CAL_PRT", "meda_icu"},
     {"ICU_Analog_8V_FRANGI_TM", "meda_icu"},
     {"ICU_Analog_MEDA_RDS_TEMP", "meda_icu"},
     {"ICU_Analog_B1_SEC_PWR_TLM", "meda_icu"},
     {"ICU_Analog_CAM_PCB_PRT", "meda_icu"},
     {"ICU_Analog_B2_SEC_PWR_TLM", "meda_icu"},
     {"ICU_Analog_CAM_CCD_PRT", "meda_icu"},
     {"ICU_Analog_8V_H_PRT_TM", "meda_icu"},
     {"ICU_Analog_H_PRT_TEMP_2", "meda_icu"},
     {"ICU_Analog_12V_HTR_CURRENT_TM", "meda_icu"},
     {"ICU_Analog_WS1_PRT", "meda_icu"},
     {"ICU_Analog_WS2_PRT", "meda_icu"},
     {"PS_P_CHANNEL_CH1", "meda_ps"},
     {"PS_P_CHANNEL_CH2", "meda_ps"},
     {"PS_P_CHANNEL_CH3", "meda_ps"},
     {"PS_P_CHANNEL_CH4", "meda_ps"},
     {"PS_P_CHANNEL_CH5", "meda_ps"},
     {"PS_P_CHANNEL_CH6", "meda_ps"},
     {"PS_P_CHANNEL_CH7", "meda_ps"},
     {"PS_P_CHANNEL_CH8", "meda_ps"},
     {"PS_PCONF_1", "meda_ps"},
     {"PS_PCONF_2", "meda_ps"},
     {"HS_H_CHANNEL_CH1", "meda_hs"},
     {"HS_H_CHANNEL_CH2", "meda_hs"},
     {"HS_H_CHANNEL_CH3", "meda_hs"},
     {"HS_H_CHANNEL_CH4", "meda_hs"},
     {"HS_H_CHANNEL_CH5", "meda_hs"},
     {"HS_H_CHANNEL_CH6", "meda_hs"},
     {"HS_H_CHANNEL_CH7", "meda_hs"},
     {"HS_H_CHANNEL_CH8", "meda_hs"},
     {"HS_H_H_CONF1", "meda_hs"},
     {"HS_H_H_CONF2", "meda_hs"},
     {"TIRS_TIRS1_avg", "meda_tirs"},
     {"TIRS_TIRS2_avg", "meda_tirs"},
     {"TIRS_TIRS3_avg", "meda_tirs"},
     {"TIRS_TIRS4_avg", "meda_tirs"},
     {"TIRS_TIRS5_avg", "meda_tirs"},
     {"TIRS_SUP_1_PLATE_PRT_avg", "meda_tirs"},
     {"TIRS_SUP_2_PLATE_PRT_avg", "meda_tirs"},
     {"TIRS_CAL_PLATE_PRT_avg", "meda_tirs"},
     {"TIRS_HEATER_PWM", "meda_tirs"},
     {"ATS_ATS1", "meda_ats"},
     {"ATS_ATS2", "meda_ats"},
     {"ATS_ATS3", "meda_ats"},
     {"ATS_ATS4", "meda_ats"},
     {"ATS_ATS5", "meda_ats"},
     {"ATS_ATS1_PRT", "meda_ats"},
     {"ATS_ATS2_PRT", "meda_ats"},
     {"ATS_ATS3_PRT", "meda_ats"},
     {"ATS_ATS4_PRT", "meda_ats"},
     {"ATS_ATS5_PRT", "meda_ats"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_1", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_2", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_3", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_4", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_5", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_6", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_7", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_8", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_9", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_10", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_11", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_Sigma_Delta_12", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_3", "meda_ws_boom1"}, // swapped with WS_BOOM1_ASIC1_ExtCh_1
     {"WS_BOOM1_ASIC1_ExtCh_2", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_1", "meda_ws_boom1"}, // swapped with WS_BOOM1_ASIC1_ExtCh_3
     {"WS_BOOM1_ASIC1_ExtCh_4", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_5", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_6", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_7", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_8", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_9", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_LowGainCal_10", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_LowGainCal_50", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_LowGainCal_90", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_HighGainCal_10", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_HighGainCal_50", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_HighGainCal_90", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_DAC1", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_DAC2", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_DAC3", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_AVDD", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_IntTemp", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_1_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_2_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_3_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_4_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_5_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_6_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_7_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_8_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_ExtCh_9_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_LwGc_10_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_LwGc_50_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_LwGc_90_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_HhGc_10_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_HhGc_50_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_HhGc_90_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_DAC1_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_DAC2_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_DAC3_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_AVDD_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_IntTemp_status", "meda_ws_boom1"},    
     {"WS_BOOM1_ASIC1_WIND_DAC1", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_WIND_DAC2", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_WIND_DAC3", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_1", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_2", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_3", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_4", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_5", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_6", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_7", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_8", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_9", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_10", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_11", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_Sigma_Delta_12", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_3", "meda_ws_boom1"}, // swapped with WS_BOOM1_ASIC2_ExtCh_1
     {"WS_BOOM1_ASIC2_ExtCh_2", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_1", "meda_ws_boom1"}, // swapped with WS_BOOM1_ASIC2_ExtCh_3
     {"WS_BOOM1_ASIC2_ExtCh_4", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_5", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_6", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_7", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_8", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_9", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_LowGainCal_10", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_LowGainCal_50", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_LowGainCal_90", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_HighGainCal_10", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_HighGainCal_50", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_HighGainCal_90", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_DAC1", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_DAC2", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_DAC3", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_AVDD", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_IntTemp", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_1_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_2_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_3_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_4_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_5_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_6_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_7_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_8_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_ExtCh_9_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_LwGc_10_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_LwGc_50_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_LwGc_90_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_HhGc_10_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_HhGc_50_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_HhGc_90_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_DAC1_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_DAC2_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_DAC3_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_AVDD_status", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_IntTemp_status", "meda_ws_boom1"},    
     {"WS_BOOM1_ASIC2_WIND_DAC1", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_WIND_DAC2", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_WIND_DAC3", "meda_ws_boom1"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_1", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_2", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_3", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_4", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_5", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_6", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_7", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_8", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_9", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_10", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_11", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_Sigma_Delta_12", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_3", "meda_ws_boom2"}, // swapped with WS_BOOM2_ASIC1_ExtCh_1
     {"WS_BOOM2_ASIC1_ExtCh_2", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_1", "meda_ws_boom2"}, // swapped with WS_BOOM2_ASIC1_ExtCh_3
     {"WS_BOOM2_ASIC1_ExtCh_4", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_5", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_6", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_7", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_8", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_9", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_LowGainCal_10", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_LowGainCal_50", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_LowGainCal_90", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_HighGainCal_10", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_HighGainCal_50", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_HighGainCal_90", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_DAC1", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_DAC2", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_DAC3", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_AVDD", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_IntTemp", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_1_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_2_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_3_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_4_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_5_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_6_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_7_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_8_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_ExtCh_9_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_LwGc_10_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_LwGc_50_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_LwGc_90_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_HhGc_10_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_HhGc_50_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_HhGc_90_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_DAC1_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_DAC2_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_DAC3_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_AVDD_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_IntTemp_status", "meda_ws_boom2"},    
     {"WS_BOOM2_ASIC1_WIND_DAC1", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_WIND_DAC2", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_WIND_DAC3", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_1", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_2", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_3", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_4", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_5", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_6", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_7", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_8", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_9", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_10", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_11", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_Sigma_Delta_12", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_3", "meda_ws_boom2"}, // swapped with WS_BOOM2_ASIC2_ExtCh_1
     {"WS_BOOM2_ASIC2_ExtCh_2", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_1", "meda_ws_boom2"}, // swapped with WS_BOOM2_ASIC2_ExtCh_3
     {"WS_BOOM2_ASIC2_ExtCh_4", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_5", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_6", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_7", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_8", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_9", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_LowGainCal_10", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_LowGainCal_50", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_LowGainCal_90", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_HighGainCal_10", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_HighGainCal_50", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_HighGainCal_90", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_DAC1", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_DAC2", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_DAC3", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_AVDD", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_IntTemp", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_1_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_2_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_3_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_4_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_5_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_6_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_7_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_8_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_ExtCh_9_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_LwGc_10_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_LwGc_50_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_LwGc_90_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_HhGc_10_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_HhGc_50_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_HhGc_90_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_DAC1_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_DAC2_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_DAC3_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_AVDD_status", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_IntTemp_status", "meda_ws_boom2"},    
     {"WS_BOOM2_ASIC2_WIND_DAC1", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_WIND_DAC2", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_WIND_DAC3", "meda_ws_boom2"},
     {"WS_BOOM1_ASIC1_TR1_Ctrl_Alg_State", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_TR2_Ctrl_Alg_State", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC1_TR3_Ctrl_Alg_State", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_TR1_Ctrl_Alg_State", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_TR2_Ctrl_Alg_State", "meda_ws_boom1"},
     {"WS_BOOM1_ASIC2_TR3_Ctrl_Alg_State", "meda_ws_boom1"},
     {"WS_BOOM2_ASIC1_TR1_Ctrl_Alg_State", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_TR2_Ctrl_Alg_State", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC1_TR3_Ctrl_Alg_State", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_TR1_Ctrl_Alg_State", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_TR2_Ctrl_Alg_State", "meda_ws_boom2"},
     {"WS_BOOM2_ASIC2_TR3_Ctrl_Alg_State", "meda_ws_boom2"},
     {"RDS_CUMULATE_NUMBER", "meda_rds"},
     {"RDS_LAT_SE_7_PSTD", "meda_rds"},
     {"RDS_LAT_SE_7_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_5_PSTD", "meda_rds"},
     {"RDS_LAT_SE_5_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_4_PSTD", "meda_rds"},
     {"RDS_LAT_SE_4_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_6_PSTD", "meda_rds"},
     {"RDS_LAT_SE_6_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_3_PSTD", "meda_rds"},
     {"RDS_LAT_SE_3_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_8_PSTD", "meda_rds"},
     {"RDS_LAT_SE_8_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_1_PSTD", "meda_rds"},
     {"RDS_LAT_SE_1_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_2_PSTD", "meda_rds"},
     {"RDS_LAT_SE_2_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_3_PSTD", "meda_rds"},
     {"RDS_TOP_SE_3_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_1_PSTD", "meda_rds"},
     {"RDS_TOP_SE_1_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_8_PSTD", "meda_rds"},
     {"RDS_TOP_SE_8_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_2_PSTD", "meda_rds"},
     {"RDS_TOP_SE_2_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_7_PSTD", "meda_rds"},
     {"RDS_TOP_SE_7_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_4_PSTD", "meda_rds"},
     {"RDS_TOP_SE_4_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_6_PSTD", "meda_rds"},
     {"RDS_TOP_SE_6_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_5_PSTD", "meda_rds"},
     {"RDS_TOP_SE_5_ACCUM", "meda_rds"},
     {"RDS_LAT_TMP_4_PSTD", "meda_rds"},
     {"RDS_LAT_TMP_4_ACCUM", "meda_rds"},
     {"RDS_LAT_TMP_2_PSTD", "meda_rds"},
     {"RDS_LAT_TMP_2_ACCUM", "meda_rds"},
     {"RDS_TOP_TMP_5_PSTD", "meda_rds"},
     {"RDS_TOP_TMP_5_ACCUM", "meda_rds"},
     {"RDS_TOP_TMP_3_PSTD", "meda_rds"},
     {"RDS_TOP_TMP_3_ACCUM", "meda_rds"},
     {"RDS_TOP_TMP_1_PSTD", "meda_rds"},
     {"RDS_TOP_TMP_1_ACCUM", "meda_rds"},
     {"RDS_LAT_TMP_8_PSTD", "meda_rds"},
     {"RDS_LAT_TMP_8_ACCUM", "meda_rds"},
     {"RDS_TOP_TMP_7_PSTD", "meda_rds"},
     {"RDS_TOP_TMP_7_ACCUM", "meda_rds"},
     {"RDS_LAT_TMP_6_PSTD", "meda_rds"},
     {"RDS_LAT_TMP_6_ACCUM", "meda_rds"},
     {"RDS_INT_SENSE_5V_IN_PSTD", "meda_rds"},
     {"RDS_INT_SENSE_5V_IN_ACCUM", "meda_rds"},
     {"RDS_INT_SENSE_5V_DIG_PSTD", "meda_rds"},
     {"RDS_INT_SENSE_5V_DIG_ACCUM", "meda_rds"},
     {"RDS_INT_TMP_DD_PSTD", "meda_rds"},
     {"RDS_INT_TMP_DD_ACCUM", "meda_rds"},
     {"RDS_INT_SE_DD_PSTD", "meda_rds"},
     {"RDS_INT_SE_DD_ACCUM", "meda_rds"},
     {"RDS_INT_REF_DD_PSTD", "meda_rds"},
     {"RDS_INT_REF_DD_ACCUM", "meda_rds"},
     {"RDS_INT_REF_TIA_PSTD", "meda_rds"},
     {"RDS_INT_REF_TIA_ACCUM", "meda_rds"},
     {"RDS_INT_TMP_PE_PSTD", "meda_rds"},
     {"RDS_INT_TMP_PE_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_7_HG_PSTD", "meda_rds"},
     {"RDS_LAT_SE_7_HG_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_5_HG_PSTD", "meda_rds"},
     {"RDS_LAT_SE_5_HG_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_4_HG_PSTD", "meda_rds"},
     {"RDS_LAT_SE_4_HG_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_6_HG_PSTD", "meda_rds"},
     {"RDS_LAT_SE_6_HG_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_3_HG_PSTD", "meda_rds"},
     {"RDS_LAT_SE_3_HG_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_8_HG_PSTD", "meda_rds"},
     {"RDS_LAT_SE_8_HG_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_1_HG_PSTD", "meda_rds"},
     {"RDS_LAT_SE_1_HG_ACCUM", "meda_rds"},
     {"RDS_LAT_SE_2_HG_PSTD", "meda_rds"},
     {"RDS_LAT_SE_2_HG_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_3_HG_PSTD", "meda_rds"},
     {"RDS_TOP_SE_3_HG_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_1_HG_PSTD", "meda_rds"},
     {"RDS_TOP_SE_1_HG_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_8_HG_PSTD", "meda_rds"},
     {"RDS_TOP_SE_8_HG_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_2_HG_PSTD", "meda_rds"},
     {"RDS_TOP_SE_2_HG_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_7_HG_PSTD", "meda_rds"},
     {"RDS_TOP_SE_7_HG_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_4_HG_PSTD", "meda_rds"},
     {"RDS_TOP_SE_4_HG_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_6_HG_PSTD", "meda_rds"},
     {"RDS_TOP_SE_6_HG_ACCUM", "meda_rds"},
     {"RDS_TOP_SE_5_HG_PSTD", "meda_rds"},
     {"RDS_TOP_SE_5_HG_ACCUM", "meda_rds"},
   };
   // resetOffsetToColumn
     //   std::string * resetOffsetToColumn[82][5] = {
   std::vector<std::vector<std::string>> resetOffsetToColumn = {
     {"Reset_Source", "meda_reset", "Unsigned_LE", "ASCII_String", "4"},
     {"SUSW_Test_Performed", "meda_reset", "Unsigned_LE", "ASCII_String", "4"},
     {"SUSW_Test_Result", "meda_reset", "Unsigned_LE", "ASCII_String", "4"},
     {"APSW_Test_Performed", "meda_reset", "Unsigned_LE", "ASCII_String", "4"},
     {"APSW_Test_Result", "meda_reset", "Unsigned_LE", "ASCII_String", "4"},
     {"Proc_RAM_test_first_fail_address", "meda_reset", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"Proc_RAM_test_first_fail_expected_value", "meda_reset", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"Proc_RAM_test_first_fail_value_read", "meda_reset", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"PROM_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"PROM_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"MMP_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"MMP_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"MMP_Voted_Address", "meda_reset", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"APSW_Image1_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"APSW_Image1_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"APSW_Image1_Version", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"APSW_Image2_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"APSW_Image2_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"APSW_Image2_Version", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"Default_APSW_image", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSW_RAM_Checksum_Test_Performed", "meda_reset", "Unsigned_LE", "ASCII_String", "4"},
     {"APSW_RAM_Checksum_Test_Result", "meda_reset", "Unsigned_LE", "ASCII_String", "4"},
     {"Executable_APSW_image", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"SP_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"SP_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"Persistent_Data_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"Persistent_Data_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"Spare", "meda_reset", "Ignored", "Ignored", "4"},
     {"Spare", "meda_reset", "Ignored", "Ignored", "4"},
     {"Spare", "meda_reset", "Ignored", "Ignored", "4"},
     {"OT1_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT1_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT2_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT2_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT3_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT3_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT4_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT4_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT5_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT5_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT6_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT6_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT7_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT7_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT8_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT8_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT9_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT9_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT10_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT10_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT11_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT11_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT12_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT12_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT13_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT13_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT14_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT14_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT15_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT15_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT16_expected_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"OT16_calculated_checksum", "meda_reset", "Unsigned_BE", "ASCII_Numeric_Base16", "4"},
     {"SUSW_EDAC_SEF_Counter", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"SUSW_Last_SEF_Address", "meda_reset", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"SUSW_EDAC_DEF_Counter", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"SUSW_Last_DEF_Address", "meda_reset", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"SUSW_Hardfault_Interrupt_Counter", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"SUSW_Last_Hardfault_Error_Address", "meda_reset", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"SUSW_Hardfault_ID", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"SUSWMaxCycle", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"SUSWMaxLoad", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle0", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle1", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle2", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle3", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle4", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle5", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle6", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle7", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle8", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxCycle9", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"},
     {"APSWMaxLoad", "meda_reset", "Unsigned_LE", "ASCII_Integer", "4"}
   };
   std::vector<std::vector<std::string>> otEntryFailOffsetToColumn = {
     {"Failure_ID", "meda_ot_entry_fail", "Unsigned_LE", "ASCII_Integer", "4"},
     {"OT_Entry", "meda_ot_entry_fail", "", "", "0"},
     {"Failure_Cause", "meda_ot_entry_fail", "", "", "0"}
   };
   std::vector<std::vector<std::string>> otEntryFailMapping = {
     {"Start_PS_Acq", "Start_PS_Acq on going"},
     {"Start_HS_Acq", "Start_HS_Acq on going"},
     {"HS_Maintenance", "Start_HS_Acq on going"},
     {"Start_TIRS_Acq", "Start_TIRS_Acq on going"},
     {"Start_TIRS_Support_Plate", "Start_TIRS_Support_Plate on going"},
     {"Start_WS_Acq", "Start_WS_Acq on going"},
     {"Start_RDS_Acq", "Start_RDS_Acq on going"},
     {"Start_RDS_Acq", "Start_RDS_HighGain on going"},
     {"Start_RDS_Acq", "Start_RDS_Debug on going"},
     {"Start_RDS_Acq", "After 3 switch ON tries failed"},
     {"Start_RDS_HighGain", "Start_RDS_Acq on going"},
     {"Start_RDS_HighGain", "Start_RDS_HighGain on going"},
     {"Start_RDS_HighGain", "Start_RDS_Debug on going"},
     {"Start_RDS_HighGain", "After 3 switch ON tries failed"},
     {"Start_RDS_Debug", "Start_RDS_Acq on going"},
     {"Start_RDS_Debug", "Start_RDS_HighGain on going"},
     {"Start_RDS_Debug", "Start_RDS_Debug on going"},
     {"Start_RDS_Debug", "After 3 switch ON tries failed"},
     {"SkyCam_Heater_SwitchON", "SkyCam ON"},
     {"SkyCam_SwitchON", "Skycam Temperature outside threshold range"},
     {"SkyCam_Heater_SwitchON", "Ongoing SkyCam acquisition initiated by TC"},
     {"SkyCam_Heater_SwitchOFF", "Ongoing SkyCam acquisition initiated by TC"},
     {"SkyCam_SwitchON", "Ongoing SkyCam acquisition initiated by TC"},
     {"SkyCam_SwitchOFF", "Ongoing SkyCam acquisition initiated by TC"},
     {"SkyCam_Acq", "Ongoing SkyCam acquisition initiated by TC"},
     {"HS_Maintenance", "Ongoing SkyCam acquisition initiated by TC"},
     {"TIRS_Cal_Heater", "Ongoing SkyCam acquisition initiated by TC"}
   };
   std::vector<std::vector<std::string>> roverUartOffsetToColumn = {
     {"Rover_UART_Rx_Status_ORed", "meda_rover_uart_error", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"Rover_UART_Tx_Status_ORed", "meda_rover_uart_error", "Unsigned_LE", "ASCII_Numeric_Base16", "4"}
   };
   std::vector<std::vector<std::string>> wsUartOffsetToColumn = {
     {"WS_ASIC_ID", "meda_ws_uart_error", "Unsigned_LE", "ASCII_Integer", "4"},
     {"WS_UART_Status", "meda_ws_uart_error", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"BOOM_SwitchedOFF_flag", "meda_ws_uart_error", "Unsigned_LE", "ASCII_Integer", "4"}
   };
   std::vector<std::vector<std::string>> rdsUartOffsetToColumn = {
     {"RDS_UART_Status", "meda_rds_uart_error", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
     {"RDS_SwitchedOFF_flag", "meda_rds_uart_error", "Unsigned_LE", "ASCII_Integer", "4"},
     {"RDS_TCResponse", "meda_rds_uart_error", "ByteArray", "ASCII_Numeric_Base16", "7"},
     {"RDS_Timeout", "meda_rds_uart_error", "Unsigned", "ASCII_Integer", "1"}
   };
   std::vector<std::vector<std::string>> skycamUartOffsetToColumn = {
     {"SkyCam_UART_Status", "meda_skycam_uart_error", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
   };
   std::vector<std::vector<std::string>> rdsTelemetryToColumn = {
     {"RDS_Telecommand", "meda_rds_telemetry", "ByteArray", "ASCII_Numeric_Base16", "8"},
     {"RDS_TM", "meda_rds_telemetry", "ByteArray", "ASCII_Numeric_Base16", "163"},
     {"Spare", "meda_rds_telemetry", "Ignored", "Ignored", "1"},
   };
   std::vector<std::vector<std::string>> edacSefToColumn = {
     {"SEF_Address", "meda_edac_sef", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
   };
   std::vector<std::vector<std::string>> edacDefToColumn = {
     {"DEF_Address", "meda_edac_def", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
   };
   std::vector<std::vector<std::string>> hardfaultInterruptToColumn = {
     {"Error_Address", "meda_hardfault_interrupt", "Unsigned_LE", "ASCII_Numeric_Base16", "4"},
   };
   std::vector<std::vector<std::string>> frangiboltFireToColumn = {
     {"Firing_Time", "meda_frangibolt_fire", "Unsigned_LE", "ASCII_Integer", "4"},
     {"Frangibolt_Temperature", "meda_frangibolt_fire", "Unsigned_LE", "ASCII_Integer", "4"},
     {"Spare", "meda_frangibolt_fire", "Ignored", "Ignored", "2"},
   };
   std::vector<std::vector<std::string>> hsMaintenanceToColumn = {
     {"HS_Initial_TempRange_Min", "meda_hs_maintenance", "Unsigned_LE", "ASCII_Integer", "2"},
     {"HS_Initial_TempRange_Max", "meda_hs_maintenance", "Unsigned_LE", "ASCII_Integer", "2"},
     {"HS_PRT_Heater_Timeout", "meda_hs_maintenance", "Unsigned_LE", "ASCII_Integer", "4"},
     {"HS_Max_Temp", "meda_hs_maintenance", "Unsigned_LE", "ASCII_Integer", "2"},
     {"Max_Value_H_PRT_TEMP_1", "meda_hs_maintenance", "Unsigned_LE", "ASCII_Integer", "2"},
     {"Time_Start_Maintenance", "meda_hs_maintenance", "Unsigned_LE", "ASCII_Integer", "4"},
     {"Time_End_Maintenance", "meda_hs_maintenance", "Unsigned_LE", "ASCII_Integer", "4"},
     {"Maintenance_End_Status", "meda_hs_maintenance", "Unsigned_LE", "ASCII_String", "4"}
   };
   std::vector<std::vector<std::string>> flashErrorToColumn = {
     {"Flash_Error_Type", "meda_flash_error", "Unsigned_LE", "ASCII_String", "2"},
     {"Logical_block", "meda_flash_error", "Unsigned_LE", "ASCII_Numeric_Base16", "2"},
     {"Physical_block", "meda_flash_error", "Unsigned_LE", "ASCII_Numeric_Base16", "2"},
     {"Chip", "meda_flash_error,Unsigned_LE", "ASCII_Numeric_Base16", "2"},
     {"Block_ID", "meda_flash_error,Unsigned_LE", "ASCII_Numeric_Base16", "2"},
     {"Physical_Page", "meda_flash_error,Unsigned_LE", "ASCII_Numeric_Base16", "2"}
   };
   std::vector<std::vector<std::string>> tirsHeatersCurrentMonitorToColumn = {
     {"Reading_12V_HTR_CURRENT_TM", "meda_tirs_heaters_current_monitor", "Unsigned_LE", "ASCII_Integer", "2"},
     {"Spare", "meda_tirs_heaters_current_monitor", "Ignored", "Ignored", "2"},     
   };
   std::vector<std::vector<std::string>> wsAsicFailToColumn = {
     {"WS_ASIC_ID", "meda_ws_asic_fail", "Unsigned_LE", "ASCII_Integer", "4"},
     {"WS_Response_Status", "meda_ws_asic_fail", "Unsigned_LE", "ASCII_Numeric_Base16", "4"}     
   };
   std::vector<std::vector<std::vector<std::string>>> medaErrorLookup = {
     resetOffsetToColumn,
     otEntryFailOffsetToColumn,
     roverUartOffsetToColumn,
     wsUartOffsetToColumn,
     rdsUartOffsetToColumn,
     skycamUartOffsetToColumn,
     rdsTelemetryToColumn,
     edacSefToColumn,
     edacDefToColumn,
     hardfaultInterruptToColumn,
     frangiboltFireToColumn,
     hsMaintenanceToColumn,
     flashErrorToColumn,
     tirsHeatersCurrentMonitorToColumn,
     wsAsicFailToColumn
   };
   int eventIdToSize [15] = { 328, 4, 8, 12, 16, 4, 172, 4, 4, 4, 8, 24, 12, 4, 8 }; 
};

#endif            /*M2020MEDAEDR_H_ */
