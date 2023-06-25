/*
  Copyright 2008-Present, California Institute of Technology. 
  ALL RIGHTS RESERVED.
  U.S. Government Sponsorship acknowledge.
*/

/**
 * @author  Hyun Lee {Hyun.Lee@jpl.nasa.gov}
 */
#ifndef M2020PIXLVICARLABEL_H_
#define M2020PIXLVICARLABEL_H_

#include "lbl_identification.h"
#include "lbl_telemetry.h"
#include "lbl_instrument_state.h"
#include "lbl_pdshistory.h"
#include "lbl_image_data.h"
#include "lbl_compression.h"
#include "lbl_product_request.h"
#include "lbl_articulation.h"
#include "lbl_coordinate.h"
#include "lbl_observation_request.h"
#include "lbl_camera_model.h"
#include "lbl_initstate.h"
//#include "lbl_pixl_mcc_img_parms.h"
#include "lbl_illumination_parms.h"

#include "PigCameraModel.h"
#include "PigCoordSystem.h"
#include "PigMission.h"

#include "M2020Label.h"
#include "M2020DPOParser.h"
#include "M2020PixlEdr.h"
#include "M2020DpMetaData.h"
#include "M2020PixlIdph.h"

#ifndef ERR_SIZE
#define ERR_SIZE 321
#endif

#define IN_RANGE(val,min,max) ((val>=min && val<=max) ? true : false)
#define M2020_ARM_COORDINATE_SYSTEM_PROPERTY_NAME "ARM_COORDINATE_SYSTEM"
#define M2020_LOCAL_LEVEL_COORDINATE_SYSTEM_PROPERTY_NAME "LOCAL_LEVEL_COORDINATE_SYSTEM"
#define M2020_RSM_COORDINATE_SYSTEM_PROPERTY_NAME "RSM_COORDINATE_SYSTEM"
#define M2020_DRILL_COORDINATE_SYSTEM_PROPERTY_NAME "DRILL_COORDINATE_SYSTEM"
#define M2020_ROVER_COORDINATE_SYSTEM_PROPERTY_NAME "ROVER_COORDINATE_SYSTEM"
#define M2020_SITE_COORDINATE_SYSTEM_PROPERTY_NAME "SITE_COORDINATE_SYSTEM"
#define M2020_SAVED_FRAME_COORDINATE_SYSTEM_PROPERTY_NAME "SAVED_FRAME_COORDINATE_SYSTEM"
#define M2020_TURRET_COORDINATE_SYSTEM_PROPERTY_NAME "TURRET_COORDINATE_SYSTEM"
#define M2020_TOOL_COORDINATE_SYSTEM_PROPERTY_NAME "TOOL_COORDINATE_SYSTEM"
#define M2020_PIXL_SENSOR_COORDINATE_SYSTEM_PROPERTY_NAME "PIXL_SENSOR_COORDINATE_SYSTEM"

#define M2020_CHASSIS_ARTICULATION_STATE_PROPERTY_NAME "CHASSIS_ARTICULATION_STATE"
#define M2020_FILTER_ARTICULATION_STATE_PROPERTY_NAME "FILTER_ARTICULATION_STATE"
#define M2020_HGA_ARTICULATION_STATE_PROPERTY_NAME "HGA_ARTICULATION_STATE"
#define M2020_SHA_ARTICULATION_STATE_PROPERTY_NAME "SHA_ARTICULATION_STATE"
#define M2020_SCS_ARTICULATION_STATE_PROPERTY_NAME "SCS_ARTICULATION_STATE"
#define M2020_DRILL_ARTICULATION_STATE_PROPERTY_NAME "DRILL_ARTICULATION_STATE"
#define M2020_ARM_ARTICULATION_STATE_PROPERTY_NAME "ARM_ARTICULATION_STATE"
#define M2020_RSM_ARTICULATION_STATE_PROPERTY_NAME "RSM_ARTICULATION_STATE"
#define M2020_INIT_STATE_PARAMS_PROPERTY_NAME "INITIAL_STATE_PARMS"

class M2020PixlEdr;
class M2020DpMetaData;
class M2020PixlDpMetaData;
class M2020PixlVicarLabel: public M2020Label {
      protected:

   M2020Env * env_;
   M2020PixlPds *pdsLabel_;

   IdphSet4 idph_;
   bool hasIdph_;
   bool testMode_;

   int imgHeight_;
   int imgWidth_;
   unsigned char roi_;
   unsigned char cmprsThreshold_;
   unsigned char info_;
   int valid_;
   int status_;
   int codeStart_;
   int codeEnd_;
   int mccSubTime_;
   int mccTime_;
   int imod_;
   int imgFH_;
   bool imgMDAvail_;
   int cmprsMode_;                  // compression
   float cmprsQual_;                // jpeg_qual
   int gainNumber_;                 // dac_gain
   int offsetNumber_;               // dac_offset
   unsigned int exposureDuration_;  // shutter time from image metatdata
   int integrationTime_;            // integration time
   int firstLine_;
   int firstLineSample_;
   int lines_;
   int lineSamples_;

   int *pixlFliCurrent_;
   float* pixlFliDuration_;
   int* pixlSliCurrent_;
   float* pixlSliDuration_;
   int cycleType_;
   float* flashTemp_;
   unsigned int* adcOffset_;
   float* preFlashVolt_;
   float* postFlashVolt_;
   bool sliCfgA_, sliCfgB_;
   int ledA_, ledB_;
   int dpCategory_;
   string config_;
   unsigned char* imgMD_;

   float sensorPos_[3] = { 0.0 };
   float sensorQuat_[4] = { 0.0 };

   LblIdentification_typ idElem_;
   LblTelemetry_typ idTelem_;
   LblPdsHistory_typ idHist_;

   LblIlluminationParms_typ idLedIllumParms_;
   LblIlluminationParms_typ idSliIllumParms_;

   LblCoordinate_typ idRoverCoor_;
   LblCoordinate_typ idSiteCoor_;
   LblCoordinate_typ idArmCoor_;
   LblCoordinate_typ idRsmCoor_;
   LblCoordinate_typ idTurretCoor_;
   LblCoordinate_typ idToolCoor_;
   LblCoordinate_typ idSensorCoor_;

   LblArticulation_typ idRsmArt_;
   LblArticulation_typ idArmArt_;
   LblArticulation_typ idChasArt_;
   LblArticulation_typ idHgaArt_;

   LblInstrumentState_typ idInstState_;
   LblImageData_typ idImg_;

   LblObsRequest_typ idObsReq_;

   LblCompression_typ idCmprs_;
   LblInitState_typ idInitStateParms_;

   LblCameraModel_typ idGeoCamMdl_;

   string dataSetId_, dataSetName_, instName_, prodId_, appProsName_, instId_, prodName_;

   PigCameraModel *camMdl_;
   PigCoordSystem *pixlSensorCS_;
   PigMission *m_;

   M2020PixlEdr *edr_;
   bool v7Data_;

   virtual void initialize();

   virtual void createIdentificationLabels() throw(exception);
   virtual void createTelemetryLabels() throw(exception);
   virtual void createPdsHistoryLabels() throw(exception);
   virtual void createCoordinateLabels() throw(exception);

   virtual void createRsmArticulationLabels() throw(exception);
   virtual void createArmArticulationLabels() throw(exception);
   virtual void createChasArticulationLabels() throw(exception);
   virtual void createHgaArticulationLabels() throw(exception);
   virtual void createInstrumentStateLabels() throw(exception);
   virtual void createObservationRequestLabels() throw(exception);
   virtual void createImageLabels() throw(exception);
   virtual void createCompressionLabels() throw(exception);
   virtual void createGeometricCameraModelLabels() throw(exception);
   void createIlluminationParmsLabels() throw (exception);
   //void createPixlMccImgParmsLabels() throw (exception);
   static void getParam(char *name, void *value, int *count, int maxcnt, int length, void *clientData);

   virtual void writeIdentificationLabels(int unit) throw(exception);
   virtual void writeTelemetryLabels(int unit) throw(exception);
   virtual void writePdsHistoryLabels(int unit) throw(exception);
   virtual void writeCoordinateLabels(int unit) throw(exception);
   virtual void writeRsmArticulationLabels(int unit) throw(exception);
   virtual void writeArmArticulationLabels(int unit) throw(exception);
   virtual void writeChasArticulationLabels(int unit) throw(exception);
   virtual void writeHgaArticulationLabels(int unit) throw(exception);
   virtual void writeObservationRequestLabels(int unit) throw(exception);
   virtual void writeInstrumentStateLabels(int unit) throw(exception);
   virtual void writeImageLabels(int unit) throw(exception);
   virtual void writeCompressionLabels(int unit) throw(exception);
   virtual void writeGeometricCameraModelLabels(int unit) throw(exception);
   
   //void writePixlMccImgParmsLabels(int unit) throw (exception);
   void writeIlluminationParmsLabels(int unit) throw (exception);

   virtual void readIdentificationLabels(int unit) throw(exception);
   virtual void readTelemetryLabels(int unit) throw(exception);
   virtual void readPdsHistoryLabels(int unit) throw(exception);
   virtual void readCoordinateLabels(int unit) throw(exception);
   virtual void readInstrumentStateLabels(int unit) throw(exception);
   virtual void readImageLabels(int unit) throw(exception);
   virtual void readCompressionLabels(int unit) throw(exception);

   IdphSet4 getIdph(unsigned int rtt) throw(exception);

   int planetDayNumber_;   // planet day number
   double rover_az_, rover_el_, site_az_, site_el_, solar_az_, solar_el_;

  /**************************************************************************/

      public:
   explicit M2020PixlVicarLabel(M2020PixlEdr *);
   ~ M2020PixlVicarLabel();

   virtual void setM2020DpMetaData(M2020DpMetaData * metadata) throw(exception);

   //void setProdName() throw(exception) = 0;
   //void setProdNameConfig(char) throw(exception) = 0;
   //
   void setImgWidth(int w) {
      this->imgWidth_ = w;
   }
   void setImgHeight(int h) {
      this->imgHeight_ = h;
   }
   void setImgSize(int h, int w) {
      this->imgHeight_ = h;
      this->imgWidth_ = w;
   }
   void setProductName(string prod_name) {
      this->prodName_ = prod_name;
   } 
   void setCmprsMode(int mode) {
      this->cmprsMode_ = mode;
   }
   void setCmprsQual(float qual) {
      this->cmprsQual_ = qual;
   }
   void setPixlFliDuration(float* fliDurations) {
      this->pixlFliDuration_ = new float[9];
      for (int i=0; i<9; i++) {
         this->pixlFliDuration_[i] = fliDurations[i];
         //printf("----setPixlFliDuration.....fliDurations[%d] = %f     fliDurations[%d] = %f\n",
         //      i, fliDurations[i], i, this->pixlFliDuration_[i]);
      }
   }
   void setPixlFliCurrent(int* fliCurrent) {
      this->pixlFliCurrent_ = new int[17];
      for (int i=0; i<17; i++) {
         this->pixlFliCurrent_[i] = fliCurrent[i];
         //printf("----setPixlFliCurrent.....fliCurrent[%d] = %d     pixlFliCurrent_[%d] = %d\n", 
         //      i, fliCurrent[i], i, this->pixlFliCurrent_[i]);
      }
   }
   void setPixlSliDuration(float* fliDurations) {
      this->pixlSliDuration_ = new float[2];
      for (int i=0; i<2; i++)
         this->pixlSliDuration_[i] = fliDurations[i];
   }
   void setPixlSliCurrent(int* fliCurrent) {
      this->pixlSliCurrent_ = new int[2];
      for (int i=0; i<2; i++)
         this->pixlSliCurrent_[i] = fliCurrent[i];
   }
   void setCycleType(int cycleType) {
      this->cycleType_ = cycleType;
   }
   void setFlashTemperature(float* temp) {
      this->flashTemp_ = new float[2];
      for (int i=0; i<2; i++)
         this->flashTemp_[i] = temp[i];
   }
   void setAdcOffset(unsigned int* adcOffset) {
      this->adcOffset_ = new unsigned int[2];
      for (int i=0; i<2; i++)
         this->adcOffset_[i] = adcOffset[i];
   }
   void setPreFlashVoltages(float* voltage) {
      this->preFlashVolt_ = new float[8];
      for (int i=0; i<8; i++)
         this->preFlashVolt_[i] = voltage[i];
   }
   void setPostFlashVoltages(float* voltage) {
      this->postFlashVolt_ = new float[8];
      for (int i=0; i<8; i++)
         this->postFlashVolt_[i] = voltage[i];
   }
   void setSliCfgA(bool flag) {
      this->sliCfgA_ = flag;
   }
   void setSliCfgB(bool flag) {
      this->sliCfgB_ = flag;
   }

   void setIntegrationTime(int integrationTime) {
      this->integrationTime_ = integrationTime;
   }
   void setExposureDuration(int expDuration) {
      this->exposureDuration_ = expDuration;
   }
   void setGainNumber(int gainNum) {
      this->gainNumber_ = gainNum;
   }
   void setOffsetNumber(int offsetNum) {
      this->offsetNumber_ = offsetNum;
   }
   void setFirstLine(int firstLine) {
      this->firstLine_ = firstLine;
   }
   void setFirstLineSample(int firstLineSample) {
      this->firstLineSample_ = firstLineSample;
   }
   void setLines(int lines) {
      this->lines_ = lines;
   }
   void setLineSamples(int lineSamples) {
      this->lineSamples_ = lineSamples;
   }
   void setRoi(unsigned char roi) {
      this->roi_ = roi;
   }
   void setCmprsThreshold(unsigned char threshold) {
      this->cmprsThreshold_ = threshold;
   }
   void setInfo(unsigned char info) {
      this->info_ = info;
   }
   void setValid(int valid) {
      this->valid_ = valid;
   }
   void setStatus(int status) {
      this->status_ = status;
   }
   void setCodeStart(int start) {
      this->codeStart_ = start;
   }
   void setCodeEnd(int end) {
      this->codeEnd_ = end;
   }
   void setMccSubTime(int sub) {
      this->mccSubTime_ = sub;
   }
   void setMccTime(int time) {
      this->mccTime_ = time;
   }
   void setImod(int imod) {
      this->imod_ = imod;
   }
   void setImgFH(int fh) {
      this->imgFH_ = fh;
   }
   void setImgMdAvail(bool avail) {
      this->imgMDAvail_ = avail;
   }
   void setImgMD(unsigned char* md) {
      this->imgMD_ = md;
   }

   void setSensorPos(float pos[3]) {
      for (int i=0; i<3; i++)
         this->sensorPos_[i] = pos[i];
   }

   void setSensorQuat(float quat[4]) {
      for (int i=0; i<4; i++)
         this->sensorQuat_[i] = quat[i];
   }

   void setV7Data(bool v7Data) {
      this->v7Data_ = v7Data;
   }

   void setLedA(int led) {
      this->ledA_ = led;
   }

   void setLedB(int led) {
      this->ledB_ = led;
   }

   void setConfig(string filter) { 
      this->config_ = filter; 
   };

   void setDPCat(int dpCategory) {
      this->dpCategory_ = dpCategory;
   }

   virtual const char *getInstrumentName() {
      return this->instName_.c_str();
   }
   virtual const char *getInstrumentId() {
      return this->instId_.c_str();
   }
   virtual const char *getDataSetID() {
      return this->dataSetId_.c_str();
   }
   virtual const char *getDataSetName() {
      return this->dataSetName_.c_str();
   }
   virtual const char *getProductID() {
      return this->prodId_.c_str();
   }
   virtual const char *getApplicationProcessName() {
      return this->metadata_->_productName;   /*this->appProsName_.c_str(); */
   }
   virtual const char *getProductName() {
      return this->prodName_.c_str();
   }
   
  /**
   * Call individual create methods.
   */
   void createLabels() throw(exception);

   void readLabels(int unit) throw(exception);
  /**
   * Calling this method will throw an exception
   */
   void writeLabels(string & labels) throw(exception) {
      TRACE;
      THROW_EXCEPTION(M2020Exception::FAILURE, "Method not supported");
   }

  /**
   * Call individual write methods.
   */
   void writeLabels(int unit) throw(exception);

   //int setEdr(M2020Edr*);

};

#endif            /*M2020PIXLVICARLABEL_H_ */
