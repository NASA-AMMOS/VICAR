/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * This is a temporary SuperCam EDR generation software that simply takes
 * an engineering EDR labels and slaps them onto SuperCam science frame
 * data
 *
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 * @version 1.0
 */
#ifndef M2020SUPERCAMEDR_H_
#define M2020SUPERCAMEDR_H_

#include "M2020EngCamEdr.h"
#include "M2020SuperCamPds.h"

class M2020SuperCamEdr:public M2020EngCamEdr {
      private:
   string EdrName_;

      protected:

   M2020SuperCamPds * pdsLabel_;
   M2020IdphHeader *hdr_;
    std::map < std::string, M2020IdphHeader * >ancHdrs_;
   unsigned int exposure_;
   unsigned int fswver_;
   unsigned int fswverwpnt_;

   //bool foundRMI_; //need for cruise data
   unsigned int rmiDataLen_;

   //MSL DPOs and fields
   unsigned short motor_pos_;
   unsigned int n_following_soh_before_[512];
   unsigned int n_following_soh_after_[512];
   unsigned short cmd_reply_err_cntrl_[512];
   unsigned int spect_laser_dbbytecount_;
   unsigned short spect_total_shots_;
   unsigned short spect_avg_shots_;
   unsigned short spect_ignore_shots_;
   double motor_distance_;
   unsigned int spect_type_;
   unsigned int use_laser_;
   unsigned int cmp_segs_;
   unsigned int stats_;
   //unsigned char* imgDataHeaderAndFooter_;

   //Generic header fields
   unsigned short packet_header_marker_;
   unsigned int rce_time_sync_;
   unsigned short data_definition_version_;
   unsigned int nv_xmit_buffer_count_;
   unsigned int bu_software_version_;
   unsigned int cndh_firmware_version_;
   unsigned int bu_hardware_identifier_;
   unsigned int se_firmware_version_;
   unsigned int se_hardware_identifier_;
   unsigned int mu_version_identifier_;
   unsigned int mu_hardware_identifier_;
   unsigned int xmit_data_id_;
   unsigned int reserved_;
   unsigned int number_of_sections_;
   unsigned int number_of_markers_;
   unsigned int generic_data_buffer_size_;
   unsigned short *found_markers_;
   unsigned int *found_markers_offset_;
   unsigned int *found_markers_millisec_count_;
   unsigned int *found_markers_data_length_;
   //unsigned short all_opcodes_[4096];
   //unsigned int all_opcode_names_[4096];
   
   unsigned int RMI_Hdr_Size_;
   unsigned short Exposure_Time_;
   unsigned char HDR_mode_;
   unsigned short ROI_Size_;
   unsigned short ROI_Y_Position_;
   unsigned char MU_Memory_Bank_;
   unsigned short Lower_Threshold_;
   unsigned short Upper_Threshold_;
   unsigned int Max_number_under_lower_;
   unsigned int Max_number_above_upper_;
   unsigned short ROI_size_exposure_;
   unsigned short ROI_Y_position_exposure_;
   unsigned short Clipping_threshold_;
   unsigned char Frame_number_;
   unsigned short Exposure_step0_;
   unsigned short Exposure_step1_;
   unsigned short Exposure_step2_;
   unsigned short Exposure_step3_;
   unsigned short Exposure_step4_;
   unsigned short Exposure_step5_;
   char RMI_CMOS_Registers_[257];
   char MU_FPGA_Registers_[17];
   unsigned short Marker_Pad_;
   unsigned int RCE_Time_Sync_;
   unsigned int Millisecond_Count_;
   unsigned int NV_Image_Counter_ID_;
   unsigned short Reserved1_;
   unsigned short Reserved2_;
   unsigned int Data_Length_;
   //unsigned short *Image_Data_;

   unsigned char *imgSOH_;
   unsigned int imgSOHLength_;
   string imgSOHName_; 

   int _unionCmprsSegStatus[ICER_MAX_N_SEGS*4];
   int _unionSegFirstLine[ICER_MAX_N_SEGS*4];
   int _unionSegFirstSample[ICER_MAX_N_SEGS*4];
   int _unionSegLines[ICER_MAX_N_SEGS*4];
   int _unionSegSamples[ICER_MAX_N_SEGS*4];
   double _unionSegQuality[ICER_MAX_N_SEGS*4];
   int _unionSegMissingPixels[ICER_MAX_N_SEGS*4];

   bool foundBayerImage_;

   virtual bool isHeader(const string & dpoName, int vid) const {
      TRACE;
      //  NUL as it's dpo parser.
      //if (M2020EngCamEdr::isHeader(dpoName,vid)) return true;
      //else if (this->isSuperHeader(dpoName,vid)) return true;
      if (dpoName.find("Idph") != std::string::npos)
         return true;
      return false;
   }
   virtual bool isSuperHeader(const string & dpoName, int vid)const {
      //return (dpoName.compare("CcamRmiImageAncillaryData")==0);
      //cout << "isSuperHeader: " << (((dpoName.find("Ccam")!=std::string::npos &&
      //dpoName.find("Ancillary")!=std::string::npos)) || dpoName.find("Idph")!=std::string::npos) << endl;
      return ((dpoName.find("Scam") != std::string::npos && dpoName.find("Ancillary") != std::string::npos) || dpoName.find("Focal") != std::string::npos);
   }
   virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser * dpoParser);

   virtual bool isBlob(const string & dpoName, int vid) const {
      return false;
   }
      public:
    M2020IdphHeader * ancHdr_;
   explicit M2020SuperCamEdr(M2020DpMetaData * m) throw(exception);
    virtual ~ M2020SuperCamEdr();
   virtual void checkUPFParams() throw(exception);
   virtual void checkDataFile() throw(exception);
   virtual void processDPOs() throw(exception);
   virtual void process() throw(exception);
   virtual void init() throw(exception);
   virtual void setRawDataSize();
   virtual void writeProduct() throw(exception);
   virtual void read_mini_header() throw(exception);
   virtual void handle_soh_dpo() throw(exception);
   virtual void rotate_and_flip(unsigned char* data) throw(exception);
   virtual void finalize() throw(exception);
   virtual void updateUPF(string pname) throw(exception);
   virtual void writeEdr(unsigned char *edrData, int length) throw(exception);
   virtual void writeEdr() throw(exception);
   virtual unsigned char* swap(unsigned char* a) throw(exception);
   string getProductType(string &fitsTag);
   virtual unsigned int getExposure() const {
      return this->exposure_;
   }
   virtual unsigned int getFSWVer() const {
      return this->fswver_;
   }
   virtual unsigned int getFSWVerWPnt() const {
      return this->fswverwpnt_;
   }
   virtual unsigned short getMotorPos() const {
      return this->motor_pos_;
   }
   virtual double getMotorDistance() const {
      return this->motor_distance_;
   }
   virtual const unsigned int *getNFollowingSOHBefore() const {
      return this->n_following_soh_before_;
   }
   virtual const unsigned int *getNFollowingSOHAfter() const {
      return this->n_following_soh_after_;
   }
   virtual const unsigned short *getCmdReplyErrCntrl() const {
      return this->cmd_reply_err_cntrl_;
   }
   virtual unsigned int getLaserDBBytecount() const {
      return this->spect_laser_dbbytecount_;
   }
   virtual unsigned short getSpectTotalShots() const {
      return this->spect_total_shots_;
   }
   virtual unsigned short getSpectAvgShots() const {
      return this->spect_avg_shots_;
   }
   virtual unsigned short getSpectIgnoreShots() const {
      return this->spect_ignore_shots_;
   }
   virtual unsigned int getUseLaser() const {
      return this->use_laser_;
   }
   virtual unsigned int getStats() const {
      return this->stats_;
   }
   virtual unsigned int getSpectType() const {
      return this->spect_type_;
   }
   virtual unsigned int getNoCmpSegs() const {
      return this->cmp_segs_;
   }
   virtual M2020DpMetaData *getMetaData() const {
      return this->metadata_;
   }

   //For cruise data return the generic header parameters
   virtual unsigned short getPacketHeaderMarker() const {
      return this->packet_header_marker_;
   }
   virtual unsigned int getRceTimeSync() const {
      return this->rce_time_sync_;
   }
   virtual unsigned short getDataDefinitionVersion() const {
      return this->data_definition_version_;
   }
   virtual unsigned int getNVXmitBufferCount() const {
      return this->nv_xmit_buffer_count_;
   }
   virtual unsigned int getBUSoftwareVersion() const {
      return this->bu_software_version_;
   }
   virtual unsigned int getCNDHFirmwareVersion() const {
      return this->cndh_firmware_version_;
   }
   virtual unsigned int getBUHardwareIdentifier() const {
      return this->bu_hardware_identifier_;
   }
   virtual unsigned int getSEFirmwareVersion() const {
      return this->se_firmware_version_;
   }
   virtual unsigned int getSEHardwareIdentifier() const {
      return this->se_hardware_identifier_;
   }
   virtual unsigned int getMUVersionIdentifier() const {
      return this->mu_version_identifier_;
   }
   virtual unsigned int getMUHardwareIdentifier() const {
      return this->mu_hardware_identifier_;
   }
   virtual unsigned int getXMITDataId() const {
      return this->xmit_data_id_;
   }
   virtual unsigned int getReserved() const {
      return this->reserved_;
   }
   virtual unsigned int getNumberOfSections() const {
      return this->number_of_sections_;
   }
   virtual unsigned int getNumberOfMarkers() const {
      return this->number_of_markers_;
   }
   virtual unsigned int getGenericDataBufferSize() const {
      return this->generic_data_buffer_size_;
   }

   virtual unsigned short *getFoundMarkers() const {
      return this->found_markers_;
   }
   virtual unsigned int *getFoundMarkersOffset() const {
      return this->found_markers_offset_;
   }
   virtual unsigned int *getFoundMarkersMilli() const {
      return this->found_markers_millisec_count_;
   }
   virtual unsigned int *getFoundMarkersLen() const {
      return this->found_markers_data_length_;
   }
   virtual bool getFoundRMI() const {
      return this->foundRMI_;
   }
   virtual bool getFoundRMISOH() const {
      return this->foundRMISOH_;
   }

   virtual unsigned int getRmiHdrSize() const {
      return this->RMI_Hdr_Size_;
   }
   virtual unsigned short getExposureTime() const {
      return this->Exposure_Time_;
   }
   virtual unsigned char getHDRMode() const {
      return this->HDR_mode_;
   }
   virtual unsigned short getROISize() const {
      return this->ROI_Size_;
   }
   virtual unsigned short getROIYPosition() const {
      return this->ROI_Y_Position_;
   }
   virtual unsigned char getMUMemoryBank() const {
      return this->MU_Memory_Bank_;
   }
   virtual unsigned short getLowerThreshold() const {
      return this->Lower_Threshold_;
   }
   virtual unsigned short getUpperThreshold() const {
      return this->Upper_Threshold_;
   }
   virtual unsigned int getMaxNumberUnderLower() const {
      return this->Max_number_under_lower_;
   }
   virtual unsigned int getMaxNumberAboveUpper() const {
      return this->Max_number_above_upper_;
   }
   virtual unsigned short getROISizeExposure() const {
      return this->ROI_size_exposure_;
   }
   virtual unsigned short getROIYPositionExposure() const {
      return this->ROI_Y_position_exposure_;
   }
   virtual unsigned short getClippingThreshold() const {
      return this->Clipping_threshold_;
   }
   virtual unsigned char getFrameNumber() const {
      return this->Frame_number_;
   }
   virtual unsigned short getExposureStep0() const {
      return this->Exposure_step0_;
   }
   virtual unsigned short getExposureStep1() const {
      return this->Exposure_step1_;
   }
   virtual unsigned short getExposureStep2() const {
      return this->Exposure_step2_;
   }
   virtual unsigned short getExposureStep3() const {
      return this->Exposure_step3_;
   }
   virtual unsigned short getExposureStep4() const {
      return this->Exposure_step4_;
   }
   virtual unsigned short getExposureStep5() const {
      return this->Exposure_step5_;
   }
   virtual char* getRMICMOSRegisters() {
      return this->RMI_CMOS_Registers_;
   }
   virtual char* getMUFPGARegisters() {
      return this->MU_FPGA_Registers_;
   }
   virtual unsigned short getMarkerPad() const {
      return this->Marker_Pad_;
   }
   virtual unsigned int getRCETimeSync() const {
      return this->RCE_Time_Sync_;
   }
   virtual unsigned int getMillisecondCount() const {
      return this->Millisecond_Count_;
   }
   virtual unsigned int getNVImageCounterID() const {
      return this->NV_Image_Counter_ID_;
   }
   virtual unsigned short getReserved1() const {
      return this->Reserved1_;
   }
   virtual unsigned short getReserved2() const {
      return this->Reserved2_;
   }
   virtual unsigned int getDataLength() const {
      return this->Data_Length_;
   }
   virtual unsigned int getFoundBayerImage() const {
      return this->foundBayerImage_;
   }
   virtual const int *getUnionCmprsSegStatus() const {
      return this->_unionCmprsSegStatus;
   }
   virtual const int *getUnionSegFirstLine() const{
      return this->_unionSegFirstLine;
   }
   virtual const int *getUnionSegFirstSample() const{
      return this->_unionSegFirstSample;
   }
   virtual const int *getUnionSegLines() const{
      return this->_unionSegLines;
   }
   virtual const int *getUnionSegSamples() const{
      return this->_unionSegSamples;
   }
   virtual const double *getUnionSegQuality() const{
      return this->_unionSegQuality;
   }
   virtual const int *getUnionSegMissingPixels() const{
      return this->_unionSegMissingPixels;
   }

/*
   static const std::string RmiImgHdrName;
   static const std::string LibsSpectraIdphHdrName;
   static const std::string RmiImgAncHdrName;
   static const std::string RmiImgIcerHdrName;
   static const std::string RmiImgLocoHdrName;
   static const std::string RmiImgThumbHdrName;
   static const std::string RmiImgThumbIcerHdrName;
   static const std::string RmiImgThumbLocoHdrName;
   static const std::string RmiRefPixHdrName;
   static const std::string LibsSpectraHdrName;
   static const std::string SohHdrName;
   static const std::string SohDefaultHdrName;
   static const std::string SohInitHdrName;
   static const std::string SohPwrOnHdrName;
   static const std::string SohPwrOffHdrName;
   static const std::string SohWarmUpHdrName;
   static const std::string SohSunSafeHdrName;
*/


   static const std::string ImfIdph6HdrName;
   static const std::string ImfIdph4HdrName;
   static const std::string AncTempHdrName;
   static const std::string AncArgsHdrName;
   static const std::string AncParamsHdrName;
   static const std::string AncPointHdrName;
   static const std::string FocalDistHdrName;
   static const std::string AncSendDataArgsHdrName;

};

#endif            //M2020SUPERCAMEDR_H_
