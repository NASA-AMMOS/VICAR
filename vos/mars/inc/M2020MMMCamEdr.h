/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * This is a temporary MMMCam EDR generation software that simply takes
 * an engineering EDR labels and slaps them onto MMMCam science frame
 * data
 *
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 * @version 1.0
 */
#ifndef M2020MMMCAMEDR_H_
#define M2020MMMCAMEDR_H_

#include "M2020EngCamEdr.h"
#include "M2020MMMPds.h"
#include "M2020SrlcPds.h"

class M2020MMMCamEdr:public M2020EngCamEdr {
      private:
   string EdrName_;

      protected:

   std::map < std::string, M2020IdphHeader * >ancHdrs_;
   std::map < std::string, M2020IdphHeader * >supHdrs_;
   M2020IdphHeader *hdr_;
   M2020IdphHeader *mmmCamAncHdr_;
   M2020IdphHeader *mmmCamSupHdr_;
   M2020SrlcPds *pdsLabel_;

   unsigned char *miniHeader_;
   unsigned int product_id_;
   unsigned char thumbnail_;
   unsigned int magic0_;
   unsigned int sclk_;
   unsigned int subsclk_;
   unsigned int minisclk_;
   unsigned int minisubsclk_;
   unsigned short vflush_;
   unsigned short mode_;
   unsigned char filter_;
   float exposure_;
   unsigned char window_x_;
   unsigned char window_y_;
   unsigned char width_;
   unsigned char height_;
   unsigned int autofocus_;
   unsigned short initial_position_;
   unsigned short step_size_;
   unsigned short af_n_steps_;
   unsigned short zstack_;
   unsigned int autoexposure_;
   unsigned char target_dn_;
   unsigned char fraction_;
   unsigned char early_termination_;
   unsigned char ae_n_steps_;

   unsigned int start_image_id_;
   unsigned char exposure_count_;
   unsigned int z_stack_parms_;

   // for VIDEO
   float frametime_;  
   float framerate_;
   int gop_frame_index_;
   int gop_offset_;
   int gop_length_;
   int gop_total_frames_;

   unsigned char compression_[8];
   //unsigned char companding_;
   unsigned char camera_status_;
   unsigned int sys_serial_no_;
   unsigned char mech_[8];
   /*mech0, focus = bytes 44-47 (mech_0)
   mech2, zoom = bytes 48-49 (mech_2)
   mech1, filter = bytes 50-51 (mech_1)
   */
   int mech0_;
   short mech1_;
   unsigned short mech2_;
   unsigned int dc_offset_;
   unsigned int initsize_;
   unsigned int magic1_;
   int comp_quality_;

   bool createdJPEG_;

   bool isZstack_;
   bool isVideo_;
   bool isLossless_;
   bool isColor_;
   bool useOldFilenameConversion_;
   bool useOldVideoFilenameConversion_;

   char vicVideoFileNames_[16][64];
   char losslessVideoFileNames_[16][64];
   char jpgVideoFileNames_[16][64];
/*
   typedef enum { UNK, JPEG, PRED, RAW } image_t;
   typedef struct {
      int width;
      int height;
      int col_offset;
      int row_offset;
      int bits_per_element;
      int depth;
      int bands;
      int thumbnail;
      image_t image_type;
   } header_t;

   void my_error_exit(j_common_ptr cinfo);
*/

   virtual bool isHeader(const string & dpoName, int vid) const {
      TRACE;
      //  NUL as it's dpo parser.
      if (dpoName.find("DpoCidph") != std::string::npos || dpoName.find("ImfIdphSubset6") != std::string::npos)
         return true;
      else
         return false;
   } 
   virtual bool isAncHeader(const string & dpoName, int vid)const {
      return (dpoName.find("AncillaryData") != std::string::npos);
   } 
   virtual bool isSupHeader(const string & dpoName, int vid)const {
      bool issup = dpoName.find("Supplementary");
      //cout << "isSupHeaer returning: " << issup << endl;
       return (dpoName.find("Supplementary") != std::string::npos);
   } 
   virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser * dpoParser);

   virtual bool isBlob(const string & dpoName, int vid) const {
      return false;
   } 
   virtual void updateUPF() throw(exception);
   //virtual void writeEdr(unsigned char *edrData, int length) throw(exception);
   virtual void writeEdr() throw(exception);

      public:

    explicit M2020MMMCamEdr(M2020DpMetaData * m) throw(exception);
    virtual ~ M2020MMMCamEdr();
   virtual void checkUPFParams() throw(exception);
   virtual void checkDataFile() throw(exception);
   virtual void processDPOs() throw(exception);
   virtual void process() throw(exception);
   virtual void init() throw(exception);
   virtual void setRawDataSize();
   virtual void writeImage() throw(exception);
   virtual int decompressLossless(unsigned char *, int, int) throw(exception);
   virtual int decode_dat_header(int, header_t *);
   virtual int decompressJPEG(unsigned char *, int, int) throw(exception);
   virtual void bitunpack_10(unsigned char *) throw(exception);
   virtual void finalize() throw(exception);
   virtual unsigned int getProductId() const {
      return this->product_id_;
   } 
   virtual unsigned char getThumbnail() const {
      return this->thumbnail_;
   } 
   virtual unsigned int getMagic0() const {
      return this->magic0_;
   } 
   virtual unsigned int getSclk() const {
      return this->sclk_;
   } 
   virtual unsigned int getSubSclk() const {
      return this->subsclk_;
   } 
   virtual unsigned int getMiniSclk() const {
      return this->minisclk_;
   } 
   virtual unsigned short getVFlush() const {
      return this->vflush_;
   } 
   virtual unsigned short getMode() const {
      return this->mode_;
   } 
   virtual unsigned char getFilter() const {
      return this->filter_;
   } 
   virtual float getExposure() const {
      return this->exposure_;
   } 
   virtual unsigned char getWindowX() const {
      return this->window_x_;
   } 
   virtual unsigned char getWindowY() const {
      return this->window_y_;
   } 
   virtual unsigned char getWidth() const {
      return this->width_;
   } 
   virtual unsigned char getHeight() const {
      return this->height_;
   } 
   virtual unsigned int getAutofocus() const {
      return this->autofocus_;
   } 
   virtual unsigned short getInitialPosition() const {
      return this->initial_position_;
   } 
   virtual unsigned short getStepSize() const {
      return this->step_size_;
   } 
   virtual unsigned short getAFNSteps() const {
      return this->af_n_steps_;
   } 
   virtual unsigned short getZStack() const {
      return this->zstack_;
   } 
   virtual unsigned int getAutoexposure() const {
      return this->autoexposure_;
   } 
   virtual unsigned char getTargetDN() const {
      return this->target_dn_;
   } 
   virtual unsigned char getFraction() const {
      return this->fraction_;
   } 
   virtual unsigned char getEarlyTermination() const {
      return this->early_termination_;
   } 
   virtual unsigned char getAENSteps() const {
      return this->ae_n_steps_;
   } 
   virtual unsigned int getStartImageId() const {
      return this->start_image_id_;
   } 
   virtual unsigned char getExposureCount() const {
      return this->exposure_count_;
   } 
   virtual unsigned int getZStackParms() const {
      return this->z_stack_parms_;
   } 
   virtual unsigned char *getCompression() {
      return this->compression_;
   } 
   virtual int getCompQuality() const {
      return this->comp_quality_;
   } 
   virtual unsigned char getCameraStatus() const {
      return this->camera_status_;
   } 
   virtual unsigned int getSysSerialNo() const {
      return this->sys_serial_no_;
   } 
   virtual const unsigned char *getMech() const {
      return this->mech_;
   } 
   virtual int getMech0() const {
      return this->mech0_;
   } 
   virtual short getMech1() const {
      return this->mech1_;
   } 
   virtual unsigned short getMech2() const {
      return this->mech2_;
   } 
   virtual unsigned int getSclkSubSec() const {
      return this->minisubsclk_;
   } 
   virtual unsigned int getDCOffset() const {
      return this->dc_offset_;
   } 
   virtual unsigned int getInitSize() const {
      return this->initsize_;
   } 
   virtual unsigned int getMagic1() const {
      return this->magic1_;
   } 

   // for VIDEO 
   virtual float getFrameRate() const {
      return this->framerate_;
   }
   virtual float getFrameTime() const {
      return this->frametime_;
   }
   virtual unsigned int getGOPFrameIndex() const {
      return this->gop_frame_index_;
   }
   virtual unsigned int getGOPTotalFrames() const {
      return this->gop_total_frames_;
   }
   virtual unsigned int getGOPOffset() const {
      return this->gop_offset_;
   }
   virtual unsigned int getGOPLength() const {
      return this->gop_length_;
   }

   virtual unsigned isZstack() const {
      return this->isZstack_;
   } 
   virtual bool isLossless() const {
      return this->isLossless_;
   } 
   virtual bool isColor() const {
      return this->isColor_;
   } 
   virtual unsigned isVideo() const {
      return this->isVideo_;
   } 
   virtual bool useOldFilenameConversion() const {
      return this->useOldFilenameConversion_;
   }
   virtual void setUseOldFilenameConversion(bool useOld) {
      this->useOldFilenameConversion_ = useOld;
   }
   virtual bool useOldVideoFilenameConversion() const {
      return this->useOldVideoFilenameConversion_;
   }
   virtual void setUseOldVideoFilenameConversion(bool useOld) {
      this->useOldVideoFilenameConversion_ = useOld;
   }

   static const std::string MMMIdphHdrName;
   static const std::string MMMImfIdphHdrName;
   static const std::string MMMAncillaryHdrName;
   static const std::string MczAncillaryHdrName;
   static const std::string McamSupplementaryHdrName;
   static const std::string MrdiSupplementaryHdrName;
   static const std::string WatsonSupplementaryHdrName;
   static const std::string SrlcImgSupplementaryHdrName;
   static const std::string MMMZstackAncillaryHdrName;
   static const std::string MMMVideoAncillaryHdrName;
   static const std::string MMMRecoveredProductAncillaryHdrName;
   static const std::string MMMRecoveredThumbnailAncillaryHdrName;
   static const std::string MMMScidataDPOName;
   static const std::string MMMTransmitProductScidataDPOName;
   static const std::string MMMTransmitThumbnailScidataDPOName;
};

#endif            //M2020MMMCAMEDR_H_
