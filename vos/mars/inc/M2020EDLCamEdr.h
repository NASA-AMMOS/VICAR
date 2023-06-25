/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * This is a temporary EDLCam EDR generation software that simply takes
 * an engineering EDR labels and slaps them onto EDLCam science frame
 * data
 *
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 * @version 1.0
 */
#ifndef M2020EDLCAMEDR_H_
#define M2020EDLCAMEDR_H_

#include "M2020EngCamEdr.h"
#include "M2020EDLPds.h"

class M2020EDLCamEdr:public M2020EngCamEdr {
      private:
   string EdrName_;
   string edl_cam_type_;
   string edl_fname_;
   string edl_movie_prod_id_;
   bool isVideo_;

      protected:

   M2020EDLPds * pdsLabel_;

   bool foundBayerImage_;
   unsigned int img_number_;
   unsigned int frame_idx_;
   unsigned int total_frames_;
   float frame_rate_;
   unsigned int sclk_;
   unsigned int subsclk_;
   unsigned int file_counter_;

   virtual bool isBlob(const string & dpoName, int vid) const {
      return false;
   }
      public:
   explicit M2020EDLCamEdr(M2020DpMetaData * m) throw(exception);
    virtual ~ M2020EDLCamEdr();
   virtual void checkUPFParams() throw(exception);
   virtual void checkDataFile() throw(exception);
   virtual void processDPOs() throw(exception);
   virtual void process() throw(exception);
   virtual void init() throw(exception);
   virtual void setRawDataSize();
   virtual void finalize() throw(exception);
   virtual string getEDLCamType() const {
      return this->edl_cam_type_;
   }
   virtual string getEDLFname() const {
      return this->edl_fname_;
   }
   virtual string getEDLMovieProdId() const {
      return this->edl_movie_prod_id_;
   }
   virtual bool isVideo() const {
      return this->isVideo_;
   }
   virtual unsigned int getImageNumber() const {
      return this->img_number_;
   }
   virtual unsigned int getSclk() const {
      return this->sclk_;
   }
   virtual unsigned int getSubSclk() const {
      return this->subsclk_;
   }
   virtual float getFrameRate() const {
      return this->frame_rate_;
   }
   virtual unsigned int getFrameIndex() const {
      return this->frame_idx_;
   }
   virtual unsigned int getTotalFrames() const {
      return this->total_frames_;
   }
   virtual unsigned int getFileCounter() const {
      return this->file_counter_;
   }

};

#endif            //M2020EDLCAMEDR_H_
