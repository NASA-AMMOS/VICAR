/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020ENGCAMEDR_H_
#define M2020ENGCAMEDR_H_

#ifndef MAXDN12BIT
#define MAXDN12BIT      4096
#endif
#ifndef MAXDN8BIT
#define MAXDN8BIT       256
#endif

#include "M2020ImgEdr.h"
#include "M2020EngCamPds.h"
#include "M2020EngCamVicarLabel.h"
#include "M2020SkyCamVicarLabel.h"
#include <vector>
using namespace std;

extern "C" {
#include "jpeglib.h"
}
#include <string.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdlib.h>
#include <assert.h>
#include <setjmp.h>
#include <stdio.h>
#include "pdecom_msl.h"
#include <unistd.h>
#include <fcntl.h>
#include <netinet/in.h>
#define JPEG_SOI       0xffd8
#define MISSING_CONSTANT 0

//#define M2020_ENG_CAM_DATA_OFFSET 4

enum IMAGE_TYPE { UNKNOWN_IMAGE = 0,
   COLSUMS_IMAGE,
   HISTOGRAM_IMAGE,
   FULL_FRAME_IMAGE,
   REF_IMAGE,
   ROWSUMS_IMAGE,
   THUMB_IMAGE
};

class M2020EngCamEdr:public M2020ImgEdr {
/*
      private:

      typedef struct my_error_mgr *my_error_ptr;

      void my_error_exit(j_common_ptr cinfo)
      {
         my_error_ptr myerr = (my_error_ptr) cinfo->err;
         (*cinfo->err->output_message) (cinfo);
         longjmp(myerr->setjmp_buffer, 1);
      };
*/

      protected:

   M2020EngCamPds * pdsLabel_;
   unsigned int rawDataLength_;
   unsigned int fileSize_;
   unsigned int pixelSize_;
   //string format_;

   IMAGE_TYPE imgType_;

   bool isColor_;
   unsigned int comp_qual_;
   bool isJPEG422_;
   bool createdJPEG_;
   string jpegFilename_;

   bool isMeda_;
   unsigned int skycamNoSegs_;
   unsigned int skycamImageId_;
   unsigned int skycamTimetag_;
   unsigned int skycamImageType_;
   unsigned int skycamImageSize_;
   unsigned int skycamAcquisitionMode_;
   unsigned int skycamFastFlushCntrData_;
   unsigned int skycamExp_;
   unsigned int skycamStartRow_;
   unsigned int skycamNumberRows_;
   unsigned int skycamVideoOffset_;
   unsigned int skycamCamPcbPrt_;
   unsigned int skycamCamCcdPrt_;

   virtual bool isBlob(const string & dpoName, int vid) const;
   virtual void init() throw(exception);
   virtual void setRawDataSize();
   //virtual void convert(unsigned char *buf,int len);

   virtual void decompress() throw(exception);
   virtual int decompressJPEG(unsigned char *compData, int size) throw(exception);
   virtual void bitunpack() throw(exception);

   virtual void writeImage() throw(exception);
   virtual void writeImgDatFile() throw(exception);
   virtual void updateUPF() throw(exception);
   virtual void writeEdr(unsigned char *edrData, int length) throw(exception);
   virtual void writeRGBImage() throw(exception);
   virtual void combineCypheredData(unsigned char *outDataBuffer)
    throw(exception);

   virtual bool isHeader(const string & dpoName, int vid) const {
      TRACE;
      return (dpoName.compare(ImgIdphHdrName) == 0 || dpoName.compare(ImgIdphHdr6Name) == 0);
   }
   virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser * dpoParser) {
      TRACE;
      return new M2020IdphHeader(dpoParser);
   }

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
   struct my_error_mgr {
      struct jpeg_error_mgr pub;
      jmp_buf setjmp_buffer;
   };

   //void my_error_exit(j_common_ptr cinfo);
   
      public:
    explicit M2020EngCamEdr(M2020DpMetaData *) throw(exception);
   virtual ~ M2020EngCamEdr();

   virtual bool isMedaImg();
   virtual void ingestMedaSegmentZero(unsigned char *data);
   virtual void process() throw(exception);
   virtual void checkUPFParams() throw(exception);
   virtual void checkDataFile() throw(exception);
   virtual void processDPOs() throw(exception);
   virtual int getProductType() const {
      return this->imgType_;
   }
   virtual void finalize() throw(exception);

   virtual bool isColor() const {
      return this->isColor_;
   }
   virtual bool isJPEG422() const {
      return this->isColor_;
   }

   virtual unsigned char getWidth() const {
      return this->imgWidth_;
   }
   virtual unsigned char getHeight() const {
      return this->imgHeight_;
   }
   virtual unsigned char getSkycamNoSegs() const {
      return this->skycamNoSegs_;
   }
   virtual unsigned int getSkycamImageId() const {
      return this->skycamImageId_;
   }
   virtual unsigned int getSkycamTimetag() const {
      return this->skycamTimetag_;
   }
   virtual unsigned int getSkycamImageType() const {
      return this->skycamImageType_;
   }
   virtual unsigned int getSkycamImageSize() const {
      return this->skycamImageSize_;
   }
   virtual unsigned int getSkycamAcquisitionMode() const {
      return this->skycamAcquisitionMode_;
   }
   virtual unsigned int getSkycamFastFlushCntrData() const {
      return this->skycamFastFlushCntrData_;
   }
   virtual unsigned int getSkycamExp() const {
      return this->skycamExp_;
   }
   virtual unsigned int getSkycamStartRow() const {
      return this->skycamStartRow_;
   }
   virtual unsigned int getSkycamNumberRows() const {
      return this->skycamNumberRows_;
   }
   virtual unsigned int getSkycamVideoOffset() const {
      return this->skycamVideoOffset_;
   }
   virtual unsigned int getSkycamCamPcbPrt() const {
      return this->skycamCamPcbPrt_;
   }
   virtual unsigned int getSkycamCamCcdPrt() const {
      return this->skycamCamCcdPrt_;
   }

    /**
     * Returns the number of bytes of uncompressed/unbitpack data.
     */
   virtual unsigned int getRawDataSize() {
      return (M2020ImgEdr::getRawDataSize() - this->totalDataOffset_);
   }

   //TODO ays, add methods to return compression info for the label
   //virtual unsigned int getErrorPixels() {return (M2020ImgEdr::getErrorPixels()-this->nErrorPixels_); }

   static const std::string ImgIdphHdrName;
   static const std::string ImgIdphHdr6Name;
};

#endif            /*M2020ENGCAMEDR_H_ */
