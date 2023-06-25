/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020IMGEDR_H_
#define M2020IMGEDR_H_

#include "M2020Edr.h"
#include "M2020IdphHeader.h"
#include "M2020DPOParser.h"
#include "M2020DPOMetaData.h"

#include <vector>
using namespace std;

extern "C" {
#include "m2020_loco_pub.h"
#include "m2020_icer_pub.h"
} typedef std::map < string, M2020IdphHeader * >IDPHS;

class M2020ImgEdr:public M2020Edr {
      public:

   virtual ~ M2020ImgEdr();

   virtual int getProductType() const = 0;
   virtual int getCompSegNums() const {
      return this->datasegs_.size();
   }
   virtual bool isCompressed() const {
      return this->isComp_;
   }
   virtual bool isLocoComp() const {
      return this->locoComp_;
   }
   virtual bool isIcerComp() const {
      return this->icerComp_;
   }
   virtual bool isJpegComp() const {
      return this->jpegComp_;
   }
   virtual bool isLossy() const {
      return this->isLossy_;
   }
   virtual unsigned int getErrorPixels() const {
      return this->nErrorPixels_;
   }
   virtual string getFormat() const {
      return this->format_;
   }
   virtual float getBytIn() const {
      return this->bytIn_;
   }
   virtual unsigned int getPixOut() const {
      return this->pixOut_;
   }
   virtual unsigned int getNDecomps() const {
      return this->nDecomps_;
   }
   virtual const int *getCmprsSegStatus() const {
      return this->_cmprsSegStatus;
   }
   virtual const int *getSegFirstLine() const {
      return this->_segFirstLine;
   }
   virtual const int *getSegFirstSample() const {
      return this->_segFirstSample;
   }
   virtual const int *getSegLines() const {
      return this->_segLines;
   }
   virtual const int *getSegSamples() const {
      return this->_segSamples;
   }
   virtual const double *getSegQuality() const {
      return this->_segQuality;
   }
   virtual const int *getSegMissingPixels() const {
      return this->_segMissingPixels;
   }
   virtual const int getIcerFilterOut() const {
      return this->_icerFilterOut;
   }
   M2020IdphHeader *getIdph(const string dpoName);
   M2020IdphHeader *getNavMapIdph(const string dpoName, int startIdx);
   unsigned char *imgDataHeaderAndFooter_;
   bool foundRMI_;
   bool foundRMISOH_;

/*
   virtual unsigned int getIDPHDPOPdsObjectPtr() const {
      return this->_idphDPOPdsObjectPtr;
   }
   virtual unsigned int getAncDPOPdsObjectPtr() const {
      return this->_ancDPOPdsObjectPtr;
   }
   virtual unsigned int getSOHBeforeDPOPdsObjectPtr() const {
      return this->_sohBeforeDPOPdsObjectPtr;
   }
   virtual unsigned int getSOHAfterDPOPdsObjectPtr() const {
      return this->_sohAfterDPOPdsObjectPtr;
   }
   virtual unsigned int getAutoFocusDPOPdsObjectPtr() const {
      return this->_autoFocusDPOPdsObjectPtr;
   }
   virtual unsigned int getMUHeaderDPOPdsObjectPtr() const {
      return this->_muHeaderDPOPdsObjectPtr;
   }
   virtual unsigned int getMUFooterDPOPdsObjectPtr() const {
      return this->_muFooterDPOPdsObjectPtr;
   }
   virtual unsigned int getImageReplyDPOPdsObjectPtr() const {
      return this->_imageReplyDPOPdsObjectPtr;
   }
   virtual unsigned int getImageHeaderFooterPdsObjectPtr() const {
      return this->_imageHeaderFooterPdsObjectPtr;
   }
   virtual unsigned int getIDPHDPOBytes() const {
      return this->_idphDPOBytes;
   }
   virtual unsigned int getAncDPOBytes() const {
      return this->_ancDPOBytes;
   }
   virtual unsigned int getSOHBeforeDPOBytes() const {
      return this->_sohBeforeDPOBytes;
   }
   virtual unsigned int getSOHAfterDPOBytes() const {
      return this->_sohAfterDPOBytes;
   }
   virtual unsigned int getAutoFocusDPOBytes() const {
      return this->_autoFocusDPOBytes;
   }
   virtual unsigned int getMUHeaderDPOBytes() const {
      return this->_muHeaderDPOBytes;
   }
   virtual unsigned int getMUFooterDPOBytes() const {
      return this->_muFooterDPOBytes;
   }
   virtual unsigned int getImageReplyDPOBytes() const {
      return this->_imageReplyDPOBytes;
   }
   virtual unsigned int getImageHeaderFooterBytes() const {
      return this->_imageHeaderFooterBytes;
   }
   virtual unsigned int getIDPHDPORows() const {
      return this->_idphDPORows;
   }
   virtual unsigned int getAncDPORows() const {
      return this->_ancDPORows;
   }
   virtual unsigned int getSOHBeforeDPORows() const {
      return this->_sohBeforeDPORows;
   }
   virtual unsigned int getSOHAfterDPORows() const {
      return this->_sohAfterDPORows;
   }
   virtual unsigned int getAutoFocusDPORows() const {
      return this->_autoFocusDPORows;
   }
   virtual unsigned int getMUHeaderDPORows() const {
      return this->_muHeaderDPORows;
   }
   virtual unsigned int getMUFooterDPORows() const {
      return this->_muFooterDPORows;
   }
   virtual unsigned int getImageReplyDPORows() const {
      return this->_imageReplyDPORows;
   }
   virtual unsigned int getImageHeaderFooterRows() const {
      return this->_imageHeaderFooterRows;
   }
   virtual unsigned int getIDPHDPORowBytes() const {
      return this->_idphDPORowBytes;
   }
   virtual unsigned int getAncDPORowBytes() const {
      return this->_ancDPORowBytes;
   }
   virtual unsigned int getSOHBeforeDPORowBytes() const {
      return this->_sohBeforeDPORowBytes;
   }
   virtual unsigned int getSOHAfterDPORowBytes() const {
      return this->_sohAfterDPORowBytes;
   }
   virtual unsigned int getAutoFocusDPORowBytes() const {
      return this->_autoFocusDPORowBytes;
   }
   virtual unsigned int getMUHeaderDPORowBytes() const {
      return this->_muHeaderDPORowBytes;
   }
   virtual unsigned int getMUFooterDPORowBytes() const {
      return this->_muFooterDPORowBytes;
   }
   virtual unsigned int getImageReplyDPORowBytes() const {
      return this->_imageReplyDPORowBytes;
   }
   virtual unsigned int getImageHeaderFooterRowBytes() const {
      return this->_imageHeaderFooterRowBytes;
   }
   virtual unsigned int getCmdReplyFrameDPOPdsObjectPtr(int i) const {
      return this->_cmdReplyFrameDPOPdsObjectPtr[i];
   }
   virtual unsigned int getCmdReplyFrameDPOBytes(int i) const {
      return this->_cmdReplyFrameDPOBytes[i];
   }
   virtual unsigned int getCmdReplyFrameDPORows(int i) const {
      return this->_cmdReplyFrameDPORows[i];
   }
   virtual unsigned int getCmdReplyFrameDPORowBytes(int i) const {
      return this->_cmdReplyFrameDPORowBytes[i];
   }
   virtual unsigned int getTakeImageTimeDPOPdsObjectPtr() const {
      return this->_takeImageTimeDPOPdsObjectPtr;
   }
   virtual unsigned int getTakeImageTimeDPOBytes() const {
      return this->_takeImageTimeDPOBytes;
   }
   virtual unsigned int getTakeImageTimeDPORows() const {
      return this->_takeImageTimeDPORows;
   }
   virtual unsigned int getTakeImageTimeDPORowBytes() const {
      return this->_takeImageTimeDPORowBytes;
   }
*/
   vector < DataDPO * >getDatasegs() const {
      return this->datasegs_;
   }
   M2020DPOMetaData *getDPOMetaData() const {
      return this->dpoMetaData_;
   }
      protected:

   unsigned int totalDataOffset_;
   unsigned int nErrorPixels_;
   unsigned int pixOut_;
   float bytIn_;
   unsigned int nDecomps_;
   bool isLossy_;
   int _icerFilterOut;
   string format_;

   float icer_bpp_;
   unsigned int loco_pixel_size_;

/*
   unsigned int _idphDPOPdsObjectPtr;
   unsigned int _ancDPOPdsObjectPtr;
   unsigned int _sohBeforeDPOPdsObjectPtr;
   unsigned int _sohAfterDPOPdsObjectPtr;
   unsigned int _autoFocusDPOPdsObjectPtr;
   unsigned int _muHeaderDPOPdsObjectPtr;
   unsigned int _muFooterDPOPdsObjectPtr;
   unsigned int _imageReplyDPOPdsObjectPtr;
   unsigned int _imageHeaderFooterPdsObjectPtr;

   unsigned int _idphDPOBytes;
   unsigned int _ancDPOBytes;
   unsigned int _sohBeforeDPOBytes;
   unsigned int _sohAfterDPOBytes;
   unsigned int _autoFocusDPOBytes;
   unsigned int _muHeaderDPOBytes;
   unsigned int _muFooterDPOBytes;
   unsigned int _imageReplyDPOBytes;
   unsigned int _imageHeaderFooterBytes;

   unsigned int _idphDPORows;
   unsigned int _ancDPORows;
   unsigned int _sohBeforeDPORows;
   unsigned int _sohAfterDPORows;
   unsigned int _autoFocusDPORows;
   unsigned int _muHeaderDPORows;
   unsigned int _muFooterDPORows;
   unsigned int _imageReplyDPORows;
   unsigned int _imageHeaderFooterRows;

   unsigned int _idphDPORowBytes;
   unsigned int _ancDPORowBytes;
   unsigned int _sohBeforeDPORowBytes;
   unsigned int _sohAfterDPORowBytes;
   unsigned int _autoFocusDPORowBytes;
   unsigned int _muHeaderDPORowBytes;
   unsigned int _muFooterDPORowBytes;
   unsigned int _imageReplyDPORowBytes;
   unsigned int _imageHeaderFooterRowBytes;

   unsigned int _cmdReplyFrameDPOPdsObjectPtr[50];
   unsigned int _cmdReplyFrameDPOBytes[50];
   unsigned int _cmdReplyFrameDPORows[50];
   unsigned int _cmdReplyFrameDPORowBytes[50];
   unsigned int _takeImageTimeDPOPdsObjectPtr;
   unsigned int _takeImageTimeDPOBytes;
   unsigned int _takeImageTimeDPORows;
   unsigned int _takeImageTimeDPORowBytes;
*/

   int _cmprsSegStatus[ICER_MAX_N_SEGS];
   int _segFirstLine[ICER_MAX_N_SEGS];
   int _segFirstSample[ICER_MAX_N_SEGS];
   int _segLines[ICER_MAX_N_SEGS];
   int _segSamples[ICER_MAX_N_SEGS];
   double _segQuality[ICER_MAX_N_SEGS];
   int _segMissingPixels[ICER_MAX_N_SEGS];

   vector < DataDPO * >datasegs_;
   bool isComp_;
   bool locoComp_;
   bool icerComp_;
   bool jpegComp_;
   M2020DPOParser *dpoParser_;
   M2020DPOParser *dpoAncParser_;
   M2020DPOParser *dpoAncTempParser_;
   M2020DPOParser *dpoAncArgsParser_;
   M2020DPOParser *dpoAncParamsParser_;
   M2020DPOParser *dpoAncPointParser_;
   M2020DPOParser *dpoAncSendDataParser_;
   M2020DPOParser *dpoFocalParser_;
   M2020DPOParser *dpoSupParser_;
   M2020DPOMetaData *dpoMetaData_;
   explicit M2020ImgEdr(M2020DpMetaData * d) throw(exception);

   virtual bool isHeader(const string & dpoName, int vid) const = 0;
   virtual bool isBlob(const string & dpoName, int vid) const = 0;
   virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser *) = 0;
   virtual void finalize() throw(exception) = 0;
   virtual void process() throw(exception) = 0;
   virtual void processDPOs() throw(exception);
   virtual void processNavMapHeaderDPOs(int startIdx) throw(exception);
   virtual void processNavMapDPOs() throw(exception);

      private:

   IDPHS idphs_;
};

#endif            //M2020IMGEDR_H_
