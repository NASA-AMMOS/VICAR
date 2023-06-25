/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020ENGCAMVICARLABEL_H_
#define M2020ENGCAMVICARLABEL_H_

#include "M2020VicarLabel.h"
#include "rts_time.h"
#include <sys/time.h>

typedef struct {
   unsigned int sclk;
   unsigned int subsclk;
   char instr[3];
   char side;
   char config;
   char venue;
   int sol;
   char solstr[5];
   char prod[4];
   char geom;
   char sample;
   char down;
   int site;
   char thumb;
   char compr[3];
   int drive;
   char filt;
   char seqid[10];
   char who;
   char ver[32];
   char ext[4];
} outputFileNameStruct;

class M2020EngCamVicarLabel:public M2020VicarLabel {
      public:
   explicit M2020EngCamVicarLabel(M2020ImgEdr *);
   virtual ~ M2020EngCamVicarLabel();
   virtual bool isNavCam() {
      return this->isNavCam_;
   }
   virtual bool isHazCam() {
      return this->isHazCam_;
   }
   virtual bool isCacheCam() {
      return this->isCacheCam_;
   }
   virtual bool isLCam() {
      return this->isLCam_;
   }
   virtual bool isPUCam() {
      return this->isPUCam_;
   }
   virtual bool isDDCam() {
      return this->isDDCam_;
   }
   virtual bool isRUCam() {
      return this->isRUCam_;
   }
   virtual bool isRDCam() {
      return this->isRDCam_;
   }
   virtual bool isSkyCam() {
      return this->isSkyCam_;
   }
   virtual void clear();
   virtual void setM2020DpMetaData(M2020DpMetaData * metadata) throw(exception);
   virtual void setFileNameInstr() throw(exception);
   virtual void createLabels() throw(exception);
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
   virtual void getInstCoordFrameId(LblApiIdItem_typ &);
   virtual void setProdNameConfig(char) throw(exception) {
   };

      protected:
   bool isNavCam_;
   bool isHazCam_;
   bool isCacheCam_;
   bool isLCam_;
   bool isPUCam_;
   bool isDDCam_;
   bool isRUCam_;
   bool isRDCam_;
   bool isSkyCam_;
   string dataSetId_, dataSetName_, instName_, prodId_, appProsName_, instId_, prodName_;
   outputFileNameStruct outputFileName_;

    /** Set the outputFileName_ structure using the different setProdName* methods and than
     * creates the product name in prodName_ field.
     */
   virtual void setProdName() throw(exception);

   virtual void setProductName(char *prod_name) {
      this->prodName_ = prod_name;
   }

   virtual void setProdNameSample() throw(exception){}

   virtual void setProdNameDown() throw(exception);

    /** Set the sclk field of outputFileName_ structure. This implementation
     * uses sclk from emd file.
     */
   virtual void setProdNameSclk() throw(exception) {
      TRACE;

      if (this->env_->getM2020UPF().getValue("SOL_OR_DOY") != NULL){
         if (strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "DOY") == 0){
            this->outputFileName_.sclk = this->metadata_->_fnameUtc;
            this->outputFileName_.subsclk = this->metadata_->_fnameSubSec;
         }else if (strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "AUTO") == 0){
            try{
               if (this->idph_ != NULL)
                  this->outputFileName_.sclk = this->idph_->getU32("idph.sclk_seconds");
            } catch(const exception & e) {
            }
         }else{
            if (this->idph_ != NULL)
                this->outputFileName_.sclk = this->idph_->getU32("idph.sclk_seconds");
            else
                this->outputFileName_.sclk = (int)(this->metadata_->_dvtCoarse);
         }
      }else{
         if (this->idph_ != NULL)
            this->outputFileName_.sclk = this->idph_->getU32("idph.sclk_seconds");
         else
            this->outputFileName_.sclk = (int)(this->metadata_->_dvtCoarse);
      }
    }

    /** Set the sub sclk field of outputFileName_ structure. This implementation
     * uses sub sclk from emd file.
     */
   virtual void setProdNameSubSclk() throw(exception) {
      TRACE;

      if (this->env_->getM2020UPF().getValue("SOL_OR_DOY") != NULL){
         if (strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "SOL") == 0 ||
             strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "DOY-SCLK") == 0 ||
             strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "AUTO") == 0){
            int rti;
            if (this->idph_ != NULL){
               rti = (this->idph_->getU32("idph.sclk_subseconds") / 4096);
               rti = (int)((double)rti * 0.000954);   //0.954 micro second intervals, so milliseconds = 0.000954 * sclk_subseconds
            }else{
               rti = (int)(this->metadata_->_dvtFine / 100);
               //rti = (int)((int)(this->metadata_->_dvtFine) * 1000 / 65536);
            }
            //cout << "rti: " << rti << endl;
            this->outputFileName_.subsclk = rti;
            //if(strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "AUTO") == 0 && this->planetDayNumber_<0)
               //this->outputFileName_.subsclk = this->metadata_->_fnameSubSec;
         }
      }
   }

    /** Set the instr field of outputFileName_ structure.
     * This implementation is no-op.
     * The value is set somewhere else.
     */
   virtual void setProdNameInstr() throw(exception) {
      TRACE;
      //THROW_EXCEPTION(M2020Exception::FAILURE,"Not implemented yet...");
      /**NO-OP*/
   }

   virtual const char *getInstrId() throw(exception) {
      return this->outputFileName_.instr;
   }

    /** Set the side field of outputFileName_ structure */
   virtual void setProdNameSide() throw(exception) {
      TRACE;
      //Set default to 'A' if it is not 'B', this handles scarecrow data that has '0' as the value
      if(this->metadata_->_creationStringId[0] != 'B') 
         this->metadata_->_creationStringId[0] = 'A';
      this->outputFileName_.side = this->metadata_->_creationStringId[0];
   }

    /** Set the config field of outputFileName_ structure */
   virtual void setProdNameConfig() throw(exception) {
      TRACE;
      //TODO TODO TODO this is now compression mode/filter
      //add case statement when we get more data examples
      this->outputFileName_.config = '_';
   }

    /** Set the venue field of outputFileName_ structure.
     * This implementation sets this value to '_' for flight
     */
   virtual void setProdNameVenue() throw(exception) {
      TRACE;
      if (this->env_->getM2020UPF().getValue("VENUE") != NULL){
         this->outputFileName_.venue = (char)this->env_->getM2020UPF().getValue("VENUE")[0];
      }else{
         this->outputFileName_.venue = '_';
      }
   }

    /** Set the geom field of outputFileName_ structure.
     * This implementation sets this value to '1' for NavImage
     * '_' for everything else
     */
   virtual void setProdNameGeom() throw(exception) {
      TRACE;
      string s = this->metadata_->_productName;
      //cout << "prod name: " << s << endl;
      if (s.find("NavImage") != string::npos) {
         this->outputFileName_.geom = '1';
      } else {
         this->outputFileName_.geom = '_';
      }
   }

    /** Set the sol field of outputFileName_ structure */
    //Not used in filename
   virtual void setProdNameSol() throw(exception) {
      TRACE;

cout << "setProdNameSol this->planetDayNumber_: " << this->planetDayNumber_ << endl;

      memset(this->outputFileName_.solstr,'\0',5);
      if (this->env_->getM2020UPF().getValue("SOL_OR_DOY") != NULL){
         if (strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "DOY") == 0 ||
             strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "DOY-SCLK") == 0){
            sprintf(this->outputFileName_.solstr,"%c%03u",this->metadata_->_fnameYrChar,this->metadata_->_fnameDoy);
         }else if(strcmp(this->env_->getM2020UPF().getValue("SOL_OR_DOY"), "AUTO") == 0){
            if(this->planetDayNumber_ >= 0){
               this->outputFileName_.sol = this->planetDayNumber_;
               sprintf(this->outputFileName_.solstr,"%04u",this->planetDayNumber_);
cout << "1 this->outputFileName_.solstr: " << this->outputFileName_.solstr << endl;
            }else{
               sprintf(this->outputFileName_.solstr,"%c%03u",this->metadata_->_fnameYrChar,this->metadata_->_fnameDoy);
cout << "2 this->outputFileName_.solstr: " << this->outputFileName_.solstr << endl;
            }
         }else{
            if(this->planetDayNumber_ >= 0){
               this->outputFileName_.sol = this->planetDayNumber_;
               sprintf(this->outputFileName_.solstr,"%04u",this->planetDayNumber_);
cout << "3 this->outputFileName_.solstr: " << this->outputFileName_.solstr << endl;
            }else{
               this->outputFileName_.sol = 0;
               sprintf(this->outputFileName_.solstr,"0UNK");
cout << "4 this->outputFileName_.solstr: " << this->outputFileName_.solstr << endl;
            }
         }
      }else{
         sprintf(this->outputFileName_.solstr,"%s","0UNK");
         this->outputFileName_.sol = 0;
      }
   }

    /** Set the prod field of outputFileName_ structure */
   virtual void setProdNameProd() throw(exception);

    /** Set the site field of outputFileName_ structure */
   virtual void setProdNameSite() throw(exception) {
      TRACE;
      if(this->idph_ != NULL){
         this->outputFileName_.site = this->idph_->getU16("idph.rmc_site");
      }else{
         this->outputFileName_.site = this->metadata_->_siteIndex;
      }
   }

    /** Set the prod field of outputFileName_ structure */
   virtual void setProdNameThumb() throw(exception);

    /** Set the site field of outputFileName_ structure */
   virtual void setProdNameComp() throw(exception);

    /** Set the drive field of outputFileName_ structure */
   virtual void setProdNameDrive() throw(exception) {
      TRACE;
      if(this->idph_ != NULL){
         this->outputFileName_.drive = this->idph_->getU16("idph.rmc_drive");
      }else{
         this->outputFileName_.drive = this->metadata_->_driveIndex;
      }
   }

    /** Set the site field of outputFileName_ structure */
   virtual void setProdNameFilt() throw(exception) {
      TRACE;
      //TODO TODO TODO need algorithm to set filter in filename
      /* From Bob's email on 1/27/17
         Finally, for now, is the filter/color field.  Here's what I recommend:
         
         0-7 = filter (M-Z only).  Filter 0 means 3-band color, all other filters
         are 1 or 3 bands as are normally returned by the camera.
         0-3 = cover/LED status (Watson only).
         M = Bayer-encoded image (as in, single band that includes the Bayer
         pattern).  Includes M-Z and Watson that are not debayered and would
         otherwise have a 0.
         F = Full-color image (3-band RGB)
         R = Red band
         G = Green band
         B = Blue band
         U = Special case for G1 (upper green Bayer cells only)
         L = Special case for G2 (lower green Bayer cells only)
         _ = grayscale (e.g. binned to gray on the camera)
         H = Hue of HSV/HSI color space
         S = Saturation of HSV/HSI color space
         V = Value of HSV/HSI color space
         X = x of xyY color space
         Y = y of xyY color space
         I = Y (intensity) of xyY color space
         
      */
      //this->outputFileName_.filt = this->idph_->getU16("idph.filt");
      this->outputFileName_.filt = '_';
   }

    /** Set the seqid field of outputFileName_ structure */
   virtual void setProdNameSeqid() throw(exception) {
      TRACE;
      string tmpstr = this->metadata_->_seqId;
      //cout << "_seqId: " << this->metadata_->_seqId << endl;
      //cout << "tmpstr.size(): " << tmpstr.size() << endl;
      string tmpstr2;
      if (tmpstr.size() < 9) {
         for (int i = 0; i < 9 - tmpstr.size(); i++)
            tmpstr2 = '0' + tmpstr2;
         //cout << "tmpstr2.size(): " << tmpstr2.size() << endl;
         string tmpstr3 = tmpstr2 + tmpstr;
         //cout << "tmpstr3.size(): " << tmpstr3.size() << endl;
         sprintf(this->outputFileName_.seqid, "%s", tmpstr3.c_str());
         //cout << "if, this->outputFileName_.seqid: " << this->outputFileName_.seqid << endl;
      } else {
         //sprintf(this->outputFileName_.seqid,"%s",this->metadata_->_seqId);
         int i = 0;
         char str[10];
         memset(str, '\0', 10);
         strcpy(str, this->metadata_->_seqId);
         char c;
         while (str[i]) {
            c = str[i];
            str[i] = toupper(c);
            i++;
         }
         sprintf(this->outputFileName_.seqid, "%s", str);
         //cout << "else, this->outputFileName_.seqid: " << this->outputFileName_.seqid << endl;
      }
   }

    /** Set the who field of outputFileName_ structure */
   virtual void setProdNameWho() throw(exception) {
      TRACE;
      this->outputFileName_.who = 'J';
   }

    /** Set the ver field of outputFileName_ structure */
   virtual void setProdNameVer(string filename) throw(exception) {
      TRACE;
      if (this->env_->getM2020UPF().getValue("VERSION_M2020_EDR")) {
         string ver_msl_edr = this->env_->getM2020UPF().getValue("VERSION_M2020_EDR");
         if (ver_msl_edr.find("ON") != string::npos) {
            unsigned int index = filename.find_last_of("-");
            string verstr;
            if (index != string::npos)
               verstr = filename.substr(index + 1);
            index = verstr.find_last_of(".");
            if (index != string::npos)
               verstr = verstr.substr(0, index);

            memset(this->outputFileName_.ver, '\0', 32);
            sprintf(this->outputFileName_.ver, "%s", verstr.c_str());
         } else {
            memset(this->outputFileName_.ver, '\0', 32);
            sprintf(this->outputFileName_.ver, "%s", "0");
         }
      } else {
         memset(this->outputFileName_.ver, '\0', 32);
         sprintf(this->outputFileName_.ver, "%s", "0");
      }
   }

    /** Set the Ext field of outputFileName_ structure */
   virtual void setProdNameExt() throw(exception) {
      TRACE;
      sprintf(this->outputFileName_.ext, "%s", "VIC");
   }

   static void toCharDec(unsigned int val, unsigned int maxStrSize, string & output) throw(exception);

};

#endif            /*M2020ENGCAMVICARLABEL_H_ */
