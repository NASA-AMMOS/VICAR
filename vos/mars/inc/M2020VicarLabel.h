/*
  Copyright 2008-Present, California Institute of Technology. 
  ALL RIGHTS RESERVED.
  U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020VICARLABEL_H_
#define M2020VICARLABEL_H_

//#include "M2020Exception.h"

#include "lbl_identification.h"
#include "lbl_telemetry.h"
#include "lbl_instrument_state.h"
#include "lbl_pdshistory.h"
#include "lbl_derived_image.h"
#include "lbl_image_data.h"
#include "lbl_compression.h"
#include "lbl_product_request.h"
#include "lbl_command.h"
#include "lbl_articulation.h"
#include "lbl_coordinate.h"
#include "lbl_derived_geometry.h"
#include "lbl_observation_request.h"
#include "lbl_camera_model.h"
#include "lbl_initstate.h"
#include "lbl_video_parms.h"

#include "M2020Label.h"
#include "M2020IdphHeader.h"
#include "M2020DPOParser.h"
//#include "M2020Edr.h"

#ifndef ERR_SIZE
#define ERR_SIZE 321
#endif

class M2020ImgEdr;
#define IN_RANGE(val,min,max) ((val>=min && val<=max) ? true : false)

#define M2020_ARM_COORDINATE_SYSTEM_PROPERTY_NAME "ARM_COORDINATE_SYSTEM"
#define M2020_LOCAL_LEVEL_COORDINATE_SYSTEM_PROPERTY_NAME "LOCAL_LEVEL_COORDINATE_SYSTEM"
//#define M2020_LOCAL_LEVEL_COORDINATE_SYSTEM_PROPERTY_NAME "LOCAL_LEVEL_COORD_SYSTEM"
#define M2020_RSM_COORDINATE_SYSTEM_PROPERTY_NAME "RSM_COORDINATE_SYSTEM"
#define M2020_DRILL_COORDINATE_SYSTEM_PROPERTY_NAME "DRILL_COORDINATE_SYSTEM"
#define M2020_ROVER_COORDINATE_SYSTEM_PROPERTY_NAME "ROVER_COORDINATE_SYSTEM"
#define M2020_SITE_COORDINATE_SYSTEM_PROPERTY_NAME "SITE_COORDINATE_SYSTEM"
#define M2020_SAVED_FRAME_COORDINATE_SYSTEM_PROPERTY_NAME "SAVED_FRAME_COORDINATE_SYSTEM"
#define M2020_TURRET_COORDINATE_SYSTEM_PROPERTY_NAME "TURRET_COORDINATE_SYSTEM"
#define M2020_TOOL_COORDINATE_SYSTEM_PROPERTY_NAME "TOOL_COORDINATE_SYSTEM"

#define M2020_CHASSIS_ARTICULATION_STATE_PROPERTY_NAME "CHASSIS_ARTICULATION_STATE"
//#define M2020_CHASSIS_ARTICULATION_STATE_PROPERTY_NAME "CHASSIS_ARTICULATION_ST"
#define M2020_FILTER_ARTICULATION_STATE_PROPERTY_NAME "FILTER_ARTICULATION_STATE"
#define M2020_HGA_ARTICULATION_STATE_PROPERTY_NAME "HGA_ARTICULATION_STATE"
#define M2020_SHA_ARTICULATION_STATE_PROPERTY_NAME "SHA_ARTICULATION_STATE"
#define M2020_SCS_ARTICULATION_STATE_PROPERTY_NAME "SCS_ARTICULATION_STATE"
#define M2020_DRILL_ARTICULATION_STATE_PROPERTY_NAME "DRILL_ARTICULATION_STATE"
#define M2020_ARM_ARTICULATION_STATE_PROPERTY_NAME "ARM_ARTICULATION_STATE"
#define M2020_RSM_ARTICULATION_STATE_PROPERTY_NAME "RSM_ARTICULATION_STATE"

#define M2020_ROVER_DERIVED_GEOMETRY_PARAMS_PROPERTY_NAME "ROVER_DERIVED_GEOMETRY_PARMS"
#define M2020_SITE_DERIVED_GEOMETRY_PARAMS_PROPERTY_NAME "SITE_DERIVED_GEOMETRY_PARMS"
#define M2020_INIT_STATE_PARAMS_PROPERTY_NAME "INITIAL_STATE_PARMS"
#define M2020_MINI_HEADER_PARAMS_PROPERTY_NAME "MINI_HEADER_PARMS"
#define M2020_VIDEO_PARMS_PROPERTY_NAME "VIDEO_PARMS"

class M2020VicarLabel:public M2020Label {
      protected:

   M2020Env * env_;
   M2020IdphHeader *idph_;

   LblIdentification_typ idElem_;
   LblTelemetry_typ idTelem_;
   LblPdsHistory_typ idHist_;

   LblCoordinate_typ idRoverCoor_;
   LblCoordinate_typ idSiteCoor_;
   LblCoordinate_typ idLocalCoor_;
   LblCoordinate_typ idDrillCoor_;
   LblCoordinate_typ idArmCoor_;
   LblCoordinate_typ idRsmCoor_;
   LblCoordinate_typ idTurretCoor_;
   LblCoordinate_typ idToolCoor_;
   LblCoordinate_typ idSvdFrmCoor_;

   LblArticulation_typ idRsmArt_;
   LblArticulation_typ idArmArt_;
   LblArticulation_typ idChasArt_;
   LblArticulation_typ idHgaArt_;
   LblArticulation_typ idShaArt_;
   LblArticulation_typ idScsArt_;
   LblArticulation_typ idDrillArt_;

   LblInstrumentState_typ idInstState_;
   LblImageData_typ idImg_;
   LblDerivedImage_typ idDerivedImg_;

   LblVideoParms_typ idVideo_;

   LblObsRequest_typ idObsReq_;

   LblProdRequest_typ idImgReq_;
   LblProdRequest_typ idTmbReq_;
   LblProdRequest_typ idColReq_;
   LblProdRequest_typ idRowReq_;
   LblProdRequest_typ idHistReq_;
   LblProdRequest_typ idSubReq_;
   LblProdRequest_typ idZstackReq_;
   LblProdRequest_typ idVideoReq_;

   LblCompression_typ idCmprs_;

   LblCameraModel_typ idGeoCamMdl_;

   LblDerivedGeometry_typ idDerivedGeoRover_;
   LblDerivedGeometry_typ idDerivedGeoSite_;
   // LblDerivedGeometry_typ idDrivedGeoRover_;
   //LblDerivedGeometry_typ idDrivedGeoRover_;
   //LblCommand_typ idCmd_;

   LblInitState_typ idInitStateParms_;

   M2020ImgEdr *edr_;

   virtual void initialize();

   virtual void createIdentificationLabels() throw(exception);
   virtual void createTelemetryLabels() throw(exception);
   virtual void createPdsHistoryLabels() throw(exception);
   virtual void createCoordinateLabels() throw(exception);
   virtual void createRsmArticulationLabels() throw(exception);
   virtual void createArmArticulationLabels() throw(exception);
   virtual void createChasArticulationLabels() throw(exception);
   virtual void createHgaArticulationLabels() throw(exception);
   virtual void createShaArticulationLabels() throw(exception);
   virtual void createScsArticulationLabels() throw(exception);
   virtual void createDrillArticulationLabels() throw(exception);
   virtual void createObservationRequestLabels() throw(exception);
   virtual void createInstrumentStateLabels() throw(exception);
   virtual void createImageLabels() throw(exception);
   virtual void createDerivedImageLabels() throw(exception);
   virtual void createDerivedGeometryLabels() throw(exception);
   virtual void createDerivedRoverGeometryLabels() throw(exception);
   virtual void createDerivedSiteGeometryLabels() throw(exception);
   //virtual void createImageHeaderLabels     () throw (exception);
   virtual void createProductRequestLabels() throw(exception);
   virtual void createCompressionLabels() throw(exception);
   virtual void createGeometricCameraModelLabels() throw(exception);
   virtual void createInitStateParmsLabels() throw(exception);
   //virtual void createCommandLabels() throw (exception);

   virtual void writeIdentificationLabels(int unit) throw(exception);
   virtual void writeTelemetryLabels(int unit) throw(exception);
   virtual void writePdsHistoryLabels(int unit) throw(exception);
   virtual void writeCoordinateLabels(int unit) throw(exception);
   virtual void writeRsmArticulationLabels(int unit) throw(exception);
   virtual void writeArmArticulationLabels(int unit) throw(exception);
   virtual void writeChasArticulationLabels(int unit) throw(exception);
   virtual void writeHgaArticulationLabels(int unit) throw(exception);
   virtual void writeShaArticulationLabels(int unit) throw(exception);
   virtual void writeScsArticulationLabels(int unit) throw(exception);
   virtual void writeDrillArticulationLabels(int unit) throw(exception);
   virtual void writeObservationRequestLabels(int unit) throw(exception);
   virtual void writeInstrumentStateLabels(int unit) throw(exception);
   virtual void writeImageLabels(int unit) throw(exception);
   virtual void writeDerivedImageLabels(int unit) throw(exception);
   virtual void writeDerivedGeometryLabels(int unit) throw(exception);
   virtual void writeDerivedRoverGeometryLabels(int unit) throw(exception);
   virtual void writeDerivedSiteGeometryLabels(int unit) throw(exception);
   //virtual void writeImageHeaderLabels     (int unit) throw (exception);
   virtual void writeProductRequestLabels(int unit) throw(exception);
   virtual void writeCompressionLabels(int unit) throw(exception);
   virtual void writeGeometricCameraModelLabels(int unit) throw(exception);
   virtual void writeInitStateParmsLabels(int unit) throw(exception);
   //virtual void writeCommandLabels() throw (exception);

   virtual void readIdentificationLabels(int unit) throw(exception);
   virtual void readTelemetryLabels(int unit) throw(exception);
   virtual void readPdsHistoryLabels(int unit) throw(exception);
   virtual void readCoordinateLabels(int unit) throw(exception);
   virtual void readRsmArticulationLabels(int unit) throw(exception);
   virtual void readArmArticulationLabels(int unit) throw(exception);
   virtual void readChasArticulationLabels(int unit) throw(exception);
   virtual void readHgaArticulationLabels(int unit) throw(exception);
   virtual void readShaArticulationLabels(int unit) throw(exception);
   virtual void readScsArticulationLabels(int unit) throw(exception);
   virtual void readDrillArticulationLabels(int unit) throw(exception);
   virtual void readObservationRequestLabels(int unit) throw(exception);
   virtual void readInstrumentStateLabels(int unit) throw(exception);
   virtual void readImageLabels(int unit) throw(exception);
   virtual void readDerivedImageLabels(int unit) throw(exception);
   virtual void readDerivedGeometryLabels(int unit) throw(exception);
   virtual void readDerivedRoverGeometryLabels(int unit) throw(exception);
   virtual void readDerivedSiteGeometryLabels(int unit) throw(exception);
   //virtual void readImageHeaderLabels     (int unit) throw (exception);
   virtual void readProductRequestLabels(int unit) throw(exception);
   virtual void readCompressionLabels(int unit) throw(exception);
   virtual void readGeometricCameraModelLabels(int unit) throw(exception);
   virtual void readInitStateParmsLabels(int unit) throw(exception);
   //virtual void readCommandLabels() throw (exception);
   //virtual void getInstId() throw (exception) const = 0;

   static const char *FilterId[];
   static const char *InstCmprsId[];
   static const char *InstCmprsName[];

   const char *calculateSampleBitModeId(IDPH_I32 scale) const;

   int planetDayNumber_;   // planet day number
   double rover_az_, rover_el_, site_az_, site_el_, solar_az_, solar_el_;

   /*
      inline bool IN_RANGE(IDPH_I8 val, IDPH_I8 min, IDPH_I8 max) { return ((val>=min && val<=max)?true:false); }
      inline bool IN_RANGE(IDPH_U8 val, IDPH_U8 min, IDPH_U8 max) { return ((val>=min && val<=max)?true:false); }

      inline bool IN_RANGE(IDPH_I16 val, IDPH_I16 min, IDPH_I16 max) { return ((val>=min && val<=max)?true:false); }
      inline bool IN_RANGE(IDPH_U16 val, IDPH_U16 min, IDPH_U16 max) { return ((val>=min && val<=max)?true:false); }

      inline bool IN_RANGE(IDPH_I32 val, IDPH_I32 min, IDPH_I32 max) { return ((val>=min && val<=max)?true:false); }
      inline bool IN_RANGE(IDPH_U32 val, IDPH_U32 min, IDPH_U32 max) { return ((val>=min && val<=max)?true:false); }

      inline bool IN_RANGE(IDPH_I64 val, IDPH_I64 min, IDPH_I64 max) { return ((val>=min && val<=max)?true:false); }
      inline bool IN_RANGE(IDPH_U64 val, IDPH_U64 min, IDPH_U64 max) { return ((val>=min && val<=max)?true:false); }

      inline bool IN_RANGE(IDPH_F32 val, IDPH_F32 min, IDPH_F32 max) { return ((val>=min && val<=max)?true:false); }
      inline bool IN_RANGE(IDPH_F64 val, IDPH_F64 min, IDPH_F64 max) { return ((val>=min && val<=max)?true:false); }
    */
   virtual void NO_YES(const char *field, LblApiFlagItem_typ & item);

   virtual void FALSE_TRUE(const char *field, LblApiFlagItem_typ & item);

   virtual void POPULATE(const char *field, LblApiIntItem_typ & item, IDPH_I8 min, IDPH_I8 max);
   virtual void POPULATE(const char *field, LblApiIntItem_typ & item, IDPH_U8 min, IDPH_U8 max);

   virtual void POPULATE(const char *field, LblApiIntItem_typ & item, IDPH_I16 min, IDPH_I16 max);
   virtual void POPULATE(const char *field, LblApiIntItem_typ & item, IDPH_U16 min, IDPH_U16 max);

   virtual void POPULATE(const char *field, LblApiIntItem_typ & item, IDPH_I32 min, IDPH_I32 max);
   virtual void POPULATE(const char *field, LblApiIntItem_typ & item, IDPH_U32 min, IDPH_U32 max);

   virtual void POPULATE(const char *field, LblApiIntItem_typ & item, IDPH_I64 min, IDPH_I64 max);
   virtual void POPULATE(const char *field, LblApiIntItem_typ & item, IDPH_U64 min, IDPH_U64 max);

   virtual void POPULATE(const char *field, LblApiRealItem_typ & item, IDPH_F32 min, IDPH_F32 max);
   virtual void POPULATE(const char *field, LblApiDoubleItem_typ & item, IDPH_F64 min, IDPH_F64 max);

  /*****************************************************************************/
    template < typename E, typename T > void
    POPULATE_(const M2020IdphHeader * hdr, const string & fieldName, E & item, T min, T max, bool required = false)
   const throw(exception) {

      try {
         this->POPULATE_ < E, T > (hdr, fieldName, item, required);
      } catch(const M2020Exception & e) {
         if (required)
            THROW_EXCEPTION_E(M2020Exception::PARAMETER_ERROR, e, "Error obtaining filed.");
         else {
            item.Valid = LBL_PDS_UNK;
            if (this->env_->isUsable())
               this->env_->getM2020Logger().warn(e.what());
         }
      }

      if (item.Valid == LBL_VALID && (item.Value < min || item.Value > max)) {
         item.Valid = LBL_PDS_UNK;
         ostringstream s;
         s << "Field '" << fieldName << "' value=" << item.Value
             << " is out of specified range [" << min << "," << max << "]" << endl;
         if (this->env_->isUsable())
            this->env_->getM2020Logger().warn(s.str().c_str());

         //Temporary taken out due to some Ccam RMI data debugging. --cxr 10/28/2010
         //THROW_EXCEPTION(M2020Exception::PARAMETER_ERROR,s.str().c_str());
      }
   }
  /*****************************************************************************/
   template < typename E, typename T > void
    POPULATE_(const M2020IdphHeader * hdr, const string & fieldName,
         E & item, bool required = false) const throw(exception) {
      try {
         item.Value = this->getValue_ < T > (hdr, fieldName, item.Valid, (T) 0);
      }
      catch(const M2020Exception & e) {
         item.Valid = LBL_PDS_UNK;
         if (required) {
            string s = "Error obtaining field '";
            s.append(fieldName).append("'");
            THROW_EXCEPTION_E(M2020Exception::PARAMETER_ERROR, e, s.c_str());
         } else {
            item.Valid = LBL_PDS_UNK;
            if (this->env_->isUsable())
               this->env_->getM2020Logger().warn(e.what());
         }
      }
   }

  /*****************************************************************************/
   template < typename T >
       T
       getValue_(const M2020IdphHeader * hdr, const string & fieldName,
            int &valid, T defVal, bool required = false) const throw(exception) {
      T val = defVal;
      if (hdr == NULL) {
         valid = LBL_PDS_UNK;
         string s = "Cannot populate field '";
         s.append(fieldName).append("' since the provided header is NULL");
         if (required) {
            THROW_EXCEPTION(M2020Exception::PARAMETER_ERROR, s.c_str());
         } else {
            if (this->env_->isUsable())
               this->env_->getM2020Logger().warn(s.c_str());
         }
         //cerr<<fieldName<<" assigned value="<<val<<endl;
         return val;
      }

      try {
         val = hdr->getValue_ < T > (fieldName);
         valid = LBL_VALID;
      }
      catch(const M2020Exception & e) {
         val = LBL_PDS_UNK;
         string s = "Error obtaining field '";
         s.append(fieldName).append("'");
         if (required)
            THROW_EXCEPTION_E(M2020Exception::PARAMETER_ERROR, e, s.c_str());
         else {
            if (this->env_->isUsable())
               this->env_->getM2020Logger().warn(s.c_str());
         }
      }
      return val;
   }
  /**************************************************************************/

      public:
   explicit M2020VicarLabel(M2020ImgEdr *);
   virtual ~ M2020VicarLabel();

   virtual void setProdName() throw(exception) = 0;
   virtual void setFileNameInstr() throw(exception) = 0;
   virtual void setProdNameConfig(char) throw(exception) = 0;
   virtual void setProductName(char *) = 0;

   virtual const char *getDataSetID() = 0;
   virtual const char *getProductID() = 0;
   virtual const char *getApplicationProcessName() = 0;
   virtual const char *getInstrumentName() = 0;
   virtual const char *getInstrumentId() = 0;
   virtual const char *getProductName() = 0;
   virtual const char *getDataSetName() = 0;

   virtual void getInstCoordFrameId(LblApiIdItem_typ &) = 0;
   static void getParam(char *name, void *value, int *count, int maxcnt, int length, void *clientData);

   virtual void setAncHdr(M2020DpMetaData * dpMetadata) throw(exception);
   virtual void setMMMAncHdr(M2020DpMetaData * dpMetadata,
              M2020IdphHeader * idph,
              M2020IdphHeader * ancidph, M2020IdphHeader * supidph) throw(exception);
   virtual void setPixelAncHdr(M2020DpMetaData * dpMetadata,
                M2020IdphHeader * idph,
                M2020IdphHeader * ancidph, M2020IdphHeader * supidph) throw(exception);
   virtual void setSherlocAncHdr(M2020DpMetaData * dpMetadata,
                  M2020IdphHeader * idph,
                  M2020IdphHeader * ancidph, M2020IdphHeader * supidph) throw(exception);
   virtual void setNavMapAncHdr(M2020DpMetaData * dpMetadata,
                 M2020IdphHeader * idph, M2020IdphHeader * ancidph) throw(exception);
   virtual void setHeader(M2020InstrumentHeader * header) throw(exception);
  /**
   * Call individual create methods.
   */
   virtual void createLabels() throw(exception);

   virtual void readLabels(int unit) throw(exception);
  /**
   * Calling this method will throw an exception
   */
   virtual void writeLabels(string & labels) throw(exception) {
      TRACE;
      THROW_EXCEPTION(M2020Exception::FAILURE, "Method not supported");
   }

  /**
   * Call individual write methods.
   */
   virtual void writeLabels(int unit) throw(exception);

   virtual const char *getInstrId() throw(exception) = 0;

};

#endif            /*M2020VICARLABEL_H_ */
