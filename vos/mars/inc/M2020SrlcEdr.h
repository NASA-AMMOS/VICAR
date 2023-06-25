/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * This is a temporary Srlc EDR generation software that simply takes
 * an engineering EDR labels and slaps them onto Srlc science frame
 * data
 *
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 * @version 1.0
 */
#ifndef M2020SRLCEDR_H_
#define M2020SRLCEDR_H_

#include "M2020EngCamEdr.h"
#include "M2020SrlcPds.h"

class M2020SrlcEdr:public M2020EngCamEdr {
      private:
   string EdrName_;

      protected:

   M2020SrlcPds * pdsLabel_;
   M2020IdphHeader *hdr_;
    std::map < std::string, M2020IdphHeader * >ancHdrs_;
   unsigned int exposure_;
   unsigned int fswver_;
   unsigned int fswverwpnt_;
   list < string > edrList_;

   std::string genSoh_;
   std::string scanSoh_;
   std::string seSoh_;
  
   //Generic header fields
   unsigned short packet_header_marker_;
   unsigned int rce_time_sync_;
   unsigned short data_definition_version_;
   unsigned int nv_xmit_buffer_count_;
   unsigned int cndh_software_version_;
   unsigned int cndh_firmware_version_;
   unsigned int cndh_hardware_identifier_;
   unsigned int se_firmware_version_;
   unsigned int se_hardware_identifier_;
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
   
   unsigned int Data_Length_;
   //unsigned short *Image_Data_;


   virtual bool isHeader(const string & dpoName, int vid) const {
      TRACE;
      //  NUL as it's dpo parser.
      //if (M2020EngCamEdr::isHeader(dpoName,vid)) return true;
      //else if (this->isSrlcHeader(dpoName,vid)) return true;
      if (dpoName.find("Idph") != std::string::npos)
         return true;
      return false;
   }
   virtual bool isSrlcHeader(const string & dpoName, int vid)const {
      return ((dpoName.find("Srlc") != std::string::npos && dpoName.find("Ancillary") != std::string::npos));
   }
   virtual M2020IdphHeader *createHeader(const string & dpoName, int vid, M2020DPOParser * dpoParser);

   virtual bool isBlob(const string & dpoName, int vid) const {
      return false;
   }
      public:
    M2020IdphHeader * ancHdr_;
   explicit M2020SrlcEdr(M2020DpMetaData * m) throw(exception);
    virtual ~ M2020SrlcEdr();
   virtual void checkUPFParams() throw(exception);
   virtual void checkDataFile() throw(exception);
   virtual void parseDPO(unsigned char *sci_data, int sciDataLen, const string & dpoName) throw(exception);
   virtual void processDPOs() throw(exception);
   virtual void process() throw(exception);
   virtual void init() throw(exception);
   virtual void writeProduct(unsigned char *sci_data, int sciDataLen, const string & dpoName) throw(exception);
   virtual void writeProduct() throw(exception);
   virtual void finalize() throw(exception);
   virtual void updateUPF(string pname) throw(exception);
   virtual void updateUPF() throw(exception);
   virtual void writeEdr(unsigned char *edrData, int length, const string & edr_type) throw(exception);
   virtual unsigned int getExposure() const {
      return this->exposure_;
   }
   virtual unsigned int getFSWVer() const {
      return this->fswver_;
   }
   virtual unsigned int getFSWVerWPnt() const {
      return this->fswverwpnt_;
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
   virtual unsigned int getCNDHSoftwareVersion() const {
      return this->cndh_software_version_;
   }
   virtual unsigned int getCNDHFirmwareVersion() const {
      return this->cndh_firmware_version_;
   }
   virtual unsigned int getCNDHHardwareIdentifier() const {
      return this->cndh_hardware_identifier_;
   }
   virtual unsigned int getSEFirmwareVersion() const {
      return this->se_firmware_version_;
   }
   virtual unsigned int getSEHardwareIdentifier() const {
      return this->se_hardware_identifier_;
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

   virtual unsigned int getDataLength() const {
      return this->Data_Length_;
   }

   static const std::string SrlcIdphHdrName;
   static const std::string SrlcIdph1HdrName;
   static const std::string SrlcIdph4HdrName;
   static const std::string SrlcIdph6HdrName;
   static const std::string SrlcArgs1HdrName;
   static const std::string SrlcDefaultHdrName;
   static const std::string SrlcParams3HdrName;
   static const std::string SrlcParams4HdrName;


};

#endif            //M2020SRLCEDR_H_
