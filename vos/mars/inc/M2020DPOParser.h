/*
  Copyright 2008-Present, California Institute of Technology. 
  ALL RIGHTS RESERVED.
  U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020DPOPARSER_H_
#define M2020DPOPARSER_H_

#include "M2020Env.h"
#include "M2020DpMetaData.h"
#include "M2020DPOMetaData.h"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <sstream>
#include <map>
#include <vector>
using namespace std;

#ifdef XERCES_CPP_NAMESPACE_USE
XERCES_CPP_NAMESPACE_USE
#endif
#define IDPH_U8   unsigned char
#define IDPH_U16  unsigned short
#define IDPH_U32  unsigned int
#define IDPH_U64  unsigned long
#define IDPH_I8   char
#define IDPH_I16  short
#define IDPH_I32  int
#define IDPH_I64  long
#define IDPH_F32  float
#define IDPH_F64  double
#define IDPH_ENUM IDPH_U32
#define IDPH_BOOL IDPH_U8
const int IDPH_U8_SIZE = sizeof(IDPH_U8);
const int IDPH_U16_SIZE = sizeof(IDPH_U16);
const int IDPH_U32_SIZE = sizeof(IDPH_U32);
const int IDPH_U64_SIZE = sizeof(IDPH_U64);
const int IDPH_F32_SIZE = sizeof(IDPH_F32);
const int IDPH_F64_SIZE = sizeof(IDPH_F64);

typedef struct {
   string type;
   IDPH_U32 size;
   IDPH_U32 relativeOffset;
} DPOElement;

typedef std::map < std::string, DPOElement * >DPOMAP;

typedef struct {
   DPOMAP *dpoMap_;
   IDPH_U32 dpoLength_;
   IDPH_U8 *dpoData_;
} DPODataMap;

typedef std::map < IDPH_U32, DPODataMap * >DPODATAMAP;

/*! \brief Allows parsing of dpo based on an Ascii description of DPO.
 *
 * This class takes a data file and a tab delimited DPO description file and
 * allows extraction of fields based on field name.
 *
 * @author Alice Stanboli
 */
class M2020DPOParser {

      protected:

   DPODATAMAP dpos_;
   string fswDir_, fswVer_, datFileName_;
   ifstream datFile_;
   long datFileSize_;
   DPODataMap *currentDPO_;
   IDPH_U32 currentDPOLength_;
   bool bigEnd_;
   string dpoDefRoot_;
   M2020Env *env_;
   M2020DpMetaData *md_;
   M2020DPOMetaData *dpoEnums_;
   virtual DPOMAP *createDPOMap(IDPH_U32 vid) throw(exception);

   void get(IDPH_U32 offset, IDPH_U8 &) const;
   void get(IDPH_U32 offset, IDPH_U16 &) const;
   void get(IDPH_U32 offset, IDPH_U32 &) const;
   void get(IDPH_U32 offset, IDPH_U64 &) const;
   void get(IDPH_U32 offset, IDPH_I8 &) const;
   void get(IDPH_U32 offset, IDPH_I16 &) const;
   void get(IDPH_U32 offset, IDPH_I32 &) const;
   void get(IDPH_U32 offset, IDPH_I64 &) const;

   void get(IDPH_U32 offset, IDPH_F32 &) const;
   void get(IDPH_U32 offset, IDPH_F64 &) const;

      public:

   explicit M2020DPOParser(M2020DpMetaData * metaData) throw(exception);
   virtual ~ M2020DPOParser();

   M2020DPOMetaData *getDPOMetaData() const {
      return this->dpoEnums_;
   }
   /*virtual void reset(const char* buffer) throw(exception);
      brief Opens the given dat file for future parsing.

      The file is kept open until next restart or the destruction
      of this object.
    */ virtual void reset(const char *datFile) throw(exception);

   /*!
      \brief Load the dpo from the dat file into a local buffer.

      This method loads the tab delimited dpo definition from the directory
      pointed to by environment variable M2020_DPO_CSV_REPOSITORY or PWD.

      \param vid the vid is used to load the tabulated file
      \param length the length of the dpo to be loaded
      \param offset the offset in the file where the data for this vid starts.
    */
   virtual void load(IDPH_U32 vid, IDPH_U32 length, IDPH_U32 offset) throw(exception);
   virtual void loadNavMapDPO(IDPH_U32 vid, IDPH_U32 length, IDPH_U32 offset) throw(exception);

   /*!
      Returns the type of the field specified by fieldName.

      \param fieldName
      \exception exception if the filed is not part of currently loaded DPO.
    */
   virtual string getType(const char *fieldName) const throw(exception);

   /*!
      Returns the size of DPO as specified in DPO definition.

      \exception exception if the field is not part of currently loaded DPO.
    */
   virtual IDPH_U32 getSize(const char *fieldName) const throw(exception);

   /*!
      Returns the offset of DPO as specified in DPO definition.

      \exception exception if the field is not part of currently loaded DPO.
    */
   virtual IDPH_U32 getOffset(const char *fieldName) const
    throw(exception);

   /*!
      All the getter methods returns the fied, converted to the
      type as the type of the second parameter. All endianess conversations
      are also performed. An exception is thrown if the field is not
      part of the currently loaded DPO.
    */
   void get(const char *fieldName, IDPH_U8 &) const throw(exception);
   void get(const char *fieldName, IDPH_I8 &) const throw(exception);
   void get(const char *fieldName, IDPH_U16 &) const throw(exception);
   void get(const char *fieldName, IDPH_I16 &) const throw(exception);
   void get(const char *fieldName, IDPH_U32 &) const throw(exception);
   void get(const char *fieldName, IDPH_I32 &) const throw(exception);
   void get(const char *fieldName, IDPH_U64 &) const throw(exception);
   void get(const char *fieldName, IDPH_I64 &) const throw(exception);
   void get(const char *fieldName, IDPH_F32 &) const throw(exception);
   void get(const char *fieldName, IDPH_F64 &) const throw(exception);

  /****************************************************************************/
   template < typename T > void get_(const string & fieldName, T & val) const throw(exception) {
      TRACE;

      unsigned int sizeOfVal = sizeof(val);

      IDPH_U32 offset;

      try {
         offset = this->getOffset(fieldName.c_str());
      }
      catch(const M2020Exception & e) {
         string s = "Missing required parameter '";
         s.append(fieldName).append("'");
         THROW_EXCEPTION_E(M2020Exception::PARAMETER_ERROR, e, s.c_str());
      }

      if (offset + sizeOfVal > this->currentDPOLength_) {
         ostringstream s;
         s << "For fieldname='" << fieldName << "' offset+length=" << (offset + sizeOfVal)
             << " is bigger then DPO size=" << this->currentDPOLength_ << endl;
         THROW_EXCEPTION(M2020Exception::PARAMETER_ERROR, s.str().c_str());
      }

      this->get(offset, val);
      /*
         val = this->currentDPOData_[offset];

         for (int i=offset+1; i<offset+sizeOfVal; ++i)
         val = ((val<<8)||this->currentDPOData_[i]);
       */
      ostringstream s;
      s << fieldName << " = " << val << endl;
      this->env_->getM2020Logger().debug(s.str().c_str());
   }
  /** Get entire dpo */
   void get(IDPH_U8 * dpo) const;

  /** Return a string representation of this DPO. The string contains fieldName value type and offset
   *  on each line separated by tabs.
   **/
   string toString() const;
};

#endif
