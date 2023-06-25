/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/
#ifndef M2020DPOETADATA_H_
#define M2020DPOETADATA_H_
#include "M2020Env.h"
#include "M2020DpMetaData.h"
#include "M2020MainInclude.h"
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>
using namespace std;

#define NODE_STR_LEN 99
#define TEST_BED 158
//#define FLIGHT 76
#define FLIGHT 168
#define IDPH_U32  unsigned int

typedef struct {
   char *enumname;
   char *fsw_value;
   int numeric_value;
   char *dict_value;
} EnumMetaData;

/**
* This class contains Meta Data for a Data Product (DP).
* The information is extracted from a .emd file.
*/
class M2020DPOMetaData {

      public:
   M2020Env * _env;
   M2020DpMetaData *md_;
   string dpoDefRoot_;
   IDPH_U32 vid_;

    vector < EnumMetaData * >dpo_enums_;

   XMLCh _nsStr[100];   // namespace 

   void initialize();
   virtual void parseFile() throw(exception);
   void addDPOEnums(xercesc::DOMDocument * parent);

      public:
   /**
     */
    explicit M2020DPOMetaData(M2020DpMetaData * metaData, IDPH_U32 vid) throw(exception);
    virtual ~ M2020DPOMetaData();

   int dump();      // Dump the object to stdout.

};

#endif            /*M2020DPOMETADATA_ */
