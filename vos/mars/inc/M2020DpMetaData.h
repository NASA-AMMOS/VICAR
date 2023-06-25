/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/
#ifndef M2020DPMETADATA_H_
#define M2020DPMETADATA_H_
#include "M2020Env.h"
#include "M2020MainInclude.h"
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>
using namespace std;

#define NODE_STR_LEN 99
#define TEST_BED 158
//#define FLIGHT 76
#define FLIGHT 168
/**
* Dpo Meta Data
*/
typedef struct {
   int typ;
   int vid;
   char *name;
   int length;
   int offsetInFile;
   char *status;
   char *partNr;
   long checksum;
   int dataGapOffset;
   int dataGapLength;
} DPOMetaData;

/**
* This class contains Meta Data for a Data Product (DP).
* The information is extracted from a .emd file.
*/
class M2020DpMetaData {

      public:
   M2020Env * _env;

   bool _fswMatched;

   unsigned short _scId, _apId, _vcId, _seqVersion, _cmdNum,
       //_dvtFine,
       _totalParts,
       _totalPartsExp,
       _totalPartsRec,
       _totalMissingParts,
       _activityId,
       _sequenceExecutionCounter,
       _deleteOnSend,
       _productPriority,
       _transmissionControlCriterion,
       _transmissionStatus, _commPath,
       _commSessionId, _commWindowId,
       _dataCompression,
       _fswMode, _siteIndex, _driveIndex, _poseIndex, _armIndex, _chimraIndex, 
       _shaIndex, _drillIndex, _rsmIndex, _hgaIndex, _bitcarIndex, _sealIndex;

   unsigned int      //_seqId,
       _bootCounter,
       _dvtFine,
       _dvtCoarse,
       _cmdFine,
       _cmdCoarse,
       _requestId,
       _checksum,
       _fileSize,
       _partNr,
       _DPONr,
       _partOffset,
       _idphFlag, _tidphFlag, _minSclk, _maxSclk, *_lenList, *_actualLenList, *_offsetList, _packetMapMask;

    vector < DPOMetaData * >dpos_;

   float _roverAttX,
       _roverAttY,
       _roverAttZ,
       _roverAttW, _azimuth, _elevation, _jointAngle[6], _roverPositionX, _roverPositionY, _roverPositionZ;

   char *_seqId,
       *_groundCreationTime,
       *_groundStatus,
       *_productName,
       *_fswVersion,
       *_fswDictVer,
       *_venueTypeId,
       *_venueTestbedName,
       *_venueType,
       *_productTag, *_dataFilename, *_onboardCreationTime, *_sclk, *_scet, *_firstPartErt, *_creationStringId,
       *_commandDispatchSclk,
       //*_xmlVersion,
   *_maxErt, *_minErt;
   XMLCh _nsStr[100];   // namespace 

   unsigned int _fnameYr;
   char _fnameYrChar;
   unsigned int _fnameDoy;
   unsigned int _fnameUtc;
   unsigned int _fnameSubSec;

   string _upfFswVer;
   unsigned int _fswVerNumber;

   void initialize();
   virtual void checkFSWVersion() throw(exception);
   virtual void checkFSWDictLUT() throw(exception);
   virtual void parseFile(char *emdFileName) throw(exception);
   virtual void parseFile() throw(exception);
   void addDPOs(xercesc::DOMDocument * parent);
   void addParts(xercesc::DOMDocument * xmlDoc);
   void addRest(xercesc::DOMDocument * xmlDoc);

   //int seqId (char s)         { return (!(_seqId = s)); };
   //int ert (char *s)          { return (!(_ert = strdup(s))); };
   void setMinMaxErt(char *s);

      public:
   /**
     */
    explicit M2020DpMetaData() throw(exception);
    M2020DpMetaData(char *emdFileName) throw(exception);
    virtual ~ M2020DpMetaData();

    unsigned int getMarsYear(float lsl);

   int dump();      // Dump the object to stdout.
   inline const char *getMaxErt() const {
      return _maxErt;
   };
   inline const char *getMinErt() const {
      return _minErt;
   };

   inline bool isTestBedData() const {
      return this->_scId == TEST_BED;
   }

   //TODO: Need to revisit this. Should be determined using//      idph.serial_no being in a certain range.
       inline bool isFlightData() const {
      //cout << "this->_scId: " << this->_scId << "FLIGHT: " << FLIGHT << endl;
      return this->_scId == FLIGHT;
   }
};

#endif            /*M2020DPMETADATA_ */
