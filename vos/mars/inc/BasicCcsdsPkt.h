//////////////////////////////////////////////////////////////////////////////
//	BasicCcsdsPtk.h
//
// This is the encapsulation of the CCSDS telemetry data package. We
// would like to leave as much open as possible to make this object usefull.
// Applications should derive from this to create their own package format
// instead of using this directly. For further information, please
// review document #: CCSDS102.0-B-4
//
// This object has no mention of the secondary header. It's up to the derived
//      class to define this.
//
// **** This class is more for declaration purpose. See sub-classes to
//	findout how these are defined !!!!
//
//////////////////////////////////////////////////////////////////////////////
#ifndef BASIC_CCSDS_PKT_H
#define BASIC_CCSDS_PKT_H

#ifndef MAXSTRLEN
#define MAXSTRLEN	256
#endif

class LogFile;

class BasicCcsdsPkt {
 protected:
	u_int	_versionId 		: 3,	// *** PRIMARY HEADER FIELDS ***
		_typeIndicator 		: 1,	// see sub-classes for more 
		_hasSecHdr 		: 1,	// details on how these fields 
		_appProcessId 		: 11,	// are used.
		_groupingFlag 		: 2,
		_sequenceCount 		: 14;
	u_int	_dataLen;			// len of data that followed

	BasicCcsdsPkt ();			// default constructor
	BasicCcsdsPkt (	u_int versionId,	// fill in the object's
			u_int typeIndicator,	// attributes with given
			u_int hasSecHdr,	// parameters
			u_int appProcessId,
			u_int groupingFlag,
			u_int sequenceCount,
			u_int dataLen);

	BasicCcsdsPkt (BasicCcsdsPkt &pkt);

 public:	

	~BasicCcsdsPkt ();
	// destructor should set everything to 0

	virtual int isContinuationPkt	() { return (_groupingFlag == 0); };
	virtual int isFirstPkt		() { return (_groupingFlag == 1); };
	virtual int isLastPkt		() { return (_groupingFlag == 2); };
	virtual int isStandAlonePkt	() { return (_groupingFlag == 3); };

	virtual u_int versionId		() { return _versionId;		};
	virtual u_int typeIndicator	() { return _typeIndicator;	};
	virtual u_int hasSecHdr		() { return _hasSecHdr;		};
	virtual u_int appProcessId	() { return _appProcessId;	};
	virtual u_int groupingFlag	() { return _groupingFlag;	};
	virtual u_int sequenceCount	() { return _sequenceCount;	};
	virtual u_short dataLen		() { return _dataLen;		};
	// member functions to retrieve object's internal attributes

	virtual int obj2rawData (u_char *rawData, u_int dataLen);
	// member function to return the object's
	// content in the raw data stream

	virtual int dump (LogFile *logFile, int doCleanUp = 1);
	// get the object's content in the raw data stream, then
	// dump it into the given logFile

	virtual u_int rawDataUsed () { return ((u_int)6); };
	// This should be constant because it's define by the specs. This is
	// a very basic object, therefore, it does not have the secondary
	// header or the data portion. It's up to the sub-class to define
	// these two items. Hopefully, by the time you define your sub-class
	// you should know if you are expecting a secondary header & some more
	// information on what the data portion should look like !!!!

	virtual int zeroOut();
	// member function to clear out object's content. This is usefull
	// when you want to use the same object over & over for different
	// purpose
};

#endif
