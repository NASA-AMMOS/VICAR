////////////////////////////////////////////////////////////////////////
// PigCameraMapper
//
// This is a concrete base class that manages a Camera Mapping file.
// The CameraMapping file is an XML file that maps between camera name or
// ID and the serial number of the camera, as well as providing some other
// information.
//
// PigCameraMapEntry objects are returned from various search methods in
// this class.  From them, specific items can be extracted.
//
// There is no PigMission factory for this because it is usually called
// from code that is mission-specific anyway (pointing/camera model creation
// code) so there's no need.  For missions with multiple spacecraft (e.g. MER),
// the caller is responsible for creating one of these for each spacecraft
// that might be needed, and calling the right one based on the mission name/
// host_id.  While one of these could be created and destroyed for each file
// being read, that could get expensive for e.g. a mosaic.
//
// Camera mapping files look like this:
//
// <?appropriate xml header junk.../>
// <camera_mapping host_id="MER1">
//     <camera
//         id="PANCAM_RIGHT"
//         stereo_id="PANCAM_LEFT"
//         short_id="P"
//         eye_id="R"
//         name="PANORAMIC CAMERA RIGHT"
//         serial_number="54"
//         filters="true"
//         type="cahvor"
//         nl="1024"
//         ns="1024"
//	   color="false"
//     /camera>
// 
//     <camera
//         id="FRONT_HAZCAM_LEFT"
//         stereo_id="FRONT_HAZCAM_RIGHT"
//         short_id="F"
//         eye_id="L"
//         name="FRONT HAZARD AVOIDANCE CAMERA LEFT"
//         serial_number="42"
//         filters="false"
//         type="cahvore"
//         nl="1024"
//         ns="1024"
//     /camera>
// 
//     ...
// </camera_mapping>
//
// "host_id" is the host ID (actual rover ID) from INSTRUMENT_HOST_ID in a
// label, "id" is INSTRUMENT_ID, "name" is INSTRUMENT_NAME, and "serial_number"
// is INSTRUMENT_SERIAL_NUMBER (notice a pattern?  ;-}  ).
//
// "stereo_id" is the INSTRUMENT_ID for the "stereo partner" camera.  It is
// used to find the stereo partner for a given camera.  "short_id" is a
// single-character instrument ID (matches the EDR/RDR filename convention
// for MER), and "eye_id" is the single character eye ID: L, R, or M (for
// Mono).
//
// "filters" true or false indicate whether or not different filters have
// different camera models for this camera. "type" is the type of the camera
// model.
//
// "nl" and "ns" are optional for older missions (new with MSL) and default
// to 1024 if not given.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCAMERAMAPPER_H
#define PIGCAMERAMAPPER_H

#include "PigModelBase.h"

class PigCameraMapEntry;

#define MAX_CAMERA_ENTRIES 30		// max # of cameras in a host

class PigCameraMapper : public PigModelBase {

  protected:

    PigCameraMapEntry *_entries[MAX_CAMERA_ENTRIES];
    int _numEntries;

    char *_host_id;

    char *_xml_filename;

    void readXMLfile(const char *xmlFilename);

  public:

    // Create a mapper for the given host.  The XML filename is constructed
    // by appending "_camera_mapping.xml" to the mission and host IDs, and
    // using PigModelBase::openConfigFile() to find it.
    // Note that MER incorrectly used only the host ID (and not the mission
    // Thus for BACKWARD COMPATIBILITY ONLY, mission can be null.

    PigCameraMapper(const char *mission, const char *hostID);

    // Create a mapper with an explicit filename.  Mostly a debugging thing;
    // the first contructor should normally be used.  The third argument is
    // simply to disambiguate function signatures and should just be passed
    // as 0.

    PigCameraMapper(const char *hostID, const char *xmlFilename, int xxx);

    virtual ~PigCameraMapper();

    // NOTE:  ALL search routines return a pointer to the internal
    // PigCameraMapEntry structure.  THIS MUST NOT BE MODIFIED!
    // NULL is returned if the entry is not found.

    // Find an entry given a serial number (treated as string)

    virtual PigCameraMapEntry *findFromSerialNumber(const char *serial_number);

    // Find an entry given an ID.

    virtual PigCameraMapEntry *findFromID(const char *id);

    // Find the stereo partner for a given ID

    virtual PigCameraMapEntry *findFromStereoPartnerID(const char *stereo_id);

    // Find the stereo partner using the actual entry

    virtual PigCameraMapEntry *findStereoPartner(PigCameraMapEntry *orig);

    // Accessor routines to allow iterating through all entries.  These
    // return pointers to internal structures so be careful with them!!

    virtual int getNumEntries() { return _numEntries; }
    virtual PigCameraMapEntry *getEntry(int num) { return _entries[num]; }

};

#endif

