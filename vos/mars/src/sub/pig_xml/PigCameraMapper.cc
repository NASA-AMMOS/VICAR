////////////////////////////////////////////////////////////////////////
// PigCameraMapper
//
// This is a concrete base class that manages a Camera Mapping file.
// See the .h file for documentation.
////////////////////////////////////////////////////////////////////////

#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include "PigXerces.h"
#include <string.h>
#include <stdio.h>

////////////////////////////////////////////////////////////////////////
// Create a mapper for the given host.  The XML filename is constructed
// by appending "_camera_mapping.xml" to the mission and host IDs, and using
// PigModelBase::openConfigFile() to find it in the "param_files/" directory.
//
// For BACKWARD COMPATIBILITY with MER only, mission can be null.
////////////////////////////////////////////////////////////////////////

PigCameraMapper::PigCameraMapper(const char *mission, const char *hostID)
{
    _numEntries = 0;
    _host_id = strdup(hostID);
    _xml_filename = NULL;

    char filename[1024];
    char found_path[1024];

    strcpy(filename, "param_files/");
    if (mission != NULL) {		// Remove the : from the mission name
	const char *colon = strchr(mission, ':');
	if (colon == NULL)
            strcat(filename, mission);
	else {
	    strncat(filename, mission, (colon-mission));
	}
        strcat(filename, "_");
    }
    strcat(filename, hostID);
    strcat(filename, "_camera_mapping.xml");

    FILE *fp = PigModelBase::openConfigFile(filename, found_path);

    if (fp == NULL) {
	char msg[1024];
	sprintf(msg, "Error opening camera mapping file '%s'", filename);
	printError(msg);
	return;
    }
    fclose(fp);

    readXMLfile(found_path);
}

////////////////////////////////////////////////////////////////////////
// Create a mapper with an explicit filename.  Mostly a debugging thing;
// the first contructor should normally be used.
////////////////////////////////////////////////////////////////////////

PigCameraMapper::PigCameraMapper(const char *hostID, const char *xmlFilename,
			int xxx)
{
    _numEntries = 0;
    _host_id = strdup(hostID);
    _xml_filename = NULL;

    readXMLfile(xmlFilename);
}

////////////////////////////////////////////////////////////////////////
// Internal routine to read the given XML file.
////////////////////////////////////////////////////////////////////////

void PigCameraMapper::readXMLfile(const char *xmlFilename)
{
    _xml_filename = strdup(xmlFilename);

    DOMDocument *doc = PigXerces::parseFile(_xml_filename);
    if (doc == NULL) {
	char msg[1024];
	sprintf(msg, "Error reading camera mapping file '%s'", _xml_filename);
	printError(msg);
	return;
    }

    DOMElement *root = doc->getDocumentElement(); // should be camera_mapping

    // Verify the host_id against the host ID.

    char *host_id = PigXerces::getAttribute(root, "host_id");
    if (strcasecmp(host_id, _host_id) != 0) {
	char msg[256];
	sprintf(msg, "Host_id of '%s' from camera mapping file '%s' does not match given host ID of '%s'!\n",
		host_id, _xml_filename, _host_id);
	printError(msg);
    }
    XMLString::release(&host_id);

    // Find any <camera> entries and process them...

    DOMNodeList *entries = PigXerces::getElementsByTagName(root, "camera");

    for (int i=0; i<entries->getLength(); i++) {

	DOMElement *camera = PigXerces::nextElement(entries, i);

	// PigCME will handle deallocation of strings...

	PigCameraMapEntry *map = new PigCameraMapEntry(
			PigXerces::getAttributeCstr(camera, "id"),
			PigXerces::getAttributeCstr(camera, "stereo_id"),
			PigXerces::getAttributeCstr(camera, "short_id"),
			PigXerces::getAttributeCstr(camera, "eye_id"),
			PigXerces::getAttributeCstr(camera, "name"),
			PigXerces::getAttributeCstr(camera, "serial_number"),
			PigXerces::getAttributeCstr(camera, "filters"),
			PigXerces::getAttributeCstr(camera, "focus"),
			PigXerces::getAttributeCstr(camera, "zoom"),
			PigXerces::getAttributeCstr(camera, "temperature"),
			PigXerces::getAttributeCstr(camera, "type"),
			PigXerces::getAttributeCstr(camera, "nl"),
			PigXerces::getAttributeCstr(camera, "ns"),
			PigXerces::getAttributeCstr(camera, "color"),
			PigXerces::getAttributeCstr(camera, "var_flat"));

	if (_numEntries >= MAX_CAMERA_ENTRIES)
	    printError("Internal error!!  Too many PigCameraMapEntry objects!");
	else
	    _entries[_numEntries++] = map;
    }

    doc->release();

}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigCameraMapper::~PigCameraMapper()
{
    for (int i=0; i<_numEntries; i++) {
	if (_entries[i] != NULL)
	    delete _entries[i];
    }
    if (_host_id != NULL)
	delete _host_id;
    if (_xml_filename != NULL)
	delete _xml_filename;
}

////////////////////////////////////////////////////////////////////////
// Find an entry given a serial number (treated as string)
////////////////////////////////////////////////////////////////////////

PigCameraMapEntry *PigCameraMapper::findFromSerialNumber(
						const char *serial_number)
{
    for (int i=0; i < _numEntries; i++) {
	char *s = _entries[i]->getSerialNumber();
	if (s == NULL)
	    continue;
	if (strcasecmp(serial_number, s) == 0)
	    return _entries[i];
    }
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Find an entry given an ID.
////////////////////////////////////////////////////////////////////////

PigCameraMapEntry *PigCameraMapper::findFromID(const char *id)
{
    for (int i=0; i < _numEntries; i++) {
	char *s = _entries[i]->getID();
	if (s == NULL)
	    continue;
	if (strcasecmp(id, s) == 0)
	    return _entries[i];
    }
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Find the stereo partner for a given ID
////////////////////////////////////////////////////////////////////////

PigCameraMapEntry *PigCameraMapper::findFromStereoPartnerID(
						const char *stereo_id)
{
    for (int i=0; i < _numEntries; i++) {
	char *s = _entries[i]->getStereoID();
	if (s == NULL)
	    continue;
	if (strcasecmp(stereo_id, s) == 0)
	    return _entries[i];
    }
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Find the stereo partner using the actual entry
////////////////////////////////////////////////////////////////////////

PigCameraMapEntry *PigCameraMapper::findStereoPartner(
						PigCameraMapEntry *orig)
{
    return findFromStereoPartnerID(orig->getID());
}

