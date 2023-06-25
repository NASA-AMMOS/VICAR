////////////////////////////////////////////////////////////////////////
// PigXerces
//
// Simple class whose only job is to initialize (and terminate, in the
// unlikely event that ever gets called) the Xerces C++ package.  For
// whatever reason, the Initialize and Terminate calls must be done only
// once.
//
// A reference count of init calls is kept.  If close() is actually called
// often enough, the package will be closed out.  This is unlikely though
// since most references hang around for the life of the program and thus
// their destructors never get called.
//
// A few convenience functions are also provided.
//
// This class is purely static; it should not be instantiated.
////////////////////////////////////////////////////////////////////////

#include "PigXerces.h"
#include "PigModelBase.h"
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <stdio.h>

int PigXerces::_initCount = 0;

////////////////////////////////////////////////////////////////////////
// Call initialize() before doing any Xerces work.  Calls are
// reference counted so the Xerces init function is not called more
// than once.  Calls to close() are also reference-counted and the
// the Xerces library will be closed if the count ever goes to 0.
////////////////////////////////////////////////////////////////////////

void PigXerces::initialize()
{
    if (_initCount == 0) {
	XMLPlatformUtils::Initialize();
    }
    _initCount++;
}

void PigXerces::close()
{
    _initCount--;
    if (_initCount == 0) {	// if we're called too often, close just once
	// Currently commented out, because any active pointers to DOM
	// structures causes Terminate to crash (!), so we can't do this in
	// any "interesting" destructors.
	// That was in xerces 1, unknown about xerces 3

	// XMLPlatformUtils::Terminate();
    }
}

////////////////////////////////////////////////////////////////////////
// Convenience routine to parse an XML file.  Returns NULL on error.
// The error text is output via PigModelBase::printStaticMsg().
////////////////////////////////////////////////////////////////////////

DOMDocument *PigXerces::parseFile(const char *filename)
{
    char msg[256];

    // It's probably inefficient to create a parser each time, but this
    // routine shouldn't be called all that often, and this way we're
    // more reentrant (just in case).  Who knows, some Java wrapper might
    // want to use this stuff in a thread... (like MICA).

    XercesDOMParser *parser = new XercesDOMParser;

    parser->setValidationScheme(XercesDOMParser::Val_Auto);
    parser->setDoNamespaces(true);
    parser->setDoSchema(false);

    try {
	parser->parse(filename);
    } catch (...) {		// !!!! use real exception error msg !!!!
	sprintf(msg, "Unable to parse XML file %s", filename);
	PigModelBase::printStaticMsg(msg, PigMsgWarning);
	return NULL;
    }

    int count;
    if ((count = parser->getErrorCount()) != 0) {
	sprintf(msg, "Warning: %d errors encountered parsing XML file %s (continuing...)",
			count, filename);
	PigModelBase::printStaticMsg(msg, PigMsgWarning);
    }

    DOMDocument *doc = parser->getDocument();
//!!!! One might think deleting the parser, now that we're done with it, would
//!!!! be a good idea.  Apparently not... it makes the document inaccessible.
//!!!! Gotta love that Xerces documentation...
//!!!!    delete parser;

    if (doc == NULL) {
	sprintf(msg, "Cannot retrieve XML Document object for unknown reasons for file %s", filename);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return NULL;
    }

    return doc;
}

