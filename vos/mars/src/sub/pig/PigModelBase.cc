////////////////////////////////////////////////////////////////////////
// PigModelBase
//
// Base class for all Pig models.  The base class provides the following
// services:
//   * Message output
//   * Obtaining user parameters
//   * Defining the default coordinate system (not yet implemented)
////////////////////////////////////////////////////////////////////////

#include "PigModelBase.h"
#include "zvproto.h"
#include "applic.h"		/* for SUCCESS */

#include <string.h>
#include <ctype.h>

// Static initializers

PigMsgFunction PigModelBase::_defaultMsgFunction = PigMsgPrinter;
void *PigModelBase::_defaultMsgClientData = NULL;
PigParamFunction PigModelBase::_defaultParamFunction = PigParamGetter;
void *PigModelBase::_defaultParamClientData = NULL;

char *PigModelBase::unique_msgs[PIG_MAX_UNIQUE_MSGS];
int PigModelBase::unique_msg_count = 0;
int PigModelBase::unique_msg_printed = 0;

////////////////////////////////////////////////////////////////////////
// This function must be called for printing ALL messages within
// the Pig models.  DO NOT USE zvmessage() or printf() directly.
// The msg string should not end in a newline (it is automatically added).
////////////////////////////////////////////////////////////////////////

void PigModelBase::printMsg(char *msg, PigMsgSeverity severity) const
{
    if (_msgFunction)			// call specific class version
	(*_msgFunction)(msg, severity, _msgClientData);
    else if (_defaultMsgFunction)	// global default
	(*_defaultMsgFunction)(msg, severity, _defaultMsgClientData);
    else				// shouldn't happen, but just in case...
	PigMsgPrinter(msg, severity, NULL);
}

////////////////////////////////////////////////////////////////////////
// Like the above, except it can put [key] in front of the message, a la
// zvmessage().
////////////////////////////////////////////////////////////////////////

void PigModelBase::printMsg(char *msg, char *key, PigMsgSeverity severity) const
{
    char msgbuf[512];

    if (key)
	sprintf(msgbuf, "[%s] %s", key, msg);
    else
	strcpy(msgbuf, msg);
    printMsg(msgbuf, severity);
}

////////////////////////////////////////////////////////////////////////
// STATIC function to be used ONLY in factory classes that create
// models, when a message needs to be printed but no model is available.
// Uses the default message function.  There are no convenience functions.
// The msg string should not end in a newline (it is automatically added).
////////////////////////////////////////////////////////////////////////

void PigModelBase::printStaticMsg(char *msg, PigMsgSeverity severity)
{
    if (_defaultMsgFunction)		// global default
	(*_defaultMsgFunction)(msg, severity, _defaultMsgClientData);
    else				// shouldn't happen, but just in case...
	PigMsgPrinter(msg, severity, NULL);
}

////////////////////////////////////////////////////////////////////////
// STATIC function to be used ONLY in factory classes that create
// models, when a message needs to be printed but no model is available.
// Uses the default message function.  There are no convenience functions.
// The msg string should not end in a newline (it is automatically added).
//
// The difference is, this function prints the message ONLY if it is unique,
// i.e. we haven't printed one like it before.  This is intended to suppress
// the endless "camera model created" messages from things like mosaic programs.
// It's useful to see where the models come from, but we don't have to see
// it over and over and over again.
////////////////////////////////////////////////////////////////////////

void PigModelBase::printUniqueStaticMsg(char *msg, PigMsgSeverity severity)
{
    int found = 0;
    for (int i=0; i < unique_msg_count; i++) {
	if (strcmp(msg, unique_msgs[i]) == 0) {
	    found++;
	    break;
	}
    }
    if (unique_msg_count == 0 || found == 0) {

	printStaticMsg(msg, severity);

	if (unique_msg_count < PIG_MAX_UNIQUE_MSGS) {
	    unique_msgs[unique_msg_count] = strdup(msg);
	    if (unique_msgs[unique_msg_count] == NULL) {
		printStaticMsg("Allocation error in printUniqueStaticMsg", PigMsgError);
	    }
	    unique_msg_count++;
	}
    }
    else {
	if (!unique_msg_printed) {
	    printStaticMsg("Non-unique messages suppressed...", PigMsgInfo);
	    unique_msg_printed++;
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Like the above, except it can put [key] in front of the message, a la
// zvmessage().
////////////////////////////////////////////////////////////////////////

void PigModelBase::printStaticMsg(char *msg, char *key, PigMsgSeverity severity)
{
    char msgbuf[512];

    if (key)
	sprintf(msgbuf, "[%s] %s", key, msg);
    else
	strcpy(msgbuf, msg);
    printStaticMsg(msgbuf, severity);
}

////////////////////////////////////////////////////////////////////////
// This function muust be called for obtaining ALL user parameters
// within the Pig models.  DO NOT USE zvp() et al directly.
// Note that the data type returned is implicit in the name, a la
// zvp and friends (which examine the PDF for the data type).
// Be careful about this.  "count" (the returned # of params) may
// be NULL.  If maxcnt is 0, there is no specific maximum.  "length"
// is the size of each string for a string array (2-D array of chars)
// and may be 0 for a single string or a non-string value.
////////////////////////////////////////////////////////////////////////

void PigModelBase::getParam(char *name, void *value, int *count, int maxcnt,
							int length) const
{
    if (_paramFunction)			// call specific class version
	(*_paramFunction)(name, value, count, maxcnt, length, _paramClientData);
    else if (_defaultParamFunction)	// global default
	(*_defaultParamFunction)(name, value, count, maxcnt, length,
						_defaultParamClientData);
    else				// shouldn't happen, but just in case...
	PigParamGetter(name, value, count, maxcnt, length, NULL);
}

////////////////////////////////////////////////////////////////////////
// STATIC function to be used ONLY in factory classes that create
// models, when a parameter needs to be obtained but no model is available.
// Uses the default parameter function.  There are no convenience functions.
////////////////////////////////////////////////////////////////////////

void PigModelBase::getStaticParam(char *name, void *value, int *count,
							int maxcnt, int length)
{
    if (_defaultParamFunction)		// global default
	(*_defaultParamFunction)(name, value, count, maxcnt, length,
						_defaultParamClientData);
    else				// shouldn't happen, but just in case...
	PigParamGetter(name, value, count, maxcnt, length, NULL);
}

////////////////////////////////////////////////////////////////////////
// Default constructor/destructor.
////////////////////////////////////////////////////////////////////////

PigModelBase::PigModelBase()
{
    _msgFunction = NULL;
    _msgClientData = NULL;
    _paramFunction = NULL;
    _paramClientData = NULL;
}

PigModelBase::~PigModelBase()
{
    // empty
}

////////////////////////////////////////////////////////////////////////
// STATIC function that sets the default message printer for ALL
// models that aren't specifically overridden.
////////////////////////////////////////////////////////////////////////

void PigModelBase::setDefaultMsgFunction(PigMsgFunction func, void *clientData)
{
    _defaultMsgFunction = func;
    _defaultMsgClientData = clientData;
    if (func == NULL)
	_defaultMsgFunction = PigMsgPrinter;
}

////////////////////////////////////////////////////////////////////////
// Sets the message printer for THIS class only.  If this function is
// not called, or the function is set to NULL, then the default is used.
// Note that changing the default *will* change the function used in this
// case (i.e. the default is checked on each call).
////////////////////////////////////////////////////////////////////////

void PigModelBase::setMsgFunction(PigMsgFunction func, void *clientData)
{
    _msgFunction = func;
    _msgClientData = clientData;
}

////////////////////////////////////////////////////////////////////////
// STATIC function that sets the default parameter processor for ALL
// models that aren't specifically overridden.
////////////////////////////////////////////////////////////////////////

void PigModelBase::setDefaultParamFunction(PigParamFunction func,
							void *clientData)
{
    _defaultParamFunction = func;
    _defaultParamClientData = clientData;
    if (func == NULL)
	_defaultParamFunction = PigParamGetter;
}


////////////////////////////////////////////////////////////////////////
// Sets the parameter processor for THIS class only.  If this function is
// not called, or the function is set to NULL, then the default is used.
// Note that changing the default *will* change the function used in this
// case (i.e. the default is checked on each call).
////////////////////////////////////////////////////////////////////////

void PigModelBase::setParamFunction(PigParamFunction func, void *clientData)
{
    _paramFunction = func;
    _paramClientData = clientData;
}

////////////////////////////////////////////////////////////////////////
// This is the ultimate default print function.  It simply calls
// zvmessage() and ignores the severity... except that Debug messages
// are not printed.
////////////////////////////////////////////////////////////////////////

void PigMsgPrinter(char *msg, PigMsgSeverity severity, void *clientData)
{
    if (severity != PigMsgDebug)
	zvmessage(msg, "");
}

////////////////////////////////////////////////////////////////////////
// This is the ultimate default parameter processor.  It simply calls
// zvparmd().
////////////////////////////////////////////////////////////////////////

void PigParamGetter(char *name, void *value, int *count, int maxcnt, int length,
							void *clientData)
{
    int def;		// dummy
    zvparmd(name, value, count, &def, maxcnt, length);
}

////////////////////////////////////////////////////////////////////////
// This parameter processor can be used with interactive or RT programs.
// It simply returns (count=0) for all parameter requests except
// CONFIG_PATH (which is required for most processing).
//
// Note that this function is not called normally; the user must register
// it if desired.
////////////////////////////////////////////////////////////////////////

void PigSimpleParamGetter(char *name, void *value, int *count, int maxcnt,
			int length, void *clientData)
{
    if (strcasecmp(name, "CONFIG_PATH") == 0) {
	if (length == 0 || length > 17)
	    strcpy((char *)value, "$MARS_CONFIG_PATH");
	else
	    strcpy((char *)value, "");
	*count = 1;
	return;
    }
    // Return nothing for any other parameter
    *count = 0;
    return;
}

////////////////////////////////////////////////////////////////////////
// STATIC function to find a given configuration/calibration file.
// This function should be called (as a static) by subclasses as well.
// The filename is supplied as a partial path, relative to the "root"
// of the config file tree.  A list of config file paths is obtained,
// and prepended to this partial path in order until the file is
// found.
//
// The file will be returned opened by fopen() for read.  The pathname
// by which the file was found will be copied into the "found_path"
// parameter.  If no file is found, NULL is returned, and an empty
// string is copied into "found_path".  "found_path" may be passed in
// as NULL, in which case it is ignored.
//
// The following parameter is expected if this routine is used:
//
// PARM CONFIG_PATH STRING DEFAULT="$MARS_CONFIG_PATH"
//
// The parameter is a string, containing a colon-separated list of
// pathnames.  Any environment variables ( $name or ${name} ) are
// translated first, allowing them to be used to represent a common list
// of pathnames (e.g. the default value).
//
// Each element of the path list is searched in turn, with the supplied
// partial path appended to the name (with a "/" separator).  The function
// returns at the first successful fopen().
//
// For example, the config path (after variable substitution) might be
// /home/me/mod_config:/some/disk/mpf/calibration:/some/disk/m98/calibration
// and the partial path might be "point_files/M98_RAC_FM.point" or
// "flat_fields/flat_field_SSI_L05_flight.vicar".
////////////////////////////////////////////////////////////////////////

FILE *PigModelBase::openConfigFile(char *name, char *found_path)
{
    char path_list[1024], orig_path_list[1024];	// max length of path string
    char *path_element[128];			// max # of path elements
    char pathname[1024];
    int count, status;
    FILE *f;

    if (found_path)
	strcpy(found_path, "");			// in case of error

    getStaticParam("CONFIG_PATH", orig_path_list, &count, 1,
						sizeof(orig_path_list)-1);

    if (count == 0)			// no path, return not found
	return NULL;

    // Expand environment variables (and ~ if the first character)

    status = zvfilename(orig_path_list, path_list, sizeof(path_list)-1);
    if (status != SUCCESS) {
	printStaticMsg("Syntax error or undefined variable in config path",
				PigMsgError);
	return NULL;
    }

    // Find the :'s in the string, and replace them with nulls.  Build up
    // the path_element pointer array.

    int num_paths = 0;
    char *p = path_list;

    path_element[num_paths++] = p;
    while (p = strchr(p, ':')) {
	*p++ = '\0';
	if (*p)
	    path_element[num_paths++] = p;
    }

    // Now search through the path until we find one we like

    printStaticMsg("Config file testing pathnames:", PigMsgDebug);

    for (int i=0; i < num_paths; i++) {
	strcpy(pathname, path_element[i]);
	strcat(pathname, "/");
	strcat(pathname, name);
	printStaticMsg(pathname, PigMsgDebug);

	f = fopen(pathname, "r");
	if (f != NULL) {		// found the file
	    printStaticMsg("Previous filename found", PigMsgDebug);
	    if (found_path)
		strcpy(found_path, pathname);
	    return f;
	}
    }

    return NULL;			// not found
}

////////////////////////////////////////////////////////////////////////
// Utility routine to search a string for a keyword and return the
// value associated with it.  Strings are expected to be keyword=value
// format, although just keyword (no =value) is allowed.  The returned
// pointer is *not* NULL-terminated; it is a pointer into the supplied
// string.  Use strncasecmp to look for values.  If there is no value,
// a pointer to the character past the end of the keyword is returned.
// Keywords are separated by commas and/or spaces and are not case-
// sensitive.  Spaces are allowed around the =.  However, only simple
// values are expected; quotes are not searched for.  NULL is returned
// if the keyword is not found.
////////////////////////////////////////////////////////////////////////

char *PigModelBase::parseParamString(char *param, char *keyword)
{
    if (param == NULL || keyword == NULL)
	return NULL;

    char *p = param;

    while (*p != '\0') {		// look for a keyword
	while (isspace(*p))
	    p++;
	char *kwd = p;			// parse out the potential value
	while (*p != '\0' && !isspace(*p) && *p != '=' && *p != ',')
	    p++;
	char *value = p;
	int kwd_len = (p - kwd);
	while (isspace(*p))
	    p++;
	if (*p == '=') {
	    p++;
	    while (isspace(*p))
		p++;
	    value = p;
	}
	if (kwd_len == strlen(keyword) &&
		strncasecmp(kwd, keyword, kwd_len) == 0) {	// found it!
	    return value;
	}

	while (*p != '\0' && !isspace(*p) && *p != ',')	// find end of value
	    p++;

	while (isspace(*p))		// skip to next kwd
	    p++;
	if (*p == ',')
	    p++;
    }

    return NULL;			// didn't find it
}

