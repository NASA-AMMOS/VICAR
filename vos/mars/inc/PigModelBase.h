////////////////////////////////////////////////////////////////////////
// PigModelBase
//
// Base class for all Pig models.  The base class provides the following
// services:
//   * Message output
//   * Obtaining user parameters
//   * Defining the default coordinate system (not yet implemented)
////////////////////////////////////////////////////////////////////////
#ifndef PIGMODELBASE_H
#define PIGMODELBASE_H

#include <stdio.h>		/* for NULL */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

// RTL library has fundamental string limit of 250 characters.
// Also the same constant is defined in mars_support.h.  Thus
// if we ever should change value here, we also need to change
// it in mars_support.h
#ifndef PIG_MAX_FILENAME_SIZE
#define PIG_MAX_FILENAME_SIZE 250
#endif

#ifndef PIG_MAX_UNIQUE_MSGS
#define PIG_MAX_UNIQUE_MSGS 1000
#endif

// Severity codes for messages

typedef enum {PigMsgDebug, PigMsgInfo, PigMsgWarning, PigMsgError, PigMsgFatal}
					PigMsgSeverity;

// Functions matching this prototype may be substituted for the message
// printer using setMsgFunction().  Subclasses must call printMsg();
// do not call this function directly!!  A newline should be added at the
// end of the message (i.e. the msg string has none).

typedef void (*PigMsgFunction)(char *msg, PigMsgSeverity severity,
				void *clientData);

// Functions matching this prototype may be substituted for the parameter
// processor using setParamFunction().  Subclasses must call getParam();
// do not call this function directly!!  See getParam() for the general
// contract; it is the same.

typedef void (*PigParamFunction)(char *name, void *value, int *count,
		int maxcnt, int length, void *clientData);

class PigModelBase {

  protected:

    /******************************************************************/
    /* MESSAGES							      */
    /******************************************************************/

    static PigMsgFunction _defaultMsgFunction;
    static void *_defaultMsgClientData;

    PigMsgFunction _msgFunction;
    void *_msgClientData;

    static char *unique_msgs[PIG_MAX_UNIQUE_MSGS];
    static int unique_msg_count;
    static int unique_msg_printed;

    // These functions must be called for printing ALL messages within
    // the Pig models.  DO NOT USE zvmessage() or printf() directly.
    // The key, if any, is put in [] before the message, a la zvmessage().
    // The msg string should not end in a newline (it is automatically added).

    void printMsg(char *msg, PigMsgSeverity severity) const;
    void printMsg(char *msg, char *key, PigMsgSeverity severity) const;

    // These are convenience functions for the above
    void printDebug(char *msg) const   { printMsg(msg, PigMsgDebug); }
    void printInfo(char *msg) const    { printMsg(msg, PigMsgInfo); }
    void printWarning(char *msg) const { printMsg(msg, PigMsgWarning); }
    void printError(char *msg) const   { printMsg(msg, PigMsgError); }
    void printFatal(char *msg) const   { printMsg(msg, PigMsgFatal); }

    /******************************************************************/
    /* PARAMETERS						      */
    /******************************************************************/

    static PigParamFunction _defaultParamFunction;
    static void *_defaultParamClientData;

    PigParamFunction _paramFunction;
    void *_paramClientData;

    // This function must be called for obtaining ALL user parameters
    // within the Pig models.  DO NOT USE zvp() et al directly.
    // Note that the data type returned is implicit in the name, a la
    // zvp and friends (which examine the PDF for the data type).
    // Be careful about this.  Also, float types are never returned,
    // only double (as in, the default implementation uses zvparmd()).
    // "count" (the returned # of params) may be NULL.  If maxcnt is 0,
    // there is no specific maximum.  "length" is the size of each string
    // for a string array (2-D array of chars) and may be 0 for a single
    // string or a non-string value.

    void getParam(char *name, void *value, int *count, int maxcnt,
							int length) const;

    // These are convenience functions for the above
    void getOneParam(char *name, void *value) const
	{ getParam(name, value, NULL, 1, 0); }

    ////////////////////////////////////////////////////////////////////

  public:

    // Default constructor/destructor.

    PigModelBase();
    virtual ~PigModelBase();

    /******************************************************************/
    /* MESSAGES							      */
    /******************************************************************/

    // STATIC function that sets the default message printer for ALL
    // models that aren't specifically overridden.

    static void setDefaultMsgFunction(PigMsgFunction func, void *clientData);

    // Sets the message printer for THIS class only.  If this function is
    // not called, or the function is set to NULL, then the default is used.
    // Note that changing the default *will* change the function used in this
    // case (i.e. the default is checked on each call).

    void setMsgFunction(PigMsgFunction func, void *clientData);

    // STATIC functions to be used ONLY in factory classes that create
    // models, when a message needs to be printed but no model is available.
    // Uses the default message function.  There are no convenience functions.
    // The msg string should not end in a newline (it is automatically added).

    static void printStaticMsg(char *msg, PigMsgSeverity severity);
    static void printStaticMsg(char *msg, char *key, PigMsgSeverity severity);

    // Same as above, but suppresses non-unique messages (e.g. the endless
    // loading camera model messages from mosaic programs).

    static void printUniqueStaticMsg(char *msg, PigMsgSeverity severity);

    /******************************************************************/
    /* PARAMETERS						      */
    /******************************************************************/

    // STATIC function that sets the default parameter processor for ALL
    // models that aren't specifically overridden.

    static void setDefaultParamFunction(PigParamFunction func,
							void *clientData);

    // Sets the parameter processor for THIS class only.  If this function is
    // not called, or the function is set to NULL, then the default is used.
    // Note that changing the default *will* change the function used in this
    // case (i.e. the default is checked on each call).

    void setParamFunction(PigParamFunction func, void *clientData);

    // STATIC function to be used ONLY in factory classes that create
    // models, when a parameter needs to be obtained but no model is available.
    // Uses the default parameter function.  There are no convenience functions.

    static void getStaticParam(char *name, void *value, int *count, int maxcnt,
								int length);

    /******************************************************************/
    /* CONFIG FILES						      */
    /******************************************************************/

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

    static FILE *openConfigFile(char *name, char *found_path);

    /******************************************************************/
    /* UTILITY ROUTINES						      */
    /******************************************************************/

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

    static char *parseParamString(char *param, char *keyword);

    ////////////////////////////////////////////////////////////////////
    virtual const char *const getModelName() { return "PigModelBase"; }

};

// This is the ultimate default print function

void PigMsgPrinter(char *msg, PigMsgSeverity severity, void *clientData);

// This is the ultimate default parameter processor

void PigParamGetter(char *name, void *value, int *count, int maxcnt, int length,
							void *clientData);

// This is an alternate parameter processor, for potential use by interactive
// or Real-Time programs

void PigSimpleParamGetter(char *name, void *value, int *count, int maxcnt,
						int length, void *clientData);

#endif

