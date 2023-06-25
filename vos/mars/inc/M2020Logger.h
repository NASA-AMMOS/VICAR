/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020LOGGER_H_
#define M2020LOGGER_H_

#include "M2020Exception.h"
#include "M2020UPF.h"

#include <stdarg.h>
#include <stdio.h>

#include <fstream>
using namespace std;

enum LOG_LEVEL {
   TRACE_LEVEL = 0,
   DEBUG_LEVEL,
   INFO_LEVEL,
   WARNING_LEVEL,
   ERROR_LEVEL,
   FATAL_LEVEL
};
static const char *logLevel[] = { "TRACE", "DEBUG", "INFO", "WARNING", "ERROR", "FATAL" };

class M2020Logger:public ofstream {
      private:
   //ofstream lfile_;
   LOG_LEVEL level_;

      public:
   explicit M2020Logger();
   explicit M2020Logger(const M2020UPF & upf) throw(exception);
   explicit M2020Logger(const char *fileName, bool append, LOG_LEVEL level = INFO_LEVEL) throw(exception);
    virtual ~ M2020Logger();

   static void formatTraceMsg(string & formatted_msg, unsigned int depth, const char *fname, int lineNo);
   void setLevel(LOG_LEVEL level) {
      this->level_ = level;
   } LOG_LEVEL getLevel() const {
      return this->level_;
   }
   void reset(const M2020UPF & upf) throw(exception);
   void reset(const char *fileName, bool append, LOG_LEVEL level = INFO_LEVEL) throw(exception);

   void debug(const char *msg, M2020Exception * e = NULL) throw(exception);
   void info(const char *msg, M2020Exception * e = NULL) throw(exception);
   void warn(const char *msg, M2020Exception * e = NULL) throw(exception);
   void error(const char *msg, M2020Exception * e = NULL) throw(exception);
   void fatal(const char *msg, M2020Exception * e = NULL) throw(exception);
   void trace(unsigned int depth, const char *fileName, int lineNo) throw(exception);

   void log(const char *msg, M2020Exception * e = NULL) throw(exception);
   void log(LOG_LEVEL level, const char *msg, M2020Exception * e = NULL) throw(exception);

    /**
     * Returns the LOG_LEVEL represented by the string level or INFO.
     */
   static LOG_LEVEL getLevel(const string & level);
   static string getLevel(LOG_LEVEL level);

};
#endif
