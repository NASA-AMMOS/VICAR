/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020EXCEPTION_H_
#define M2020EXCEPTION_H_

#include <exception>
#include <string>
using namespace std;

class M2020Exception:public exception {
      private:
   string msg_, userMsg_;
   int errorCode_;
   void msg(int errCode, const exception * e, const char *msg, const char *fn, int ln) throw();
      public:
   static const int SUCCESS, SUCCESS_NO_OUTPUT, FAILURE, DICTIONARY_ERROR, DB_ERROR, NAIF_ERROR, LABEL_ERROR,
       PARAMETER_ERROR, IO_ERROR, DECOMPRESSOR_ERROR, UNKNOWN_ERROR, APXS_START_LATE;

   explicit M2020Exception(int errCode, const char *msg, const char *fileName, int lineNo) throw();

   explicit M2020Exception(int errCode, const exception & e, const char *fileName, int lineNo) throw();

   explicit M2020Exception(int errCode, const exception & e,
            const char *msg, const char *fileName, int lineNo) throw();

    M2020Exception & operator=(const exception & e) throw();

    virtual ~ M2020Exception() throw();

   virtual const char *what() const throw();

   virtual int getErrorCode() const throw() {
      return this->errorCode_;
   }
   virtual const char *getUserMsg() const throw() {
      return this->userMsg_.c_str();
   }
};

#define THROW_EXCEPTION(c,msg)     throw M2020Exception(c,  msg,__MSG_SOURCE__)
#define THROW_EXCEPTION_E(c,e,msg) throw M2020Exception(c,e,msg,__MSG_SOURCE__)
#endif            /*M2020EXCEPTION_H_ */
