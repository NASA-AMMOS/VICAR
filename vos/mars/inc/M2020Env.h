/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 * @author Mauricio Hess-Flores {mauricio.a.hess.flores@jpl.nasa.gov}
 */
#ifndef M2020ENV_H_
#define M2020ENV_H_

#ifndef MAXSTRLEN
#define MAXSTRLEN       256
#endif

#include <iostream>
using namespace std;
#include "M2020MiscUtil.h"
#include "M2020UPF.h"
#include "M2020Logger.h"
#include "M2020Tracer.h"
#include "M2020Db.h"
#define __MSG_SOURCE__ __FILE__,__LINE__
#define TRACE M2020Tracer  __M2020_TRACER__ (__MSG_SOURCE__)

class M2020Env {

      private:
   int initialize();
   M2020UPF upf_;
   M2020Logger logger_;
   string spiceFname_;
   string spiceFid_;
   static M2020Env *instance_;
   static bool isUsable_;
   bool isOps_;



    M2020Env();

      public:
    bool useDb_;
    M2020Db *m20Db_;
   static M2020Env *instance() throw(exception);
   static bool isUsable() {
      return M2020Env::isUsable_;
   }
   ~M2020Env();

   void reset(const string & upfFileName) throw(exception);

   M2020UPF & getM2020UPF() {
      return this->upf_;
   }
   M2020Logger & getM2020Logger() {
      return this->logger_;
   }

   const string & furnishFile() const {
      return this->spiceFname_;
   }
   const string & spiceFileId() const {
      return this->spiceFid_;
   }
   void furnishFile(string & ffile) {
      this->spiceFname_ = ffile;
   }

   bool isOps() const {
      TRACE;
      return this->isOps_;
   }
   bool useSpice() const {
      return !this->spiceFname_.empty();
   }
   const char *getVersion() const {
      return "V7.7 2-23-2023";
   }
};
#endif            /*M2020ENV_H_ */
