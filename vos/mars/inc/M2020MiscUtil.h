/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/
/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020MISCUTIL_H_
#define M2020MISCUTIL_H_

#include "M2020MainInclude.h"

class M2020MiscUtil {
      public:
  /** Remove leading and trailing spaces*/
   static void trim(string &);
  /** Remove leading spaces */
   static void ltrim(string &);
  /** Remove trailing spaces */
   static void ttrim(string &);
  /** Check endiness of the machine */
   static bool bigEnd();
  /** set the bit specified by bitNum to 1 in the val*/
   static void setBit(unsigned int &val, int bitNum);
  /** convert given unsigned int to a binary string returned in the given string*/
   static void int2binstr(unsigned int val, string & strval);
  /** revert the bits in given val*/
   static unsigned int reverseBits(unsigned int val);

  /** Substitue, in the given string, any occurances of ${VAR}
      the value of VAR. */
   static void expandEnvs(string & str) throw(exception);
};

#endif            /*M2020MISCUTIL_H_ */
