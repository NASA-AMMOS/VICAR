/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020UPF_H_
#define M2020UPF_H_

#include <iostream>
#include <fstream>
#include <map>
using namespace std;

#define MAX_M2020UPF_LINE_SIZE 2056

class M2020UPF {
      private:
   string upfName_, strM2020UPF_;
   map < string, string > upf_;
   char delimiterChar_, commentChar_;

   void createStrUPF(string & str);

      public:
    explicit M2020UPF() throw(exception);
    virtual ~ M2020UPF();

    /**
     * Clear the content of this M2020UPF object and the upf file name.
     */
   virtual void clear() {
      this->upf_.clear();
      this->strM2020UPF_.clear();
      this->upfName_.clear();
   };

    /**
     */
   const char *getM2020UPFFileName() {
      return this->upfName_.c_str();
   }

    /**
     * Parse the given M2020UPF file and add keywords and values to existing set.
     * The key value separator is '=' and comment starts with '#'
     */
   void parse(const string & upfName) throw(exception);

   /** Parse the given M2020UPF file and add keywords and values to existing set.
    *  Use supplied delimiter as key value separator. Comments start with '#'
    */
   void parser(const string & upfName, char delimiter) throw(exception);

   /** Parse the given M2020UPF file and add keywords and values to existing set.
    *  Use supplied delimiter as key value separator and commentStartChar to mark
    *  start of a comment.
    */
   void parse(const string & upfName, char delimiter, char commentStartChar) throw(exception);

    /**
     * M2020UPF as it was read returned as string
     */
   const string & upfStr() const {
      return this->strM2020UPF_;
   }
    /**
     * Value associated with the given key or NULL if key is not specified in M2020UPF
     */ const char *getValue(const char *key) const;

    /**
     * Value associated with the given key or NULL is missing
     */
   const char *getValue(const string & key) const;

    /**
     * get value in value parameter. Return true if key is specified, false otherwise
     */
   bool getValue(const string & key, char *value) const;

   bool getValue(const string & key, string & value) const;

    /**
     * on_definition means if the key is specified and its value is equal to on_definition then the value of bool is set to true.
     */
   bool getValue(const string & key, bool & value, const string & on_definition) const;

    /**
     * Return a reference to the underlaying map object
     */
   const map < string, string > &getAll() const {
      return this->upf_;
   }
    /**
     * Add or replace given variable.
     */ void addOrReplace(const string & key, const string & value);

    /**
     * Writes the UPF. The call may generate exactly same UPF if
     * no changes are made to the content via this interface.
     */
   void write() throw(exception);
   void writeToDisk() throw(exception);
   void writeToDisk(const char *upfFileName) throw(exception);

};
#endif
