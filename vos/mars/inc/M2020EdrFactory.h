/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/

/**
 * @author Alice Stanboli {Alice.Stanboli@jpl.nasa.gov}
 */
#ifndef M2020EDRFACTORY_H_
#define M2020EDRFACTORY_H_
#include "M2020Env.h"
#include "M2020Edr.h"

class M2020EdrFactory {
      public:
   static M2020Edr *createEdr(M2020DpMetaData *) throw(exception);
};

#endif            /*M2020EDRFACTORY_H_ */
