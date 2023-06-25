#ifndef M2020TRACER_H_
#define M2020TRACER_H_
/*
       Copyright 2008-Present, California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledge.
*/
#include<iostream>
using namespace std;

/**
 * Class to trace method entrance and exit, along with time.
 * @author Alice Stanboli {Alice.Stanboli@jp.nasa.gov}
 */
class M2020Tracer {
      private:
   string fname_;
   //time_t startTime_;
   int lineNo_;
   static unsigned int depth_;
      public:
    explicit M2020Tracer(const char *fileName, int lineNo);
    virtual ~ M2020Tracer();
};

#endif            /*M2020TRACER_H_ */
