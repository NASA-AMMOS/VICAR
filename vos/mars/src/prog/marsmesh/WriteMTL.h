// WriteMTL.h

#ifndef WriteMTL_H
#define WriteMTL_H

#include <fstream>
#include <iostream>
#include <string>

#include "lbl_identification.h"

class WriteMTL {
public:
  WriteMTL(std::string filename_mtl);
  ~WriteMTL();
  void write_strr(void);
  void write_str2(void);
private:
    std::string filename_mtl;
    std::ofstream file_mtl;
    const LblIdentification_typ *lbl_identification;
};

#endif
