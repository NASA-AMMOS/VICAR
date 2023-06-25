// WriteMTL

#include "WriteMTL.h"

  WriteMTL::WriteMTL(std::string filename_mtl) {
        std::cout << filename_mtl << std::endl;
        file_mtl.open(filename_mtl);
        if (!file_mtl.is_open()) {
            std::cerr <<"Could not open file for writing." << std::endl;
        }
        //else {
         //   std::cout << "write to file" << std::endl;
       	  //  file_mtl << "filename_mtl" << std::endl;
        //}
  }
  WriteMTL::~WriteMTL() {
      std::cout << "Destructor called." << std::endl;
      //if (file_mtl)
       //   file_mtl.close();
  }
  void WriteMTL::write_strr(void) {
      if (file_mtl.is_open())
          file_mtl << "this is a test" << std::endl;
  }

  void WriteMTL::write_str2(void) {
      if (file_mtl.is_open())
          file_mtl << "this is a test2" << std::endl;
  }
  
