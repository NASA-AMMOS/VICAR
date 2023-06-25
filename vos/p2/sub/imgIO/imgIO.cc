
#include <type_traits>
#include "defines.h"
#include "imgIO.h"


//----------------------------------------------------------------------------


template<class T>
int readCmdImg(const char *param, SimpleImage<T> *&image, const int instance) {

   int length = MAX_FILE_NAME_SIZE;
   char filename[length];

   int status = zvpone(const_cast<char*>(param), filename, instance, length);
   if (status != 1)
      return status;

   return readImg(filename, image);

}


//----------------------------------------------------------------------------


template<class T>
int readImg(const char *filename, SimpleImage<T> *&image) {

   int unit;

   int status = zvunit(&unit, const_cast<char*>(filename), 1, "u_name", filename, NULL);
   if (status != 1)
      return status;
  
   status = zvopen(unit, "U_FORMAT", image->getVicarTypeString(), "OPEN_ACT", "S", NULL);
   if (status != 1)
      return status;

   // Get input image dimensions
   int nl, ns, nb;
   zvget(unit, "NL", &nl, "NS", &ns, "NB", &nb, NULL);

   // Clean output image if busy
   if (image != nullptr) {
      image->free();
      image->alloc(nb, nl, ns);
   }
   else 
      image = new SimpleImage<T>(nb, nl, ns);

   // Read input into memory
   for (int k=0; k < nb; k++) {
      for (int j=0; j < nl; j++) {
         zvread(unit, image->linePtr(k,j), "BAND", k+1, "LINE", j+1, NULL);
      }
   }

   return zvclose(unit, "CLOS_ACT", "FREE", NULL);

}


//----------------------------------------------------------------------------


template<class T>
int saveCmdImg(const char *param, SimpleImage<T> *image, const char *oformat, const int instance) {

   int length = MAX_FILE_NAME_SIZE;
   char filename[length];

   int status = zvpone(const_cast<char*>(param), filename, instance, length);
   if (status != 1)
      return status;

   return saveImg(filename, image, oformat);

}


//----------------------------------------------------------------------------


template<class T>
int saveImg(const char *filename, SimpleImage<T> *image, const char *oformat) {

   if (image == nullptr)
      return 0;

   int unit;
   int status = zvunit(&unit, "", 1, "u_name", filename, NULL);
   if (status != 1)
      return status;

   status = zvopen(unit, "OP", "WRITE", "U_FORMAT", image->getVicarTypeString(), 
                   "U_NS", image->getNS(), "U_NL", image->getNL(), "U_NB", 
                   image->getNB(), "U_ORG", "BSQ", "OPEN_ACT", "S", NULL);
   if (status != 1)
      return status;

   if (oformat != NULL) {
      status = zvadd(unit, "O_FORMAT", oformat, NULL);
      if (status != 1)
         return status;
   }


   zvplabel(unit, 0, 1);

   for (int i=0; i<image->getNB(); i++) {
      for (int j=0; j<image->getNL(); j++) {
         zvwrit(unit, image->linePtr(i,j), "LINE", j+1, "BAND", i+1,NULL);
      }
   }

   return zvclose(unit, "CLOS_ACT", "FREE", NULL);
}



//----------------------------------------------------------------------------


// Macro to instantiate templates
#define INSTANTIATE_READWRITE(T) \
template int readImg<T>(const std::string &filename, SimpleImage<T> *&image); \
template int readCmdImg<T>(const char* param, SimpleImage<T> *&image, const int instance); \
template int saveImg<T>(const char *param, SimpleImage<T> *image, const char *oformat); \
template int saveCmdImg<T>(const char *param, SimpleImage<T> *image, const char *oformat,  const int instance);


// Template Instantiation
// To be extented as needed
INSTANTIATE_READWRITE(float)
INSTANTIATE_READWRITE(double)
INSTANTIATE_READWRITE(int)
INSTANTIATE_READWRITE(char)


