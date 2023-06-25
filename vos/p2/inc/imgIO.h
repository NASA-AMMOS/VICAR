///////////////////////////////////////////////////////////////////////////////
// Image IO - Reading/Writing VICAR image
///////////////////////////////////////////////////////////////////////////////

#ifndef IMGIO_H
#define IMGIO_H

#include "SimpleImage.h" 
#include <string>


//----------------------------------------------------------------------------


// Reads and loads in a SimpleImage container a vicar image identified by its
// filename.
// All bands are loaded.
// The function will convert the vicar image data type to the output container
// type if they don't match.
// INPUT:
// - filename: path/name of the image file to read.
// OUTPUT:
// - returned value: Return 1 if successful, non-1 otherwise (RTL norm).
// - image: Pointer to a SimpleImage container declared by the user. Can't be 
//          a dereference of a statically initialized SimpleImage. The  function
//          will allocate the necessary memory if pointer is NULl or free the 
//          SimpleImage data and reallocate accordingly. The function will 
//          convert the input file type to the SimpleImage type if they differ.

template<class T>
int readImg(const char *filename, SimpleImage<T> *&image);


//----------------------------------------------------------------------------


// Convenience overload
template<class T>
int readImg(const std::string &filename, SimpleImage<T> *&image) 
{
   return readImg(filename.data(), image);  
}

//----------------------------------------------------------------------------


// Reads and loads in a SimpleImage container the Nth vicar file entered in 
// in the command line under the keyword "param". The file is loaded into a
// SimpleImage image.
// INPUT:
// - param: Name of the command line keyword to retrieve the filename from. The
//          entry could be a list of filenames from which the Nth one (via 
//          "instance" parameter) will be read. 
// - instance: Index of the file in the file list to read. Starts at 1.
// OUTPUT:
// - returned value: Return 1 if successful, non-1 otherwise (RTL norm).
// - image: Pointer to a SimpleImage container declared by the user. Can't be 
//          a dereference of a statically initialized SimpleImage. The  function
//          will allocate the necessary memory if pointer is NULl or free the 
//          SimpleImage data and reallocate accordingly. The function will 
//          convert the input file type to the SimpleImage type if they differ.
// Example:
// command line call: 
// % $R2LIB/vicarProg INP=\(file1.vic, file2.vic\) OUT=fileout.vic
// In vicarProg code: 
// SimpleImage<float> *image = nullptr;
// int status = readCmdImg("INP", image, 2);
// --> image will contain file2.vic
// LIMITATIONS:
// - Does not support list file as input, e.g.: 
// % $R2LIB/vicarProg INP=files.lis OUT=fileout.vic where files.lis is an ascii
// file containing all the image names is not supported..

template<class T>
int readCmdImg(const char *param, SimpleImage<T> *&image, const int instance);


//----------------------------------------------------------------------------


// Same as above but will read the 1st image of the list only.
template<class T>
int readCmdImg(const char *param, SimpleImage<T> *&image)
{
   return readCmdImg(param, image, 1);
}


//----------------------------------------------------------------------------


// Save a SimpleImage container to a file. All bands are saved.
// The function will save the image to file using the same data type as the
// SimpleImage if an output format is not supplied. Otherwise the function will
// convert to the supplied data type prior to writting the file to disk.
// INPUT:
// - filename: path/name of the image file to write.
// - image: SimpleImage container to save to file.
// - oformat: If NULL, no data type conversion. Otherwise will convert to a
//   format known to the RTL. Choice between: "REAL", "DOUB", "HALF", "BYTE", 
//   "FULL"
// OUTPUT:
// - returned value: Return 1 if successful, non-1 otherwise (RTL norm).

template<class T>
int saveImg(const char *filename, SimpleImage<T> *image, const char *oformat);


//----------------------------------------------------------------------------


// Convenience overload of the above without type conversion
template<class T>
int saveImg(const char *filename, SimpleImage<T> *image) 
{
   return saveImg(filename, image, NULL); 
}


//----------------------------------------------------------------------------


// Convenience overload of the above.
template<class T>
int saveImg(const std::string &filename, SimpleImage<T> *image, const std::string &oformat)
{
   return saveImg(filename.data(), image, oformat.data());
}



//----------------------------------------------------------------------------


// Convenience overload of the above.
template<class T>
int saveImg(const std::string &filename, SimpleImage<T> *image)
{
   return saveImg(filename.data(), image, NULL);
}


//----------------------------------------------------------------------------


// Save a SimpleImage container to a file whose name is the Nth filename entered
// in the command line under the keyword "param". 
// INPUT:
// - param: Name of the command line keyword to retrieve the filename from. The
//          entry could be a list of filenames from which the Nth one (via 
//          "instance" parameter) will selected for output. 
// - image: SimpleImage container to save to file.
// - oformat: If NULL, no data type conversion. Otherwise will convert to a
//   format known to the RTL. Choice between: "REAL", "DOUB", "HALF", "BYTE", 
//   "FULL"
// - instance: Index of the file in the file list to read. Starts at 1.
// OUTPUT:
// - returned value: Return 1 if successful, non-1 otherwise (RTL norm).
// Example:
// command line call: 
// % $R2LIB/vicarProg INP=\(file1.vic, file2.vic\) OUT=fileout.vic
// In vicarProg code: 
// int status = saveCmdImg("OUT", image, "DOUB");
// --> image container will be saved to fileout.vic as a double image file

template<class T>
int saveCmdImg(const char *param, SimpleImage<T> *image, const char *oformat, const int instance);


//----------------------------------------------------------------------------


// Convenience overload of the above
template<class T>
int saveCmdImg(const char *param, SimpleImage<T> *image, const char *oformat)
{
   return saveCmdImg(param, image, oformat, 1);
}


//----------------------------------------------------------------------------


// Convenience overload of the above
template<class T>
int saveCmdImg(const char *param, SimpleImage<T> *image) 
{
   return saveCmdImg(param, image, NULL, 1);
}





#endif

