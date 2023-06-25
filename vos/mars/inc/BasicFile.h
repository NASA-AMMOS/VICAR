//////////////////////////////////////////////////////////////////////////////
//	BasicFile.h
//
// This class provide basic interface to files. I am not sure where this
//	would be used, but for now we will use it for the log & query
//	files needed by the Ds1ConnectorSock, Ds1PktProcessor and
//	Ds1TelemFileStore.
//
// These Ds1 class will not use the exact class, but they will use class
//	derived from this.
//
//////////////////////////////////////////////////////////////////////////////
#ifndef BASIC_FILE_H
#define BASIC_FILE_H

#define INVALID_FILE		0
#define FILE_IS_READABLE	1
#define FILE_IS_WRITEABLE	2
#define FILE_IS_READ_WRITEABLE	3

#ifndef MAXSTRLEN
#define MAXSTRLEN	256
#endif

#include <fstream>
using namespace std;

class BasicFile {
 protected:
	fstream _fd;
	char 	*_fname,
		_isOpened;

	BasicFile ();
	BasicFile (const char*);	
	// Default constructor and init constructor. You can give the
	// the constructor the file name or set the file name later
	// via this->set (char*);
	//
	// this->set (const char*) will fill in the struct stat _info for you
	// 	so the only time you actually need to get file information is
	//	this->set (). All other information you need from this file
	//	should come from this _info attribute.
	//
	// Notice that the constructor will take care of all internal
	// parameters. You can only set the file name !!!!

 public:

	~BasicFile ();	

	int openRead ();
	int openWrite ();
	int openAppend ();
	// Member function to open
	// and close current file.
	// Here are the policies for opening files:
	// openRead (): try to open the given file name as both
	//	r/w. If failed ==> no permission or non-existence file
	//	Then try to open read only. If failed ==> non-existence
	//	Then create a new one.
	// openWrite (): open for both r/w, if failed open to create.
	// openAppend (): open for append, if failed open to create

	int appendn (char *ptr, u_int len, int doCleanUp = 1);
	int readn (char *ptr, u_int pos, u_int len, int doCleanUp = 1);
	int writen (char *ptr, u_int pos, u_int len, int doCleanUp = 1);
	// member functions which allows caller to read/write data from
	//	current file. The len of 'ptr' must be as least that of
	//	'len' and 'pos' is where you want to read/write from.
	//	'doCleanUp' indicate if the file should be closed once
	//	the read/write operation is done. By default, the file
	//	will be closed when the read/write is over. But for
	//	tasks that do repeated read/write operations, you might
	//	want to leave the file open until the last read/write
	//	request
	// return the number of bytes read/write.

	int closeFile ();
	// Member function to close up current file (if it's opened)
	// Only close and re-set _fileptr attribute, and does not
	// touch anything else.

	virtual int set (const char*);
	// Notice that if set failed, it will clean out every
	// attribute you have in this object except _errMesg.
	//
	// The only reason this failed is when you have given it an
	// invalid file name ===> stat() failed !!!
	//
	// If file is OK, the set _info, _filename, but leave
	//	other attributes alone !!!

	inline int isOpened () { return _isOpened; };
	// Member function to check if the file is/isn't opened

	int fileLen();
	// return length of current file
};

#endif
