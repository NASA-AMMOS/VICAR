// preferences.h
// This is a set of prototypes for functions to access a
// preferences file and return settings.  Preferences are
// of the form Name = Value\n with comments denoted by #
// in column 1.  An optional filename may direct access to
// a particular file with the default being ~/.vsprefs.
// John Wright	12/22/97

#ifndef _PREFERENCES_H_
#define _PREFERENCES_H_

class Preferences {
private:
	char *filename;
public:

// get a preference value from the preferences file
// value is returned in a malloced string which may be freed by the caller
// returns NULL if named value is not available
char *get_preference(const char *fieldname);

// store a preference value in the preferences file
// returns -1 for any error (unable to open or write file
// returns 0 if operation successful
int set_preference(const char *fieldname, const char *string_value);

// set the preferences filename. if path == 0 then use the HOME variable
// if it does not exist, then create it. returns 0 if file exists, 1 if file is
// created, or -1 on error
int set_file(char *name, char *path = 0);

// set the preferences filename. if path == 0 then use the HOME variable
Preferences(char *name, char *path=0) {
	filename = NULL;
	set_file(name, path);
}

Preferences() { filename = NULL; }
~Preferences() { if(filename) free(filename); }

};

#endif
