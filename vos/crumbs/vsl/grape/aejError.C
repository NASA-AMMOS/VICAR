// aejError.C 1.3 02/07/10 15:02:49
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@ri.cmu.edu)
 + DATE:    4-Feb-98
 + PURPOSE: aej error functions 
 **/

#include "grape/aej.h"
#include "grape/aejError.h"

/// Prints error message and exits
void HandleError(const char *message) {

  fprintf(stderr,"\n%s\n",message);
  exit(1);
  
}

/// Prints function name and start
void AnnounceFunctionStart(const char *name) {

  fprintf(stdout,"\n%s <<START>>\n",name);
  fflush(stdout);
  
}

/// Prints function name and end
void AnnounceFunctionEnd(const char *name) {

  fprintf(stdout,"\n%s <<END>>\n",name);
  fflush(stdout);
  
}

/// Prints command line arguments
void PrintCommandLine(int argc, char **argv) {
 
  int i;
  fprintf(stdout,"\nCOMMAND LINE: ");
  for(i=0;i<argc;i++) fprintf(stdout,"%s ",argv[i]);
  fprintf(stdout,"\n\n");
  fflush(stdout);

}

/// Returns a 2 character index for an integer
char* Index2Char(int i, char* index) {
  
  if ((i<0)||(i>99)) {
    cerr << "Index: " << i << endl;
    HandleError("Index2Char: index out of range");
  }
  if (i<10) sprintf(index,"0%d",i);
  else sprintf(index,"%d",i);
		
  return index;
  
}

/// Returns a 3 character index for an integer
char* Index3Char(int i,char* index) {
  
  if ((i<0)||(i>999)) {
    cerr << "Index: " << i << endl;
    HandleError("Index3Char: index out of range");
  }
  if (i<10) sprintf(index,"00%d",i);
  else {
    if (i<99) sprintf(index,"0%d",i);
    else sprintf(index,"%d",i);
  }
  
  return index;
  
}
