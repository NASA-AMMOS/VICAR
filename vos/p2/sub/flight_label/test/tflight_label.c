#include <string.h> 
#include "vicmain_c"
#include "zifmessage.h"
#include "flight_label.h"

struct  node { char * str;  /* LINK list for output buffer */
  struct node * next;
};
typedef struct node NODE;
void main44()
{NODE * list;
  int   count, unit[10], x, y;
  char  inputs[800];
  char  outbuf[100];

  zifmessage("FLIGHT_LABEL test version 2019-08-01");

  zvp("INP",inputs, &count);
  for( x=0,y=1; x<count; x++,y++ ) /* Open files before calling flight_label*/
    {
      zvunit(&unit[x],"INP", y, NULL);
      zvopen(unit[x],"OP","READ","OPEN_ACT","SA", NULL);
      list =(NODE*) flight_label(unit[x]);        /* Display flight label  */
      zvmessage("",""); 
      zvmessage("PRINTING FROM OUTPUT BUFFER"," ");
      zvmessage("","");
      if (list == NULL)  zvmessage("Nothing to print.  Buffer is empty.","");
      else  while (list != NULL){
	  strcpy (outbuf, list->str);
	  zvmessage(outbuf,"");
	  list = list->next;
	}
     
      zvclose(unit[x], NULL);
      zvmessage("FINISHED", "");
      zvmessage("", "");
    }
}

