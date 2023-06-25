#include <stdio.h>
#include <string.h>
#include <tcl.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"
#include "zifmessage.h"

#include "cartoTaeUtils.h"

void main44(void)
{
  int parmdf, scriptLineCount;
  char script [100][250];
  char * wholeScript = 0;
  int wholeScriptSize, i;
  Tcl_Interp * interp;

  zifmessage("tcl2tcl version Thu Jan  3 2008");
   
  zvparm ("script", script, &scriptLineCount, &parmdf, 100, 250);
   
  wholeScriptSize = 1; /* null */
  for ( i = 0; i < scriptLineCount; i ++ )
    wholeScriptSize += strlen( script[ i ] );

  wholeScript = malloc( wholeScriptSize );

  wholeScript[0] = '\0';

  for ( i = 0; i < scriptLineCount; i ++ )
    strcat( wholeScript, script[ i ] );

  interp = Tcl_CreateInterp ();
  Tcl_Init(interp);
  if (Tcl_Eval (interp, wholeScript) != TCL_OK) {
    char msg[1000];
    snprintf (msg, 1000, "TCL call failed: %s\n", Tcl_GetStringResult (interp));
    zifmessage(msg);
    mq_out_string ("strvar", (char*) Tcl_GetStringResult (interp), 99);
    mq_out_int ("intvar", 0);
    mq_out_real ("realvar", 0.0);
  } else {
    char * result = strdup (Tcl_GetStringResult (interp));
    int intval;
    double realval;
    mq_out_string ("strvar", result, 99);

    if (Tcl_GetInt (interp, result, & intval) == TCL_OK)
      mq_out_int ("intvar", intval);
    else
      mq_out_int ("intvar", 0);

    if (Tcl_GetDouble (interp, result, & realval) == TCL_OK)
      mq_out_real ("realvar", realval);
    else
      mq_out_real ("realvar", 0.0);

    /* not freeing result */
    /* not deleting interp */
  }
}
