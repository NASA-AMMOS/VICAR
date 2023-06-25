#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"


static void updateParm( char * property, int geounit, char * parm ) {
  char * value;

  value = strchr( parm, '=' );
  if ( ! value ) {
    char buf[ 100 ];
    sprintf( buf, "parm \"%s\" missing '='", parm );
    zifmessage( buf );
  } else {
    *value = 0; /* null the key */
    value ++; /* skip '=' */

    zldel( geounit, "PROPERTY", parm, "PROPERTY", property, "ERR_ACT", " ", NULL);
    zladd( geounit, "PROPERTY", parm, value, "PROPERTY", property, "FORMAT", "STRING", "ERR_ACT", " ", NULL);
  }
}

static void freeKeyValuePairs( int count, char *** keyValuePairs ) {
  int i;

  for (i = 0; i < count; i ++) {
    free( (*keyValuePairs)[ i ] );
  }
  free( *keyValuePairs );
  *keyValuePairs = 0;
}

static void getPropertyLabel(char * property, int unitIndex, char * infilename, int * count, char*** keyValuePairs ) {
  char key[ 33 ], format[ 9 ];
  int parmunit, maxlength, nelement, foundProperty;
  char * buf = 0;

  * count = 0;
  * keyValuePairs = 0;

  if ( zvunit( &parmunit, "INP", unitIndex, NULL) != 1 )
    zmabend ("gtgenup: error in zvunit for inp parm");
  if ( zvopen( parmunit, "OP", "READ", "OPEN_ACT", "SA", "LAB_ACT", " ", NULL) != 1 )
    zmabend ("gtgenup: error in zvopen for inp parm");

  foundProperty = 0;
  while ( zlninfo( parmunit, key, format, & maxlength, & nelement, NULL) == 1 ) {
    if ( ! strcmp( key, "PROPERTY" ) ) {
      buf = (char*) realloc( buf, maxlength + 1 );
      zlget( parmunit, "PROPERTY", key, buf,
	     "ERR_ACT", "SA",
	     "FORMAT", "STRING",
	     "NELEMENT", 1,
	     "PROPERTY", property, NULL);
      if ( ! strcmp( buf, property ) )
	foundProperty = 1;
	break;
    }
  }

  if ( foundProperty ) {
    while ( zlninfo( parmunit, key, format, & maxlength, & nelement, NULL) == 1 ) {
      if ( ! strcmp( key, "PROPERTY" ) || ! strcmp( key, "TASK" ) )
	break;

      if ( strcmp( format, "STRING" ) ) {
	char msg[200];
	sprintf( msg, "property %s in file %s is not FORMAT STRING", key, infilename );
	zmabend( msg );
      }

      buf = (char*) realloc( buf, maxlength + 1 );

      zlget( parmunit, "PROPERTY", key, buf,
	     "ERR_ACT", "SA",
	     "FORMAT", "STRING",
	     "NELEMENT", 1,
	     "PROPERTY", property, NULL);

      ++ (* count);
      * keyValuePairs = (char**) realloc( * keyValuePairs, * count * sizeof( char* ) );
      (* keyValuePairs)[ * count - 1] = (char*) malloc( strlen( key ) + strlen( buf ) + 2 ); /* '=' plus null */
      
      sprintf( (* keyValuePairs)[ * count - 1], "%s=%s", key, buf );
    }

  }

  free( buf );

  zvclose( parmunit, NULL);
}

void main44(void)
{
  char infilename[2][100], parmfile[100], gtparms[100][200], property[100];
  int geounit, parmct, inpct, parmdf, i, gtcount, gtParmFileDef, geotiffDef;

  zifmessage( "gtgenup version 2015-10-15" );

  zvparm( "inp", infilename, &inpct, &parmdf, 2, 99 );
  zvparm( "parmfile", parmfile, &parmct, &gtParmFileDef, 1, 99 );
  zvparm( "geotiff", gtparms, &gtcount, &geotiffDef, 40, 200 );
  zvparm( "property", property, &parmct, &parmdf, 1, 99 );

  if ( gtParmFileDef + geotiffDef + (inpct == 1) != 2)
    zmabend( "gtgenup: exactly one of the second inp parm, the parmfile parm, or the geotiff parm must be specified" );
   
  if (zvunit( &geounit, "INP", 1, NULL) != 1)
    zmabend ("gtgenup: error in zvunit for first inp parm");
  if (zvopen( geounit, "OP", "UPDATE", "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL) != 1)
    zmabend ("gtgenup: error in zvopen for first inp parm");

  if (inpct == 2) {
    int gtLabelCount, i;
    char ** keyValuePairs = 0;

    /* get GeoTIFF label from update source (second inp image) */
    getPropertyLabel( property, 2, infilename[ 1 ], & gtLabelCount, & keyValuePairs );

    for (i = 0; i < gtLabelCount; i ++)
      updateParm( property, geounit, keyValuePairs[ i ] );

    freeKeyValuePairs( gtLabelCount, & keyValuePairs );

  } else if ( gtParmFileDef )
    for ( i = 0; i < gtcount; i++ )
      updateParm( property, geounit, gtparms[ i ] );

  else {
    FILE * f = fopen ( parmfile, "r" );
    char buf[ 200 ];

    if ( ! f )
      zmabend( "gtgenup: error opening parmfile for input" );

    while (! feof( f ) ) {
      if ( fgets( buf, 199, f ) && strlen ( buf ) ) {
	buf[ strlen( buf ) - 1] = 0;
	updateParm( property, geounit, buf );
      }
    }
  }

  zvclose( geounit, NULL);
}
