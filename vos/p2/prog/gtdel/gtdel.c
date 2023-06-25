#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

/* copied from gtgenup.c */
static void freeKeyValuePairs( int count, char *** keyValuePairs ) {
  int i;

  for (i = 0; i < count; i ++) {
    free( (*keyValuePairs)[ i ] );
  }
  free( *keyValuePairs );
  *keyValuePairs = 0;
}

/* copied from gtgenup.c */
static void getGeotiffLabel(int unitIndex, char * infilename, int * count, char*** keyValuePairs ) {
  char key[ 33 ], format[ 9 ];
  int parmunit, maxlength, nelement, foundGeotiff;
  char * buf = 0;

  * count = 0;
  * keyValuePairs = 0;

  if ( zvunit( &parmunit, "INP", unitIndex, NULL) != 1 )
    zmabend ("gtgenup: error in zvunit for inp parm");
  if ( zvopen( parmunit, "OP", "READ", "OPEN_ACT", "SA", "LAB_ACT", " ", NULL) != 1 )
    zmabend ("gtgenup: error in zvopen for inp parm");

  foundGeotiff = 0;
  while ( zlninfo( parmunit, key, format, & maxlength, & nelement, NULL) == 1 ) {
    if ( ! strcmp( key, "PROPERTY" ) ) {
      buf = (char*) realloc( buf, maxlength + 1 );
      zlget( parmunit, "PROPERTY", key, buf,
	     "ERR_ACT", "SA",
	     "FORMAT", "STRING",
	     "NELEMENT", 1,
	     "PROPERTY", "GEOTIFF", NULL);
      if ( ! strcmp( buf, "GEOTIFF" ) )
	foundGeotiff = 1;
	break;
    }
  }

  if ( foundGeotiff ) {
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
	     "PROPERTY", "GEOTIFF", NULL);

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
  char infilename[100];
  char prefix[100];
  int geounit, parmct, parmdf, i;
  int itemCount;
  char ** keyValuePairs = 0;

  zifmessage( "gtdel version 2015-10-15" );

  zvparm( "inp", infilename, & parmct, & parmdf, 1, 99 );
  zvparm( "prefix", prefix, & parmct, & parmdf, 1, 99 );

  getGeotiffLabel( 1, infilename, & itemCount, & keyValuePairs );

  if (zvunit( & geounit, "INP", 1, NULL) != 1)
    zmabend ("gtdel: error in zvunit for inp");
  if (zvopen( geounit, "OP", "UPDATE", "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL) != 1)
    zmabend ("gtdel: error in zvopen for inp");

  for ( i = 0; i < itemCount; ++ i ) {
    if ( ! strncmp( keyValuePairs[ i ], prefix, strlen( prefix ) ) ) {
      char * equals;

      equals = strchr( keyValuePairs[ i ], '=' );
      if ( ! equals ) {
	char buf[ 100 ];
	sprintf( buf, "parm \"%s\" missing '='", keyValuePairs[ i ] );
	zifmessage( buf );
      } else {
	* equals = 0; /* chip the =value */
	zldel( geounit, "PROPERTY", keyValuePairs[ i ], "PROPERTY", "GEOTIFF", "ERR_ACT", " ", NULL);
      }
    }
  }

  freeKeyValuePairs( itemCount, & keyValuePairs );

  zvclose( geounit, NULL);
}
