// geodata.C
//
// Written by John Wright 04/01/99

// This file does not contain any code for the geodata base class.
// Instead it contains a convenience function for instantiating a
// derived class from a token string and then using the derived
// classes from_string method to populate the object.

#include "image/types/kap_geodata.h"
#include "image/types/pds_geodata.h"


GeoData	*parse_object_geodata(char *token_string)
{
	char	type[256];
	GeoData	*gd = NULL;

	sscanf(token_string,"%255s",type);
	if(!strcmp(type,"KAPGeoData")) {
		gd = new KAPGeoData;
	} else if(!strcmp(type,"PDSGeoData")) {
		gd = new PDSGeoData;
	} else {
		fprintf(stderr,"Whoops - Unrecognized geodata type >%s< in token string.\n",type);
	}

	if(gd) {
		gd->from_string(token_string);
	}

	return(gd);
}
