// geodatatypes.h
//
// Written by John Wright 03/27/97

#ifndef _GEODATATYPES_H_
#define _GEODATATYPES_H_

// Georeferencing data types
#define BASE_GEO_DATA		1
#define KAP_GEO_DATA		2
#define PDS_GEO_DATA		3
#define WINKEL_GEO_DATA		4
#define MAP_GEO_DATA		5
#define CAMERA_GEO_DATA		6

// Geodetic Datum Types
typedef	int	DatumType;

#define	UNKNOWN		0	// Undetermined

#define	NAD02		1
#define	NAD27		2
#define	NAD83		3
#define	WGS84		4
#define	ASTRO		5	// Astronomic Datum
#define	ASTRO_GEN	6	// Astronomic Datum (general)
#define	ASTRO_GENS	7	// Astronomic Datums (general)
#define	ASTRO_28	8	// Astronomic Datum 1928 (field)
#define	ASTRO_30	9	// Astronomic Datum 1930 (field)
#define	ASTRO_31	10	// Astronomic Datum 1931 (field)
#define	ASTRO_39	11	// Astro Datum 1939
#define	ASTRO_LOCAL	12	// Local Astronomic Datum
#define	LOCAL_IND	13	// Local Datum (undetermined)
#define LOCAL		14	// Local Datum
#define	GARDNERS	15	// Gardners Pinnacles 1929
#define	NECKER		16	// Neckers 1928
#define	NIHOA		17	// Nihoa 1928
#define OLD_HAWAII	18	// Old Hawaiian
#define	TOGCHA		19	// Togcha Datum
#define	JOHNSTON	20	// Johnston Isl 1961 Astro Datum
#define NUM_DATUM_TYPES 21	// always the last number

// Projection Types
typedef	int	ProjectionType;

#define	MERCATOR	1	// Mercator
#define	UTM		2	// Universal Transverse Marcator
#define	POLYCONIC	3	// Polyconic
#define	LAMBERT		4	// Lambert Conformal Conic
#define GEO_LAT_LON	5	// geographic (Lat/Lon)
#define SINUSOIDAL	6	// sinusoidal used for PDS images
#define NUM_PROJECTION_TYPES	7	// always last number


#endif // _GEODATATYPES_H_
