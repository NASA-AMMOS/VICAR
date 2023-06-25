#ifndef OBJECT_TYPES_H
#define OBJECT_TYPES_H
// object_types.h 1.16 02/07/10 12:48:11
/** \file
 ** object type codes 
 **/

#define	BASE_OBJ		0
#define	POLY_V1			1
#define	CAM_V1			2
#define	WAVE_V1			3
#define	PLAY_V1			4
#define DIRECTIONAL_LIGHT_V1	5
#define POINT_LIGHT_V1		6
#define SPOT_LIGHT_V1 		7
#define AREA_LIGHT_V1		8
#define POLY_TERRAIN_V1  	9
#define PERF_V1                 10
#define GRP_V1                  11
#define OCTREE_V1		12
#define POLY_SPHERE_V1  	13
#define SFC_MODEL_V1		14

#define	NUM_OBJ_FILE_TYPES	15

// maybe types 100 and up are application-specific?

extern	char	*object_file_type_list[NUM_OBJ_FILE_TYPES];

#endif
