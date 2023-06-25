// object_creator.C 1.16 02/07/10 15:02:52
/** \file
 ** Create an appropriate type of object
 **/

#include "grape/object.h"
#include "grape/object_types.h"
#include "grape/poly_object_1.h"
#include "grape/wave_object_1.h"
#include "grape/playback.h"
#include "grape/camera.h"
#include "grape/light.h"
#include "grape/sfcmodel.h"
#include "grape/perf_object.h"

//******************************************************************************

char	*object_file_type_list[NUM_OBJ_FILE_TYPES] =
	{
		"BASE_OBJ",
		"POLY_V1",
		"CAM_V1",
		"#WAVE_OBJ_V1",
		"PLAY_V1",
		"DIRECTIONAL_LIGHT_V1",
		"POINT_LIGHT_V1",
		"SPOT_LIGHT_V1",
		"AREA_LIGHT_V1",
		"POLY_TERRAIN_V1",
		"PERF_V1",
		"GRP_V1",
		"OCTREE_V1",
		"POLY_SPHERE_V1",
		"SFC_MODEL_V1",
	};


int	object_file_num_from_type(char *type)
{
	int	i;

	for(i=0; i<NUM_OBJ_FILE_TYPES; i++) {
		if(!strcmp(object_file_type_list[i], type)) {
			return(i);
		}
	}
	return(-1);
}

Obj	*object_creator(Dataport *fp)
{
	char	token[128];
	Obj	*obj;

	get_next_token(fp, token);
	switch(object_file_num_from_type(token)) {
		case WAVE_V1 :
			obj = new WaveObject1();
			break;
		case POLY_V1 :
			obj = new PolyObject1();
			break;
		case CAM_V1 :
			obj = new Camera();
			break;
		case PLAY_V1 :
			obj = new PB_Obj();
			break;
		case DIRECTIONAL_LIGHT_V1:
			obj = new DirectionalLightObj();
			break;
		case POINT_LIGHT_V1:
			obj = new PointLightObj();
			break;
		case SPOT_LIGHT_V1:
			obj = new SpotLightObj();
			break;
		case AREA_LIGHT_V1:
			obj = new AreaLightObj();
			break;
		case POLY_TERRAIN_V1:
			obj = new PolyTerrainObj();
			break;
#ifdef _USE_PERFORMER_
		case PERF_V1:
			obj = new PerfObj();
			break;
#endif
		case GRP_V1:
			obj = new GroupObj();
			break;
		case OCTREE_V1:
			obj = new Octree();
			break;
		case SFC_MODEL_V1:
			obj = new SfcModel();
			break;
		default:
			fprintf(stderr,"Whoops - Unrecognized object type in file header = %s\n",token);
			obj = NULL;
			break;
	}
	return(obj);
}




