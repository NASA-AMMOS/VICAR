// patch.C 1.2 02/08/07 15:15:52
/** \file
// Terrain patch class implementation
*/

#include "patch.h"
#include "summitt_func.h"

// helpers to parse one camera model component
static int scan_cmod(Dataport *fp, double v[3])
{
	char token[1024];
	if(get_next_token(fp, token))
		v[0] = atof(token);
	if(get_next_token(fp, token))
		v[1] = atof(token);
	if(get_next_token(fp, token))
		v[2] = atof(token);
	else
		return FALSE;
	return TRUE;
}	

static char *fmt_cmod(char *token, char id, double v[3])
{
	sprintf(token, "CMOD_%c %f %f %f\n", id, v[0], v[1], v[2]);
	return token;
}

/// Read ancillary data from forest file.
// Optionally read the referenced surface data also.
// Basic format is: PATCH_V1 {OBJECT|GEOOBJECT} <Objnode stuff>
// <patch ancillary stuff> END_PATCH
int Patch::parse_in(Dataport *fp, int expand)
{
	char token[4096];

	init();

	int err = TRUE;
	for (;;) {
		if(!get_next_token(fp, token))
			break;
		if(!strcmp(token, "PATCH_V1")) {
			// Get base objnode stuff, including world transform
			// and geodata. This also loads referenced sfcmodel 
			// data if expand is TRUE.
			if (!ObjNode::parse_in(fp, expand))
				return FALSE;
			// if loaded an object, check it's a valid type
			if (!obj)
				continue;
			if (obj->get_type() == OCTREE_V1) {
				// convert octree to SfcModel
				SfcModel *sfc = new SfcModel;
				*(Octree *)sfc = *(Octree *)obj;
				// can't delete octree, this would delete
				// voxels. so we leak a bit here...
				obj = sfc;
				// note mesh/pt_list is not initialized
			} else if (obj->get_type() != SFC_MODEL_V1) {
				fprintf(stderr, "Patch %s is wrong type %d\n",
					name, obj->get_type());
				return FALSE;
			}
		} else if(!strcmp(token, "BASE_MODEL")) {
			base_model = TRUE;
		} else if(!strcmp(token, "SRC_IMAGE")) {
			// ignore obsolete acq_time token
			get_next_token(fp, token);
			if(get_next_token(fp, token))
				xres = atoi(token);
			if(get_next_token(fp, token))
				yres = atoi(token);
			if(get_next_token(fp, token))
				bands = atoi(token);
			if(get_next_token(fp, token))
				fov = atof(token);
			else
				break;

		} else if(!strcmp(token, "CMOD_C")) {
			if (!scan_cmod(fp, cmod.c))
				break;		
		} else if(!strcmp(token, "CMOD_A")) {
			if (!scan_cmod(fp, cmod.a))
				break;		
		} else if(!strcmp(token, "CMOD_H")) {
			if (!scan_cmod(fp, cmod.h))
				break;		
		} else if(!strcmp(token, "CMOD_V")) {
			if (!scan_cmod(fp, cmod.v))
				break;		
		} else if(!strcmp(token, "CMOD_O")) {
			if (!scan_cmod(fp, cmod.o))
				break;		
		} else if(!strcmp(token, "CMOD_R")) {
			if (!scan_cmod(fp, cmod.r))
				break;		
		} else if(!strcmp(token, "BND_VOL")) {
			if(get_next_token(fp, token))
				bvol.xmin = atof(token);
			if(get_next_token(fp, token))
				bvol.ymin = atof(token);
			if(get_next_token(fp, token))
				bvol.zmin = atof(token);
			if(get_next_token(fp, token))
				bvol.xmax = atof(token);
			if(get_next_token(fp, token))
				bvol.ymax = atof(token);
			if(get_next_token(fp, token))
				bvol.zmax = atof(token);
			else
				break;			
		} else if(!strcmp(token, "RES")) {
			if(!get_next_token(fp, token))
				break;
			obj_res = atof(token);
		} else if(!strcmp(token, "ORIG_XFORM")) {
			if(get_next_token(fp, token))
				otrans[0] = atof(token);
			if(get_next_token(fp, token))
				otrans[1] = atof(token);
			if(get_next_token(fp, token))
				otrans[2] = atof(token);
			if(get_next_token(fp, token))
				orot[0] = atof(token);
			if(get_next_token(fp, token))
				orot[1] = atof(token);
			if(get_next_token(fp, token))
				orot[2] = atof(token);

#ifdef MER_MISSION
		} else if(!strcmp(token, "SITE")) {
			if(get_next_token(fp, token))
				site = atoi(token);
			if(get_next_token(fp, token))
				site_vector[0] = atof(token);
			if(get_next_token(fp, token))
				site_vector[1] = atof(token);
			if(get_next_token(fp, token))
				site_vector[2] = atof(token);
#endif

		} else if(!strcmp(token, "INST") ||
				!strcmp(token, "FILTER")) {
			// ignore obsolete fields
			get_next_token(fp, token);

		} else if(!strcmp(token, "END_PATCH")) {
			err = FALSE;	// successful
			break;
		} else {
			fprintf(stderr," Whoops - Unexpected token %s "
				"in Patch entry\n", token);
			return(FALSE);
		}
	}

	if (err) {
		fprintf(stderr," Whoops - Unexpected EOF in patch entry\n");
		return(FALSE);
	}

	return TRUE;
}


/// Write ancillary data to forest file. Does not write sfcmodel data.
int Patch::parse_out(Dataport *fp, int)
{
	char token[1024];

	put_token(fp, "PATCH_V1");

	ObjNode::parse_out(fp, FALSE);

	if (base_model)
		put_token(fp, "BASE_MODEL");

	sprintf(token, "SRC_IMAGE - %d %d %d %f\n", 
		xres, yres, bands, fov);
	put_token(fp, token);

	put_token(fp, fmt_cmod(token, 'C', cmod.c));
	put_token(fp, fmt_cmod(token, 'A', cmod.a));
	put_token(fp, fmt_cmod(token, 'H', cmod.h));
	put_token(fp, fmt_cmod(token, 'V', cmod.v));
	put_token(fp, fmt_cmod(token, 'O', cmod.o));
	put_token(fp, fmt_cmod(token, 'R', cmod.r));
	
	sprintf(token, "BND_VOL %f %f %f %f %f %f\n",
		bvol.xmin, bvol.ymin, bvol.zmin,
		bvol.xmax, bvol.ymax, bvol.zmax);
	put_token(fp, token);

	sprintf(token, "RES %f\n", obj_res);
	put_token(fp, token);

	sprintf(token, "ORIG_XFORM %f %f %f %f %f %f\n",
		otrans[0], otrans[1], otrans[2], orot[0], orot[1], orot[2]);
	put_token(fp, token);

#ifdef MER_MISSION
	sprintf(token, "SITE %d %lf %lf %lf\n", 
		site, site_vector[0], site_vector[1], site_vector[2]);
	put_token(fp, token);
#endif

	put_token(fp, "END_PATCH\n");
	return TRUE;
}

/// Update bounding volume from object voxel range
void Patch::update_bvol()
{
	summitt_bounding_volume((ObjNode *)this, &bvol);
}
