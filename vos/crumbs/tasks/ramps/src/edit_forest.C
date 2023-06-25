// edit_forest.C 1.4 02/10/17 07:18:44
/** \file
// This program maintains an octree forest.
// It can promote a new octree/surface model to a terrain patch, or
// modify an existing patch. It then adds or updates the patch in
// an octree forest, creating the forest if necessary.
*/
#include "summitt_func.h"
#include "patch.h"

static const char usage[] = 
"Usage:%s [-v] [-t tx ty tz] [-r rx ry ry] [-s sx sy sz]\n"
"[-p posn] [-n name] [-i surface_file] [-del] -f forest_file\n"
"[-base 0|1] [-img xres yres bands]\n"
"[-fov {x | auto}] [-cmod model_file] [-ot tx ty tz] [-or rx ry rz]\n"
"[-bvol xmin xmax ymin ymax zmin zmax | -bvgen] [-res {x | auto}]\n\n"
"-v = verbose output\n"
"-t/-r/-s = (revised) object->world translation, rotation, and scaling\n"
"-p = position in forest (0=first), default = last posn\n"
"-n = object node name, default = input filename\n"
"-i = input surface model/octree file, if adding new model\n"
"-f = (input and) output forest file\n"
"-del  = delete model from specified position\n"
"-base = indicates whether this model is part of forest baseline\n"
"-img  = original source (texture) image dimensions\n"
"-fov  = camera horizontal field of view in degrees\n"
"        (0=RH height map, -1=LH height map; auto=derive from camera model)\n"
"-cmod = camera model (cahvor) filename (in world frame; camera model is\n"
"        transformed to camera frame and transform is applied to patch)\n"
"-ccmod = camera model (cahvor) filename (in camera frame)\n"
"-ot/-or = original camera (obj->world) translation and rotation,\n"
"	   as used in XYZ range map creation\n"
"          Or use -oc to copy current transform\n"
"-bvol = model bounding volume in world frame\n"
"-bvgen = generated bounding volume from data\n"
"-res x = set model resolution to x world units\n"
"-res auto = set resolution from octree depth\n";

extern int verbose;

/// Estimate model resolution based on octree depth
static void auto_resolution(Patch *p)
{
	// read enough of the octree header to get transform and depth
	FILE_Dataport fp;
	char token[4096];
	Obj obj;

	if (p->get_reference() == NULL || !fp.open(p->get_reference())) {
		fprintf(stderr, "Whoops - Can't open referenced octree\n");
		return;
	}
	get_next_token(&fp, token);	// skip OCTREE_V1
	if(!obj.parse_in(&fp))
		return;			// failed to get transform

	for (;;) {
		if (!get_next_token(&fp, token)) {
			fprintf(stderr, "Whoops - Unexpected EOF in octree file\n");
			return;
		}
		if (!strcmp(token, "END_OCTREE")) {
			fprintf(stderr, "Whoops - Didn't find LEVELS in octree\n");
			return;
		}
		if (!strcmp(token, "LEVELS") && get_next_token(&fp, token))
			break;
	}

	int levels = atoi(token);
	double model_res = 4.0 / (1 << levels);
	// scale to object coords
	p->obj_res = model_res * (obj.xscale + obj.yscale + obj.zscale)/3.0;
}

// check forest for existing equivalent patch,
// return index, or -1 if not found
static int find_patch(Forest *forest, char *ident)
{
#ifdef MER_MISSION
	// look for patch with same ID,  any version number (last character)
	int n = strlen(ident)-1;
	for (int i=0; i<forest->get_num_children(); i++) {
		Patch *p = forest->get_patch(i);
		if (!strncmp(p->get_name(), ident, n))
			return i;       // found a match
	}
	return -1;
#else
	return forest->find_patch(ident);
#endif
}

int main(int argc, char **argv)
{
	char 	*sfcfile=NULL, *fstfile=NULL, *name=NULL;
	char	*cmodname=NULL;
	int	i;
	int 	posn = -1;
	int	base_model = -1;	
	int	xres = -1, yres, bands;
	float 	fov = -3.0;	// -3=leave alone, -2=auto, -1/0/+ = manual
	Summitt_range bvol;
	CAHVOR  cmod;
	int	ccmod = 0;
	double	trans[3], rot[3], scale[3];
	double 	res = -1.0;
	int	del_model = 0;
	int	tnew, rnew, snew, bvnew;
	tnew = rnew = snew = bvnew = 0;

	// original transform: 0 = leave alone,
	// 1 = use explicit otrans[] and orot[], 2 = copy from current
	int	orig_mode = 0;
	static double  otrans[3], orot[3];

	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-v")) {
			verbose = 1;
		} else if(!strcmp(argv[i],"-r") && i+3 < argc) {
			rnew = 1;
			rot[0] = atof(argv[++i]);
			rot[1] = atof(argv[++i]);
			rot[2] = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-or") && i+3 < argc) {
			orig_mode = 1;
			orot[0] = atof(argv[++i]);
			orot[1] = atof(argv[++i]);
			orot[2] = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-s") && i+3 < argc) {
			snew = 1;
			scale[0] = atof(argv[++i]);
			scale[1] = atof(argv[++i]);
			scale[2] = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-t") && i+3 < argc) {
			tnew = 1;
			trans[0] = atof(argv[++i]);
			trans[1] = atof(argv[++i]);
			trans[2] = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-ot") && i+3 < argc) {
			orig_mode = 1;
			otrans[0] = atof(argv[++i]);
			otrans[1] = atof(argv[++i]);
			otrans[2] = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-img") && i+3 < argc) {
			xres = atoi(argv[++i]);
			yres = atoi(argv[++i]);
			bands = atoi(argv[++i]);
		} else if(!strcmp(argv[i], "-bvol") && i+6 < argc) {
			bvnew = 1;
			bvol.xmin = atof(argv[++i]);
			bvol.xmax = atof(argv[++i]);
			bvol.ymin = atof(argv[++i]);
			bvol.ymax = atof(argv[++i]);
			bvol.zmin = atof(argv[++i]);
			bvol.zmax = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-bvgen")) {
			bvnew = 2;
		} else if(!strcmp(argv[i],"-p")) {
			posn = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-n")) {
			name = argv[++i];
		} else if(!strcmp(argv[i],"-i")) {
			sfcfile = argv[++i];
		} else if(!strcmp(argv[i],"-f")) {
			fstfile = argv[++i];
		} else if(!strcmp(argv[i],"-del")) {
			del_model = 1;
		} else if(!strcmp(argv[i],"-base")) {
			base_model = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-fov")) {
			if (!strcmp(argv[++i], "auto"))
				fov = -2.0;
			else
				fov = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-cmod")) {
			cmodname = argv[++i];
			ccmod = 0;
		} else if(!strcmp(argv[i], "-ccmod")) {
			cmodname = argv[++i];
			ccmod = 1;
		} else if(!strcmp(argv[i],"-res")) {
			if (!strcmp(argv[++i], "auto"))
				res = -2.0;
			else
				res = atof(argv[i]);
		} else if(!strcmp(argv[i],"-oc")) {
			orig_mode = 2;
		} else {
			fprintf(stderr,"Whoops - Unrecognized option %s\n",argv[i]);
			fprintf(stderr,usage, argv[0]);
			exit(1);
		}
	}

	// read in forest file (but not voxel data)
	Forest forest;
	if (fstfile == NULL) {
		fprintf(stderr, "Need to specify forest filename\n");
		fprintf(stderr,usage, argv[0]);
		exit(1);
	}
	FILE_Dataport fp;
	if (!fp.ropen(fstfile)) {	// forest doesn't exist yet
		if (sfcfile == NULL) {
			fprintf(stderr, "Need to specify either an input "
				"surface or an existing forest\n");
			exit(1);
		}
		if (verbose)
			fprintf(stderr, "Creating new forest file %s\n",
				fstfile);
	} else {
		char token[256];
		get_next_token(&fp, token); // skip GRP_V1 token
		if (!forest.parse_in(&fp, FALSE)) {
			fprintf(stderr, "Error loading forest %s\n",
				fstfile);
			exit(1);
		}
		if (verbose)
			fprintf(stderr, "Loaded forest from %s, "
				"has %d patch(es)\n", fstfile,
				forest.get_num_children());
	}
	fp.close();

	if (cmodname) {
		if (cmod.read(cmodname)) {
			fprintf(stderr, "Error reading camera model %s\n",
				cmodname);
			cmodname = NULL;	// don't update
		} else if (!ccmod) {
			// camera model is in world frame,
			// so transform to camera frame and get 
			// object-to-world transform for the patch.
			CAHVOR cm_old = cmod;
			summitt_cmod_to_pointing(&cm_old, rot, &cmod);
			vector_copy(cm_old.c, trans);
			rnew = tnew = 1;
		}
	}

	Patch *p;
	if (sfcfile) {		// create new patch
		if (verbose)
			fprintf(stderr, "Creating new patch from %s\n",
				sfcfile);
		p = new Patch();
		p->set_reference(sfcfile);
		p->set_name(name ? name : sfcfile);
		p->xres = p->yres = p->bands = 0;
		p->fov = 20.0f;
		p->bvol.init();
		memset(p->cmod.c, 0, 6*8*sizeof(double));

		// check if equivalent patch is already there
		int id = find_patch(&forest, p->get_name());
		if (id >= 0) {
			fprintf(stderr, "Patch %s was already in forest,"
				" replacing it.\n", p->get_name());
			Patch *oldp = forest.get_patch(id);
			*oldp = *p;	// copy in new data
			p = oldp;	// set pointer to existing patch
		} else if (!forest.add_child(p, posn)) {
			fprintf(stderr, "Failed adding patch\n");
		}

	} else {		// update (or delete) existing patch
		if (posn < 0 || posn >= forest.get_num_children())
			posn = forest.get_num_children() - 1;
		if (verbose && !del_model)
			fprintf(stderr, "Updating forest patch %d\n", posn);
		p = forest.get_patch(posn);
		if (name)
			p->set_name(name);
	}

	// set/update patch parameters
	if (base_model >= 0)
		p->base_model = base_model;
	if (xres > 0) {
		p->xres = xres;
		p->yres = yres;
		p->bands = bands;
	}

	if (tnew) {
		p->x.set_value(trans[0]);
		p->y.set_value(trans[1]);
		p->z.set_value(trans[2]);
	}
	if (rnew) {
		p->xrot.set_value(rot[0]);
		p->yrot.set_value(rot[1]);
		p->zrot.set_value(rot[2]);
	}
	if (snew) {
		p->xscale.set_value(scale[0]);
		p->yscale.set_value(scale[1]);
		p->zscale.set_value(scale[2]);
	}
	if (cmodname)
		p->cmod = cmod;
	if (bvnew == 1)
		p->bvol = bvol;
	else if (bvnew==2) {	// need to compute new bounding volume
		// note have to update transform before this point
		FILE_Dataport fp;
		// load voxels and compute bounding volume in world space
		if (p->parse_reference(&fp))
			p->update_bvol();
	}

	if (fov >= -1.0)	// manual set
		p->fov = fov;
	else if (fov == -2.0) {	// auto from camera model
		p->fov = p->cmod.hfov(xres, yres);
		if (verbose)
			fprintf(stderr, "HFOV = %f deg\n", p->fov);
	}

	if (res >= 0)		// manual resolution
		p->set_res(res);
	else if (res < -1.0)	// auto res
		auto_resolution(p);

	if (orig_mode == 1) {	// explicit original transform
		for (int i=0; i<3; i++) {
			p->otrans[i] = otrans[i];
			p->orot[i] = orot[i];
		}
	} else if (orig_mode == 2) {	// copy current to original
		p->otrans[0] = p->x;
		p->otrans[1] = p->y;
		p->otrans[2] = p->z;
		p->orot[0] = p->xrot;
		p->orot[1] = p->yrot;
		p->orot[2] = p->zrot;
	}

	// delete requested?
	if (del_model) {
		fprintf(stderr, "Deleting forest patch %d\n", posn);
		forest.remove_child(posn, FALSE);
	}

	// write out updated forest
	if (!fp.wopen(fstfile)) {
		fprintf(stderr, "Unable to open %s for writing\n", fstfile);
		exit(1);
	}
	forest.parse_out(&fp, FALSE);
	fp.close();

	return 0;
}
