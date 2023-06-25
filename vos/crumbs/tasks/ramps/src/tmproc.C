// tmproc.C 1.15 04/01/09 19:21:37
/** \file
// Terrain processing setup and cleanup for master PVM task
//
// These functions do the manipulation of the SUMMITT terrain
// database for the master task, working with temporary files
// created for and written by PVM slave tasks.
//
// (In retrospect, maybe a lot of this should have been done
// in a Perl script or something...)
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include "image/image.h"
#include "image/types/all.h"
#include "grape/vector_ops.h"
#include "summitt_func.h"
#include "tp_req.h"

#define MAX_PATH	1024

static Forest forest;		///< the terrain forest (octree list)
static int fst_version;		///< forest version ID

extern char *base_path;		// base SUMMITT mission path in OSS file sys
extern char *temp_path;		// base temporary path in shared local file sys
extern int  verbose;

/// basic file copying, return TRUE if okay
static int copy_file(const char *src, const char *dest)
{
	FILE *infp = fopen(src, "r");
	if (infp == NULL) {
		fprintf(stderr, "copy_file: can't read source %s\n", src);
		return FALSE;
	}
    //make sure output file will have proper permissions
    umask(002);
	FILE *outfp = fopen(dest, "w");
	if (outfp == NULL) {
		fprintf(stderr, "copy_file: can't create dest %s\n", dest);
		fclose(infp);
		return FALSE;
	}

	char buf[4096];
	int n;
	int okay = TRUE;
	while ((n = fread(buf, 1, sizeof(buf), infp)) > 0) {
		if (fwrite(buf, 1, n, outfp) != n) {
			fprintf(stderr, "copy_file: write error to %s\n",
				dest);
			okay = FALSE;
			break;
		}
	}
	fclose(infp);
	if (fclose(outfp)) {
		fprintf(stderr, "copy_file: close error on %s\n", dest);
		return FALSE;
	}
	return okay;
}

/// Update copy of forest or octree on shared local disk 
// if version in OSS file system is newer (or local copy doesn't exist yet).
static void update_copy(const char *name)
{
	// nothing to do if no temp path
	if (!temp_path)
		return;

	// construct pathnames
	char base[MAX_PATH], temp[MAX_PATH];
	sprintf(base, "%s/%s", base_path, name);
	sprintf(temp, "%s/%s", temp_path, name);
	
	// get source modification date
	struct stat sbuf;
	if (stat(base, &sbuf)) {
		fprintf(stderr, "Can't stat OSS copy %s\n", base);
		return;
	}
	time_t src_time = sbuf.st_mtime;

	// Get copy modification date. If not smaller (older), we're okay
	if (!stat(temp, &sbuf) && sbuf.st_mtime >= src_time)
		return;

	// either the copy doesn't exist, or it's out-of-date, so update it
	if (verbose)
		fprintf(stderr, "Updating copy %s from %s\n", temp, base);
	copy_file(base, temp);
}

/// Setup data structures and files during PVM master initialization.
void master_setup(const char *svf_file)
{
	// load current forest (but not component models) from OSS path
	char fname[MAX_PATH];
	char buf[MAX_PATH];
	sprintf(fname, "%s/forest/current.fst", base_path);
	FILE_Dataport fp;
	if (!fp.ropen(fname)) {
		fprintf(stderr, "Can't open forest %s\n", fname);
		exit(1);
	}
	get_next_token(&fp, buf);	// skip GRP_V1 token
	if (!forest.parse_in(&fp, FALSE)) {
		fprintf(stderr, "Error reading forest %s\n", fname);
		exit(1);
	}
	fp.close();

	// get forest version. 'current.fst' should be a link to fstnnnn.fst
	fst_version = 1;	// in case of trouble
	int n = readlink(fname, buf, sizeof(buf)-1);
	if (n < 1) {
		fprintf(stderr, "Warning, %s is not a symlink\n", fname);
		sprintf(buf, "%s/forest/fst0001.fst", base_path);
	} else {
		char *dot = strrchr(buf, '.');
		if (dot==NULL || sscanf(dot-5, "t%d", &fst_version) != 1)
			fprintf(stderr, "Warning, unexpected forest name "
				"(%s -> %s)\n", fname, buf);
	}
	if (verbose)
		fprintf(stderr, "Current forest file is version %d\n",
			fst_version);

	// bring copies on shared local disk up-to-date
	update_copy("forest/current.fst");	// forest file
	for (int i=0; i<forest.get_num_children(); i++) {	// octrees
		ObjNode *model = forest.get_child(i);		
		sprintf(buf, "octree/%s", model->get_reference());
		update_copy(buf);
	}
}


#ifdef MER_MISSION

// MER replacement for forest::find_patch(), looks for patch
// with same ID except any version number (last character)
static int find_equivalent_patch(Forest *forest, const char *ident)
{
	int n = strlen(ident)-1;
	for (int i=0; i<forest->get_num_children(); i++) {
		Patch *p = forest->get_patch(i);
		if (!strncmp(p->get_name(), ident, n))
			return i;       // found a match
	}
	return -1;
}

#endif

/// Setup terrain processing request for processing by slave.
// Includes creation of patch object.
// If "cmod" is NULL or "-", camera model is taken from XYZ file header.
TP_Req *setup_request(char *xyz, char *img, char *cmod)
{
	// temporary patch object
	Patch p;
	
	// Get camera model if explicit
	if (cmod && strcmp(cmod, "-")) {
		if (p.cmod.read(cmod)) {
			fprintf(stderr, "Can't read camera model %s\n", cmod);
			return NULL;
		}
	} else {
		cmod = NULL;
	}
	
	// Read in the XYZ range map
	Image xyz_img(xyz);
	if (xyz_img.get_res() < 1) {
		fprintf(stderr, "XYZ input %s not found or invalid\n", xyz);
		return NULL;
	}
	
	// setup XYZ as PDS file
	PDSFile *xyz_file = NULL;
	if (xyz_img.get_file_type() == PDS_FILE_ID)
		xyz_file = (PDSFile *)xyz_img.get_file();
			
	// if no explicit camera model, should be in PDS header
	if (cmod == NULL) {
		if (xyz_file == NULL) {
			fprintf(stderr, "XYZ file %s isn't PDS and no "
					"camera model file given!\n", xyz);
			return NULL;
		}

		// Get (original) CAHVOR camera model from XYZ header
		get_pds_cmod(xyz_file, &p.cmod);

		// If XYZ has ROVER_COORDINATE_SYSTEM transform,
		// camera model is in rover frame; convert it to site frame
		cmod_rov_to_site(xyz_file, &p.cmod);
	}

#ifdef MER_MISSION
	// get site number from PDS header, and note site vector
	if (xyz_file) {
		p.site = get_pds_site(xyz_file);
		get_site_vector(svf_file, p.site, p.site_vector);
	}
#endif

	char pname[256];		// patch name
	// default to XYZ name minus path
	char *tail = strrchr(xyz, '/');	
	if (tail)
		strncpy(pname, tail+1, sizeof(pname));
	else
		strncpy(pname, xyz, sizeof(pname));
	tail = strrchr(pname, '.');
	if (tail)
		*tail = 0;

       // get range map resolution
        int xres, yres;
        xyz->get_res(&xres, &yres);

	// derive field of view
	p.fov = p.cmod.hfov(xres, yres);

	// setup working directory for the wedge
	char dname[MAX_PATH];
	sprintf(dname, "%s/%s", temp_path ? temp_path : base_path, pname);
	if (mkdir(dname, 0775) && errno != EEXIST) {
		fprintf(stderr, "Can't create work dir %s\n", dname);
		return NULL;
	}

	// copy stuff to shared local disk (if local, just link)
	int n = strlen(dname);

	// copy RGB image file (as is)
	strcpy(dname+n, "/tm.img");
	if (temp_path)
		copy_file(img, dname);
	else
		symlink(img, dname);
	// Get RGB dimensions for patch info
	Image rgb_img;
	rgb_img.read_header(img);
	if (rgb_img.get_res(&p.xres, &p.yres, &p.bands) < 1)
		fprintf(stderr, "Can't read RGB input %s\n", img);

	// copy XYZ range data as RAN file (ignore header)
	// (note, this releases xyz_file!)
	strcpy(dname+n, "/tm.xyz");
	if (temp_path) 
		xyz_img.write(dname, RAN_FILE_ID);
	else
		symlink(xyz, dname);
	
	dname[n] = 0;		// restore working pathname

	// Allocate a Patch object for this terrain wedge. This is never
	// freed, as it becomes part of the forest.
	Patch *tp = new Patch;
	*tp = p;		// copy the temporary one
	tp->set_name(pname);

	// allocate object to track the slave processing request 
	// (object gets deleted when slave has finished handling it)
	return new TP_Req(dname, tp);
}

/// Transform patch identifier into VISTA linearized filename
// <11chars>???<1+chars>[.ext] -> <11chars>VIL<1+chars>.VST
// <anything_else>[.ext] -> <anything_else>.VST
static char *vista_name(char *ident)
{
	static char buf[100];

	strncpy(buf, ident, 95);
	buf[95] = 0;
	char *dot = strrchr(buf, '.');	// remove any extension
	if (dot)
		*dot = 0;
	if (strlen(buf) > 11+3) {
		buf[11] = 'V';
		buf[12] = 'I';
		buf[13] = 'L';
	}
	strcat(buf, ".VST");
	return buf;
}

/// Handle completion of terrain processing request - update forest
void finish_request(TP_Req *req, int status, 
	double posn[3], double rot[3], Summitt_range *bvol, double obj_res)
{
	char name[MAX_PATH], tname[MAX_PATH];
	char *ident = req->patch->get_name();

	// check result status
	if (!(status & 2)) {
		fprintf(stderr, "Didn't create VST wedge for %s\n", ident);
		// create empty queue file for MIPL pipeline
		sprintf(tname, "%s/vistaque/%s.VST_EMPTY", base_path, ident);
        //make sure output file will have proper permissions
        umask(002);
		fclose(fopen(tname, "w"));
	}
	if (!(status & 1)) {
		fprintf(stderr, "Didn't create octree for %s\n", ident);

		// delete temporary data on shared disk (XYZ, RGB)
		sprintf(tname, "%s/tm.xyz", req->name);
		unlink(tname);
		sprintf(tname, "%s/tm.img", req->name);
		unlink(tname);
	
		// done with work directory
		rmdir(req->name);

		return;
	}
	if (!(status & 4))	// match failed (posn/rot = original)
		fprintf(stderr, "Didn't match terrain for %s\n", ident);

	// setup patch world transform data from result posn/rot
	req->patch->x = posn[0];
	req->patch->y = posn[1];
	req->patch->z = posn[2];
	req->patch->xrot = rot[0];
	req->patch->yrot = rot[1];
	req->patch->zrot = rot[2];

	// save patch bounding volume (world space)
	req->patch->bvol = *bvol;

	// save patch resolution
	req->patch->obj_res = obj_res;

	// transform patch's camera model to camera frame
	// (extract original pointing, also saved in patch)
	CAHVOR oldcmod = req->patch->cmod;
	summitt_cmod_to_pointing(&oldcmod, req->patch->orot, &req->patch->cmod);
	memcpy(req->patch->otrans, oldcmod.c, sizeof(req->patch->otrans));

	// setup reference to octree
	sprintf(tname, "../octree/%s.oct", ident);
	req->patch->set_reference(tname);

	// delete temporary data on shared disk (XYZ, RGB)
	sprintf(tname, "%s/tm.xyz", req->name);
	unlink(tname);
	sprintf(tname, "%s/tm.img", req->name);
	unlink(tname);

	// move result data (OCT, VST) from shared disk to OSS
	sprintf(tname, "%s/tm.oct", req->name);
	sprintf(name, "%s/octree/%s.oct", base_path, ident);
	if (temp_path) {
		copy_file(tname, name);
		unlink(tname);
	} else {
		rename(tname, name);
	}
	
	if (status & 2) {	// VST created okay
		sprintf(tname, "%s/tm.vst", req->name);
		sprintf(name, "%s/vista/%s", base_path, vista_name(ident));
		if (temp_path) {
			copy_file(tname, name);
			unlink(tname);
		} else {
			rename(tname, name);
		}

		// also create a soft link for MIPL notification/queuing
		// (if queue link directory exists)
		sprintf(tname, "%s/vistaque/%s", base_path, vista_name(ident));
		symlink(name, tname);
	}

	// done with work directory
	rmdir(req->name);

	// update forest (OSS copy)
	// if duplicate patch, replace old copy
#ifdef MER_MISSION
	int id = find_equivalent_patch(&forest, ident);
#else
	int id = forest.find_patch(ident);
#endif
	if (id >= 0) {
		fprintf(stderr, "Patch %s was already in forest, replacing.\n",
			ident);
		Patch *p = forest.get_patch(id);
		*p = *req->patch;	// copy fields
		ident = p->get_name();	// update this for message below
		delete req->patch;
	}

	// else add new patch to end
	else if (!forest.add_child(req->patch, -1)) {
		fprintf(stderr, "Failed adding patch %s to forest\n", ident);
		delete req->patch;
		return;
	}
		
	sprintf(name, "%s/forest/fst%04d.fst", base_path, ++fst_version);
	FILE_Dataport fp;
	if (!fp.wopen(name)) {
		fprintf(stderr, "Can't create forest %s\n", name);
	} else if (!forest.parse_out(&fp)) {
		fprintf(stderr, "Error writing forest %s\n", name);
		fp.close();
	} else {
		fp.close();
		if (verbose)
			fprintf(stderr, "Updated forest %s with patch %s\n", 
				name, ident);
		// okay, update "current" soft link
		char cname[MAX_PATH];
		sprintf(cname, "%s/forest/current.fst", base_path);
		unlink(cname);
		symlink(name, cname);
	}
}
