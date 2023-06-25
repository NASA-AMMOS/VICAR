/* marsmesh */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigCAHV.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include "SimpleImage.h"
#include "lbl_derived_image.h"
#include "lbl_identification.h"
#include <mat3.h>
#include "cmod.h"
#include <stdlib.h>
#include <libgen.h>

#include <set>

#include "grape/object.h"
#include "grape/sfcmodel.h"

#include "image_processing.h"
#include "xyz_to_mesh.h"

using namespace std;

/* buffer sizes in main program */
#define MAX_OPEN 1
// Value of 0 indicates there is no
// upper-bound for input image size.
#define MAX_INPUTS 1
#define MAX_OPEN 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS
#define MAX_STRING_LENGTH 1024
#define TORADIANS  0.017453293  /* PI/180 to convert angles to radians */


////////////////////////////////////////////////////////////////////////


PigPoint convertWorldToObjFrame(PigPoint* world_pt, PigPoint* pt_offset, 
                                double rot[3][3]);
bool convertWorldToObjFrame(double* world_pt, double* pt_offset, 
                            double rot[3][3], double* cm_pt);
bool convertObjToWorldFrame(double* cm_pt, double* pt_offset, double rot[3][3], 
                            double* world_pt);
void create_camera_centric_frame(PigCAHV* camera_model, double rot[3][3]);
//void image_range_interpolate(SimpleImage<float> *inimg, int maxgap);
void make_LODs(FILE *fp, Range *bvol);

// subvolume IDs
enum {  GROUP_UL, GROUP_UR, GROUP_LR, GROUP_LL,
    LEAF_UL, LEAF_UR, LEAF_LR, LEAF_LL };

// Callback function pointer.
typedef void (*FQCB)(Range *vol, int subvol_id);

// recursive division
static void split(double tilesize, FQCB callback,
            Range *bvol, double mod_res, double area, int group_id)
{

    // find smallest voxel resolution of an overlapping patch
    double min_res = FLT_MAX;
    min_res = mod_res;


    // estimated number of voxels that could go into this tile
    // (may be huge!)
    double nvoxels = area / (min_res * min_res);

    // small enough to output this tile as a quadtree leaf node?
    if (nvoxels <= tilesize) {
        callback(bvol, group_id + LEAF_UL);
        return;
    }

    // need to split this tile into four subtiles
    callback(bvol, group_id);   // group marker

    area /= 4.0;
    double xc = 0.5 * (bvol->xmin + bvol->xmax);
    double yc = 0.5 * (bvol->ymin + bvol->ymax);
    Range subvol = *bvol;
    subvol.xmax = xc;
    subvol.ymax = yc;
    split(tilesize, callback, &subvol, mod_res, area, GROUP_UL);
    subvol.xmin = xc;
    subvol.xmax = bvol->xmax;
    split(tilesize, callback, &subvol, mod_res, area, GROUP_UR);
    subvol.ymin = yc;
    subvol.ymax = bvol->ymax;
    split(tilesize, callback, &subvol, mod_res, area, GROUP_LR);
    subvol.xmin = bvol->xmin;
    subvol.xmax = xc;
    split(tilesize, callback, &subvol, mod_res, area, GROUP_LL);
}

// Create quadtree list of terrain tiles given a forest and
// a nominal number of voxels per tile.
// For each tile, callback is invoked with the bounding volume,
// and either TRUE for a leaf node or FALSE for a subdivided tile.
// If input bvol is null, use overall forest volume.
//
// (This service could be used independently of the Inventor mesh
// generation application included below.)
void forest_quadtree(double tilesize, FQCB callback, Range *bvol, double mod_res)
{
    // find overall bounding volume, check for valid resolution data
    //Range bv;
    //bv.empty();
    //if (bvol == NULL)   // use default overall volume?
    //    bvol = &bv;

    // find region size
    double area = (bvol->xmax - bvol->xmin) * (bvol->ymax - bvol->ymin);

    // start recursive subdivision
    split(tilesize, callback, bvol, mod_res, area, GROUP_UL);
}

//--------------------------------------
// Sample usage, writes quadtree as Inventor file,
// with bounding volume in comments

static int depth;
static char subvol[100];    // current subvolume ID stack
static int tile_count;      // diagnostics
//static FILE *mesh_iv = fopen("mesh.iv", "w");
static FILE *mesh_iv;

static void node_cb(Range *r, int id)
{
    if (depth == 0)  {
        fprintf (mesh_iv, "#Inventor V2.0 ascii\n");
    }

    while (++subvol[depth] > '4') { // finish previous groups
        fprintf(mesh_iv, "}\n");
        subvol[depth--] = 0;
    }

    if (id < LEAF_UL) { // new group
        //fprintf(mesh_iv, "%.1s",subvol);
        fprintf(mesh_iv, "Group { # %s ", subvol);
        subvol[++depth] = '0';
        subvol[depth+1] = 0;
    } else {        // leaf
        fprintf(mesh_iv, "File { name \"tile%s.iv\" } # ", subvol);
        tile_count++;
    }
    fprintf(mesh_iv, "%f %f %f %f\n", r->xmin, r->xmax, r->ymin, r->ymax);
}



void main44()
{
#define MSG_SIZE 256
    char msg[MSG_SIZE];
    zvmessage("MARSMESH version 2020-12-01", "");

    int nids;
    char mission[64], instrument[64];
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    // PigSurfaceModel *surface_model;
    RadiometryModel *radiometric[MAX_INPUTS];
    PigCoordSystem *output_cs;
    int homogeneous_inputs = TRUE;
    double mdlcam[3];
    // cell edge length at each row/column, -1 for invalid cell
    double *edge_matrix;
    double zstretch = 1.0;
    double range_low = 0.0;
    double range_high = 30.0;
    double range_mid = 30.0;
    double range_low_actual = range_low;
    double range_high_actual = range_high;
    int MAX_LOD = 20;
    double range_low_lod[MAX_LOD];
    double range_high_lod[MAX_LOD];
    bool do_range_intervals = false;
    double tilesize = 0.0;
    int tile_ds_lvl_max = 0;
    int maxgap = 0;
    int lod_levels = 0;
    int res_max = 0;
    int res_min = 0;
    int line_exclude_min, line_exclude_max, sample_exclude_min, sample_exclude_max;
    line_exclude_min = line_exclude_max =  sample_exclude_min =  sample_exclude_max = 0;
    int pyr_curr = 1;
    
    //max angle between triangle face and view direction (deg)
    double max_angle = 0.0;  //make sure it's either default or from cmd line 
    int subsample_factor_x = 1;
    int subsample_factor_y = 1;
    short num_lod = 0;
    short max_lod = 1;
    int count, def;
    bool write_out_normals = false;

    double corr_quality = 0.25;
    double baseline = 0.0;
    double aspect_ratio = 0.5;
    double num_sigma = 1.0;
    double wfactor = 1.0;
    double density = 4.0;
    int min_window_type[2];
    min_window_type[0] = min_window_type[1] = 1.0;
    bool adaptive = true;

    zvparmd("X_SUBSAMPLE", &subsample_factor_x, &count, &def, 1, 0);
    if ((subsample_factor_x <= 0) || 
        (subsample_factor_x/2.0 != floor(subsample_factor_x/2.0) && 
        (subsample_factor_x != 1))) {
        zvmessage("Subsample Factor X HAS to be equal to 1 or be a power of 2 positive integer", "");
        zabend();
    }
    zvparmd("Y_SUBSAMPLE", &subsample_factor_y, &count, &def, 1, 0);
    if (subsample_factor_y != subsample_factor_x)
        // override provided value or default value for y if it doesn't match X
        subsample_factor_y = subsample_factor_x;
    if ((subsample_factor_y <= 0) || 
        (subsample_factor_y/2.0 != floor(subsample_factor_y/2.0) && 
        (subsample_factor_y != 1))) {
        zvmessage("Subsample Factor Y HAS to be equal to 1 or be a power of 2 positive integer", "");
        zabend();
    }
    snprintf(msg, MSG_SIZE, "\nX_SUBSAMPLE = %d, Y_SUBSAMPLE = %d\n", subsample_factor_x, subsample_factor_y);
    zvmessage(msg, "");
    
    if (zvptst("DO_NORMALS")) {   /* the only table that is optional */ 
        write_out_normals = true;
        snprintf(msg, MSG_SIZE, "\nThe output file will contain Normals Per Vertex\n");
        zvmessage(msg, "");
    }


    mars_setup(nids, file_models, camera_in, pointing_in,
               radiometric, output_cs, mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    const LblDerivedImage_typ *lbl_derived_image = file_models[0]->getLblDerivedImage();
    const LblIdentification_typ *lbl_identification = file_models[0]->getLblIdentification();
    PigPoint c;
    double c_pt[3];
    double a_vt[3];
    PigVector a, h, v;
    //apply scaling
    camera_in[0]->scaleCamera(1.0/subsample_factor_x, 1.0/subsample_factor_y);
    camera_in[0]->setCoordSystem(output_cs);
    ((PigCAHV*)camera_in[0])->getCurrentCAHV(c, a, h, v); 
    c.getXYZ(c_pt);
    a.getXYZ(a_vt);
    double r[3][3];
    create_camera_centric_frame((PigCAHV*)camera_in[0], r);

    ZMatrix m2cam;
    // ZMatrix m2cam_inv;
    ZMatrix modelToWorld;
    m2cam[0][0] = r[0][0];
    m2cam[0][1] = r[0][1];
    m2cam[0][2] = r[0][2];
    m2cam[0][3] = c.getX();

    m2cam[1][0] = r[1][0];
    m2cam[1][1] = r[1][1];
    m2cam[1][2] = r[1][2];
    m2cam[1][3] = c.getY();

    m2cam[2][0] = r[2][0];
    m2cam[2][1] = r[2][1];
    m2cam[2][2] = r[2][2];
    m2cam[2][3] = c.getZ();

    m2cam[3][0] = 0.0;
    m2cam[3][1] = 0.0;
    m2cam[3][2] = 0.0;
    m2cam[3][3] = 1.0;

    MatInvert(m2cam, modelToWorld);

    int yres = file_models[0]->getNL();
    int xres = file_models[0]->getNS();

    int yres_tex = yres;
    int xres_tex = xres;

    // process command line args

    if (zvptst("ADAPTIVE")) {
        zvparmd("BASELINE", &baseline, &count, &def, 1, 0);
        if (count == 0) {           // get from label
            baseline = file_models[0]->getStereoBaseline(0.0);
             if (baseline == 0.0) {
                zvmessage("Stereo baseline not found in label, must be provided in parameter", "");
                zabend();
             }
            snprintf(msg, MSG_SIZE, "\nStereo baseline from label: %f", baseline);
            zvmessage(msg, "");
        } else {
            snprintf(msg, MSG_SIZE, "\nStereo baseline overridden to %f", baseline);
            zvmessage(msg, "");
        }
        max_lod=4;
    }
    else {
        adaptive = false;
        max_lod=1;
    }
 
    zvparmd("RANGE_MIN", &range_low, &count, &def, 1, 0);
    if (range_low < 0.0) {
        zvmessage("RANGE_MIN cannot be smaller than 0.", "");
        zabend();
    }
    zvparmd("RANGE_MAX", &range_high, &count, &def, 1, 0);
    if (range_high > 10000000.0) {
        zvmessage("RANGE_MAX cannot be higher than 10000000.0", "");
        zabend();
    }
    zvparmd("RANGE_MID", &range_mid, &count, &def, 1, 0);
    if (count != 0) {
        if (range_mid > range_high || range_mid < range_low) {
          zvmessage("RANGE_MID cannot be higher than RANGE_MAX or lower than RANGE_MIN", "");
          zabend();
        }
        else {
           do_range_intervals = true;
        } 
    }
      
    snprintf(msg, MSG_SIZE, "Mininum Range is = %g, Maximum Range is = %g", range_low, range_high);
    zvmessage(msg, "");

    zvparmd("TILESIZE", &tilesize, &count, &def, 1, 0);
    if (tilesize < 4.0) {
        zvmessage("%s: Min tile size is 4\n", "");
        zabend();
    }
    
    zvparmd("TILE_DS_LVL_MAX", &tile_ds_lvl_max, &count, &def, 1, 0);
    if (tile_ds_lvl_max < 0) {
        zvmessage("%s: TILE DOWNSAMPLE LEVEL is invalid\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "Tile Downsample Level Max is = %d", tile_ds_lvl_max);
        zvmessage(msg, "");
    }

    zvparmd("MAXGAP", &maxgap, &count, &def, 1, 0);
    if (maxgap < 0) {
        zvmessage("%s: Maxgap is measured in pixels and can't be negative\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "Maxgap is = %d", maxgap);
        zvmessage(msg, "");
    }

    zvparmd("LOD_LEVELS", &lod_levels, &count, &def, 1, 0);
    if (lod_levels < 1 || lod_levels > 20) {
        zvmessage("%s: Number of LOD Levels  can't be less than 1 or greater than 20\n", "");
        zabend();
    }
    zvparmd("MAX_ANGLE", &max_angle, &count, &def, 1, 0);
    if (max_angle < 0.0 || max_angle > 90.0) {
        zvmessage("%s: Max_Angle is measured in degrees and can't be negative or greater than 90.0\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "Max angle is = %f", max_angle);
        zvmessage(msg, "");
    }

    zvparmd("RES_MAX", &res_max, &count, &def, 1, 0);
    if (res_max < 1 ) {
        zvmessage("%s: Res Max  is the maximum number of triangles per adaptive layer\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "Res Max is = %d", res_max);
        zvmessage(msg, "");
    }

    zvparmd("RES_MIN", &res_min, &count, &def, 1, 0);
    if (res_min < 1 ) {
        zvmessage("%s: Res Min  is the minimum number of triangles per adaptive layer\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "Res Min is = %d", res_min);
        zvmessage(msg, "");
    }

    zvparmd("CORR", &corr_quality, &count, &def, 1, 0);
    if (corr_quality < 0.0 || corr_quality > 1.0) {
        zvmessage("%s: Correlation Quality value is out of range: [0,1]\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "Corr quality is = %f", corr_quality);
        zvmessage(msg, "");
    }

    zvparmd("ASPECT_RATIO", &aspect_ratio, &count, &def, 1, 0);
    if (aspect_ratio < 0.0) {
        zvmessage("%s: Aspect ratio can't be negative\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "Aspect Ratio is = %f", aspect_ratio);
        zvmessage(msg, "");
    }

    zvparmd("NUM_SIGMA", &num_sigma, &count, &def, 1, 0);
    if (num_sigma < 0.0) {
        zvmessage("%s: Sigma can't be negative\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "NUM SIGMA is = %f", num_sigma);
        zvmessage(msg, "");
    }

    zvparmd("WFACTOR", &wfactor, &count, &def, 1, 0);
    if (wfactor < 0.0) {
        zvmessage("%s: wfactor can't be negative\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "WFactor is = %f", wfactor);
        zvmessage(msg, "");
    }

    //Implement handling of MIN_WINDOW

    zvparmd("DENSITY", &density, &count, &def, 1, 0);
    if (density < 0.0) {
        zvmessage("%s: Density factor can't be negative\n", "");
        zabend();
    }
    else {
        snprintf(msg, MSG_SIZE, "Density is = %f\n", density);
        zvmessage(msg, "");
    }
    
    zvparmd("LINE_EXCL_MIN", &line_exclude_min, &count, &def, 1, 0);
    if (line_exclude_min < 0)  {
        zvmessage("%s: Line Exclude Min can't be negative\n", "");
        zabend();
    }
    zvparmd("LINE_EXCL_MAX", &line_exclude_max, &count, &def, 1, 0);
    if (line_exclude_max < 0)  {
        zvmessage("%s: Line Exclude Max can't be negative\n", "");
        zabend();
    }
    zvparmd("SAMPLE_EXCL_MIN", &sample_exclude_min, &count, &def, 1, 0);
    if (sample_exclude_min < 0)  {
        zvmessage("%s: Sample Exclude Min can't be negative\n", "");
        zabend();
    }
    zvparmd("SAMPLE_EXCL_MAX", &sample_exclude_max, &count, &def, 1, 0);
    if (sample_exclude_max < 0)  {
        zvmessage("%s: Sample Exclude Max can't be negative\n", "");
        zabend();
    }
    
    //orig_left_tile = new SimpleImage<int>(nll, nsl);
    //SimpleImage<int> * orig_left_tile = new SimpleImage<int>(file_models[0]->getNL(), file_models[0]->getNS());
    //SimpleImage<float> * out_image = new SimpleImage<float>(3, file_models[0]->getNL(), file_models[0]->getNS());
    //std::set<int> orig_left_tiling_level;
    //int unit = file_models[0]->getUnit();
    //int nl = file_models[0]->getNL();
    //int ns = file_models[0]->getNS();
    //int statusL = mars_create_tile_res_map(file_models[0], orig_left_tile, orig_left_tiling_level);
    //int my_tile_level = orig_left_tile->get(0, 100, 200);
    //my_tile_level = orig_left_tile->get(0, 2100, 4000);
    //bool  status = mask_image_by_tile_resolution(float_image[0], orig_left_tile, int res_level, SimpleImage<double> *& out_image);


    if ((line_exclude_min == 0) || (line_exclude_max == 0) || (sample_exclude_min == 0) || (sample_exclude_max == 0))
        line_exclude_min = line_exclude_max = sample_exclude_min = sample_exclude_max = 0;


    char format_str[256];
    zvp("FORMAT", format_str, &count);
    bool format_file_iv = false;
    if (!strncmp(format_str, "ALL", 3) || !strncmp(format_str, "INVENTOR", 8))
        format_file_iv = true;
        
    // create strings for output filenames
    char output_name_str[PIG_MAX_FILENAME_SIZE];
    char output_name_lod_str[PIG_MAX_FILENAME_SIZE];
    char output_obj_name_lod_str[PIG_MAX_FILENAME_SIZE];
    char output_mtl_name_lod_str[PIG_MAX_FILENAME_SIZE];
    char output_lbl_name_lod_str[PIG_MAX_FILENAME_SIZE];
    char input_xyz_name_str[PIG_MAX_FILENAME_SIZE];
    //char output_name_mtl_str[PIG_MAX_FILENAME_SIZE];
    zvp("OUT", output_name_str, &count);
    //sprintf(msg, "Input file name: %s\n", input_name_str);
    //zvmessage(msg, "");
    //sprintf(msg, "output file name: %s\n", file_models[0]->getProductId());
    char *dot = strrchr(basename(output_name_str), '.');
    char *basename_file = basename(output_name_str);
    // short basename_length = strlen(basename_file);
    short output_name_length = strlen(output_name_str);
    short ext_length = strlen(dot);
    short id_length = strlen(basename_file) - strlen(dot);
    short output_mtl_name_length = output_name_length-ext_length+4;
    char output_mtl_name_str[output_mtl_name_length+1];
    //label fragment for PDS-4
    char output_lbl_name_str[output_mtl_name_length+1];
    //output inventor file
    char output_iv_name_str[output_mtl_name_length];
    char base_name_str[output_mtl_name_length];
    memset(output_mtl_name_str, '\0', sizeof(output_mtl_name_str));
    memset(output_lbl_name_str, '\0', sizeof(output_mtl_name_str));
    memset(base_name_str, '\0', sizeof(output_mtl_name_str));
    strncpy(output_mtl_name_str, output_name_str, output_mtl_name_length-4);
    strcpy(output_lbl_name_str, output_mtl_name_str);
    strcpy(output_iv_name_str, output_mtl_name_str);
    strcpy(base_name_str, output_mtl_name_str);
    
    //strcat(output_mtl_name_str, ".mtl");
    //strcat(output_lbl_name_str, ".lbl");
    strcat(output_iv_name_str, ".iv");
    char base[id_length+1];
    memset(base, '\0', sizeof(base));
    strncpy(base, basename_file, id_length);
    //sprintf(msg, "output file name: %s\n", base);
    //zvmessage(msg, "");

    // End of create strings for the filenames

    int pyr_cnt = log(subsample_factor_x)/log(2) + 1;
    //sprintf(msg, "pyr_cnt=%d\n", pyr_cnt);
    //zvmessage(msg, "");

    // read in XYZ image
    //SimpleImage<float> *float_image[pyr_cnt+1];
    //SimpleImage<float> *float_image[lod_levels+pyr_cnt+1];
    SimpleImage<float> *float_image[MAX_LOD+pyr_cnt+1];
    memset(float_image, 0, sizeof(float_image));
    mars_read_inputs(0, 0, file_models, float_image, MAX_NL, MAX_NS, 0, NULL, 
                     NULL); 
    //file_models[0]->setFileOpen(true);
    int yres_sub = yres;
    int xres_sub = xres;

/*
    bool has_1 = (orig_left_tiling_level.find(1) == orig_left_tiling_level.end());
    bool has_2 = (orig_left_tiling_level.find(2) == orig_left_tiling_level.end());
    bool has_4 = (orig_left_tiling_level.find(4) == orig_left_tiling_level.end());
    bool has_8 = (orig_left_tiling_level.find(8) == orig_left_tiling_level.end());
    if (orig_left_tiling_level.size() != 0 && (orig_left_tiling_level.find(tile_ds_lvl_max) != orig_left_tiling_level.end())) {
        //int my_tile_level = orig_left_tile->get(0, 100, 200);
        bool status =  mask_image_by_tile_resolution(float_image[0], orig_left_tile, tile_ds_lvl_max, out_image);
        float_image[0] = out_image;
    } 
*/

    //for (int cnt_tile_lvl = 0; cnt_tile_lvl < orig_left_tiling_level.size(); cnt_tile_lvl++) {
    //    bool status =  mask_image_by_tile_resolution(float_image[0], orig_left_tile, pow(2, cnt_tile_lvl), out_image);
    //    float_image[0] = out_image;
    //}

    // initialize ranges for adaptive range filtering
    range_low_lod[0] = range_low;
    range_high_lod[0] = range_high;
    range_high_actual = range_high;
    FILE *output_iv_file = NULL;
    SfcModel *octree = NULL;
    Triangle_Model *tm;              // triangle mesh
    bool discarded = false;
    //SimpleImage<float> *float_image_initial = new SimpleImage<float>(3, file_models[0]->getNL(), file_models[0]->getNS());
    //copy_image(SimpleImage<float> *inp_image, SimpleImage<float> *& out_image) {
    //copy_image(float_image[0], float_image_initial);
    //float_image_initial = float_image[0];
    
    
    for (num_lod = 0; num_lod < max_lod; num_lod++) {
    if (num_lod == 1)
        res_max = 100000;
    else if (num_lod == 2)
        res_max = 50000;
    else if (num_lod ==3)
        res_max = 10000;

    // initialize ranges
    pyr_curr = 1;
    range_low_lod[0] = range_low;
    range_high_lod[0] = range_high;
    range_high_actual = range_high;
    octree = NULL;
    //float_image[0] = float_image_initial;
    yres_sub = yres = file_models[0]->getNL();
    xres_sub = xres = file_models[0]->getNS();
    SimpleImage<int> * orig_tile = new SimpleImage<int>(file_models[0]->getNL(), file_models[0]->getNS());
    SimpleImage<float> * out_image = new SimpleImage<float>(3, file_models[0]->getNL(), file_models[0]->getNS());
    std::set<int> orig_tiling_level;
    int status = mars_create_tile_res_map(file_models[0], orig_tile, orig_tiling_level);
    std::set<int>::iterator it = orig_tiling_level.begin();
    bool is_first_loop = true;
    int cnt_lod = -1;
    while (it != orig_tiling_level.end() || is_first_loop) {
        is_first_loop=false;
        if ( it != orig_tiling_level.end()) {
            int tile_lvl = *it;
            it++;
            if (tile_ds_lvl_max != 0 && tile_ds_lvl_max != tile_lvl)
                continue;
            if (zvptst("USE_TILE_RES")) {
                bool status =  mask_image_by_tile_resolution(float_image[0], orig_tile, tile_lvl, out_image);
                float_image[0] = out_image;
            }
            else 
                it = orig_tiling_level.end();
        }
    //}
    bool process_more_lod_levels = true;
    cnt_lod = -1;

    int levels_aspect_X = (int)(log(1/aspect_ratio)/log(2));
    
    snprintf(msg, MSG_SIZE, "number of levels based on aspect ratio=%d\n", levels_aspect_X);
    zvmessage(msg, "");
    //need to be while loop
    while ( process_more_lod_levels ) {
    
    cnt_lod++;
 
    if (cnt_lod == 0 && format_file_iv == true && num_lod == 0) {
       output_iv_file = fopen(output_iv_name_str, "w");
    }

    if (cnt_lod == 1) {
        pyr_curr *=2;
        float_image[1] = float_image[0];
        //!!! apply cmd line limits to the actuals
        range_low_lod[cnt_lod] = fmax(range_low_actual, range_low);
        if (adaptive) 
          range_high_lod[cnt_lod] = fmin(density * sqrt(pyr_curr*pyr_curr -1 ) * (0.8/0.5) * baseline / corr_quality, range_high);
        else
          range_high_lod[cnt_lod] = fmin(range_high_actual, range_high);
        if (do_range_intervals && (range_high_lod[cnt_lod] < range_high)) {
            range_high_lod[cnt_lod] = range_high;
        }
        discarded = true;
        continue;
    }

    if (cnt_lod > 1) {


        pyr_curr *=2;
        if (levels_aspect_X > 0 && cnt_lod > 2) {
            levels_aspect_X--;
            xres_sub /= 2;
            float_image[cnt_lod] = new SimpleImage<float>(3, yres_sub, xres_sub);
            image_resample_X(float_image[cnt_lod-1], float_image[cnt_lod]);
        }
        else if (cnt_lod >2) {
            xres_sub /= 2;
            yres_sub /= 2;
            float_image[cnt_lod] = new SimpleImage<float>(3, yres_sub, xres_sub);
            image_resample(float_image[cnt_lod-1], float_image[cnt_lod]);
        }
        else {
            //!!!! ozp
            //float_image[cnt_lod] = float_image[cnt_lod-1];
            //copy_image(SimpleImage<float> *inp_image, SimpleImage<float> *& out_image) {
            float_image[cnt_lod] = new SimpleImage<float>(3, yres_sub, xres_sub);
            copy_image(float_image[cnt_lod-1], float_image[cnt_lod]);
        }

        if (!discarded) {
            if (adaptive)
              range_high_lod[cnt_lod] = fmin(density * (sqrt(pyr_curr*pyr_curr -1 ) * (0.8/0.5) * baseline / corr_quality), range_high);
            else
              range_high_lod[cnt_lod] = fmin(range_high_actual, range_high);
            if (do_range_intervals && (range_high_lod[cnt_lod] < range_high)) {
                range_high_lod[cnt_lod] = range_high;
            }
            
            range_low_lod[cnt_lod] = range_high_lod[cnt_lod-1];
            if ((range_low_lod[cnt_lod] - range_low_actual) < 0)
               range_low_lod[cnt_lod] = range_low_actual;
        }
        else {
            range_low_lod[cnt_lod] = range_low_lod[cnt_lod-1];
            range_high_lod[cnt_lod] = range_high_lod[cnt_lod-1];
        }

        long int num_polygons = 0;
        double range_out_min, range_out_max;
        range_out_min = range_out_max = 0;
        //long num_polygons = get_num_polygons(float_image[cnt_lod], NULL, c_pt, modelToWorld, mdlcam, camera_in[0]->getPixelAngle(0), max_angle, range_low_lod[cnt_lod], range_high_lod[cnt_lod]);
        populate_octree(float_image[cnt_lod], NULL, &num_polygons, &range_out_min, &range_out_max, false, c_pt, modelToWorld, mdlcam, camera_in[0]->getPixelAngle(0), max_angle, range_low_lod[cnt_lod], range_high_lod[cnt_lod]);

        if (num_polygons > res_max) {
            discarded = true;
            snprintf(msg, MSG_SIZE, "No data to process for xres_sub=%d, yres_sub=%d, Range Limit is = %lf\n", xres_sub, yres_sub, range_high_lod[cnt_lod]);
            zvmessage(msg, ""); 
            continue;
        }
        discarded = false;
        //process_more_lod_levels = false; --ozp check this
        if (!adaptive)
          process_more_lod_levels = false;

        //impose the limit on downsampling
        if (xres_sub <  64 || yres_sub < 64) {
            range_high_lod[cnt_lod] = range_high_actual;
            process_more_lod_levels = false;
        }
    }

    if ((range_high_lod[cnt_lod] - range_low_lod[cnt_lod]) < 0.0) {
        snprintf(msg, MSG_SIZE, "No data to process for xres_sub=%d, yres_sub=%d, Range Limit is = %lf\n", xres_sub, yres_sub, range_high_lod[cnt_lod]);
        zvmessage(msg, ""); 
        continue;
    }

    if ((range_low_lod[cnt_lod] > range_high_actual) || cnt_lod > MAX_LOD) {
        process_more_lod_levels = false;
        continue;
    }
    
    if (range_high_lod[cnt_lod] > range_high_actual) 
        process_more_lod_levels = false;

    if (adaptive && cnt_lod > 0)
        range_low_lod[cnt_lod] -= range_high_lod[cnt_lod-1]/10;
    range_high_lod[cnt_lod] += range_high_lod[cnt_lod]/10;

    if(maxgap && cnt_lod != 1) {
        //perform interpolation
        image_range_interpolate(float_image[cnt_lod], maxgap);
    }

    SimpleImage<float> *xyz_image = float_image[cnt_lod];
    yres = yres_sub;
    xres = xres_sub;
    snprintf(msg, MSG_SIZE, "\nxres = %d, yres = %d, num_lod=%d\n", xres, yres, num_lod);
    zvmessage(msg, "");

    // allocate array to track previous row's nodes for mesh
    NodeSpec **prow = new NodeSpec *[xres];
    // Allocate space for edge storage
    int edge_size = xres*yres;
    edge_matrix = new double[edge_size];
    memset(edge_matrix, 0, edge_size * sizeof(double));
    // find data limits in image
    // Get the Z values to compute the depth, and then the level
    int edge_index;
    double  min_x, max_x;
    double  min_y, max_y;
    double  min_z, max_z;
    double  range_min, range_max, range_mid;
    double  min_range, max_range;
    double  min_edge, max_edge;
    double scale;
    double pixfov = camera_in[0]->getPixelAngle(0);
    bool verbose = false;
    double imgxyz[3];
    
    min_x = min_y = min_z = range_min = min_edge = min_range = FLT_MAX;
    max_x = max_y = max_z = range_max = range_mid = max_edge = max_range = -FLT_MAX;

    int invalid_counter = 0;
    int minrange_counter = 0;
    int maxrange_counter = 0;

    PigPoint xyz;
    // double pt_max_world[3];
    int number_of_valid_pts = 0;
    //!!! ozp adjust range_low, range_high to offsetted values
    for(int j=0; j<yres; j++) {  //line counter
        edge_index = j * xres;  //column counter
        for(int i=0; i<xres; i++, edge_index++) {
        //if ( (j >=  line_exclude_min) && (j <= line_exclude_max) && (i >=sample_exclude_min) && (i<=sample_exclude_max) )
        //    continue;
            
      // char msg[256];
          // bool is_valid = true;
            PigPoint xyz_cm;
            double cm_pt[3];
            if (!getPoint3D(xyz_image, j, i, imgxyz))
                continue;
            number_of_valid_pts++;

            double tmp[3];
            sub3(c_pt, imgxyz, tmp);
            //apply offset of camera location to global/site coords 
            xyz_cm.setXYZ(tmp);

            // Set edge here
            // double dcam[3];
            double pt_range=xyz_cm.magnitude();
            xyz_cm.getXYZ(cm_pt);


            if (pt_range < min_range)
                min_range = pt_range;
            if (pt_range > max_range)
                max_range = pt_range;
  
            
            if (pt_range < range_low_lod[cnt_lod]) {
                minrange_counter++;
                edge_matrix[edge_index] = -1.0;
                continue;
            }
            if (pt_range > range_high_lod[cnt_lod]) {
                maxrange_counter++;
                edge_matrix[edge_index] = -1.0;
                continue;
            }
            //convert back to global/site
            xyz_cm.setXYZ(imgxyz);

            // Set mins and maxes once we know the point is
            // valid and within range

            if(xyz_cm.getX() < min_x) min_x = xyz_cm.getX();
            if(xyz_cm.getX() > max_x) max_x = xyz_cm.getX();
            if(xyz_cm.getY() < min_y) min_y = xyz_cm.getY();
            if(xyz_cm.getY() > max_y) max_y = xyz_cm.getY();
            if(xyz_cm.getZ() < min_z) min_z = xyz_cm.getZ();
            if(xyz_cm.getZ() > max_z) max_z = xyz_cm.getZ();

            edge_matrix[edge_index] = pt_range*pixfov;

            // The smallest range will indicate the smallest edge.
            if (pt_range < range_min) {
                range_min = pt_range;
                min_edge = edge_matrix[edge_index];
            }
            if (pt_range > range_max) {
                range_max = pt_range;
                max_edge = edge_matrix[edge_index];
            }
        }   //column counter
    }   //line counter
    
    //maxrange_counter = 0;
    snprintf(msg, MSG_SIZE, "number_of_valid_ptss=%d\n", number_of_valid_pts);
    zvmessage(msg, "");

    double pt_min[3] = {min_x, min_y, min_z};
    double pt_max[3] = {max_x, max_y, max_z};


   //range_max = 100.0;
   if (cnt_lod == 0) {
        range_low_actual = range_min;
        range_high_actual = range_max;
   }

    if (verbose) {
        snprintf(msg, MSG_SIZE, "\nxres = %d, yres = %d, num_lod=%d\n", xres, yres, num_lod);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "range limits: %g to %g", range_min, range_max);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "edge range is %g to %g", min_edge, max_edge);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "x range is %g to %g", pt_min[0], pt_max[0]);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "y range is %g to %g", pt_min[1], pt_max[1]);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "z range is %g to %g", pt_min[2], pt_max[2]);
        zvmessage(msg, "");
     }
     if (min_edge <= 0.0) {
         snprintf(msg, MSG_SIZE, "Min_edge must be greater than zero, is %f\n",
                 min_edge);
         zvmessage(msg, "");
         delete [] edge_matrix;
         //Free memory for each individual object pointed to by the elements of prow
         for (int i = 0; i < xres; i++) {
             delete prow[i];
         }
         // Free memory for the prow array itself.
         delete [] prow;
         return;
     }

     if (octree == NULL) {        // create a new octree

        // Choose the largest of the difference between the x, y, z to
        // determine the scale (largest dimension of the bounding box).
        scale = max_x - min_x;
        if ( (max_y - min_y) > scale)
            scale = max_y - min_y;
        if ( (max_z - min_z) > scale)
            scale = max_z - min_z;

        // If had just one point, scale could be 0 because max_x - min_x = 0.
        if (scale < max_edge)
            scale = max_edge;

        // Calculate the octree depth
        // 2^levels = depth
        // levels = log (Base 2) depth = log (depth) / log (2);
        // round up
        // Depth is the ratio of the largest boundary dimension to the smallest
        // voxel
        double depth = scale/min_edge;

        // levels is the number of levels needed in the octree
        int levels = int(ceil( log(depth)/log(2.0) )) + 1;
        if (verbose) {
            snprintf(msg, MSG_SIZE, "scale = %lf, min_edge = %lf, depth = %lf, depth = %lf, level = %d\n", 
                    scale, min_edge, scale/min_edge, depth, levels);
            zvmessage(msg, "");
        }

        // In the case of 1 point, we could end up with a levels of 0, which
        // is impossible.  Therefore, make sure levels is at least 1.
        if (levels <= 0)
            levels = 1;

        // Create the octree/surface model
        octree = new SfcModel(levels);
     tm = &(octree->mesh);
     tm->clean_up();
     } // end of create a new octree 

     //tm = &(octree->mesh);
     //tm->clean_up();
     NodeSpec *ns = NULL, *pns = NULL;
     // double  cntr[OCTREE_DIMS];
     uchar red, green, blue;
     red=255;
     green=blue=0;
     //if (insfc == NULL) {
     if (octree == NULL) {
     //if (octree != NULL) {
        // Setup model-object transformation matrix
        octree->x = -(max_x + min_x)/scale;
        printf ("max_x %f\n", max_x);
        printf ("min_x %f\n", min_x);
        printf ("scale %f\n", scale);
        octree->y = -(max_y + min_y)/scale;
        //!!!! ozp octree->z = -(max_z + min_z) * zstretch / scale;
        octree->z = 0.0;
        octree->xrot = octree->yrot = octree->zrot = 0.0;
        scale *= 0.5;           // modify scale since volume ranges -1 to 1
        octree->xscale = octree->yscale = scale;
        octree->zscale = scale / zstretch;
     //printf("xscale = %f\n", octree->x);
     //printf("yscale = %f\n", &octree->y);
     //printf("zscale = %f\n", &octree->z);
     } else {    // extract existing scale factor
           scale = octree->xscale;
     }

     // No more changes to transform parms, so precompute matrices
     // to speed up add_voxel()
     octree->freeze_xform();
     ZMatrix xform;
     octree->GetModelToObjectTransform(xform);

     // Subdivide quadtree
     Range bv;
     bv.xmin = min_x;
     bv.xmax = max_x;
     bv.ymin = min_y;
     bv.ymax = max_y;
     bv.zmin = -FLT_MAX;
     bv.zmax = FLT_MAX;

     //get voxel resolution in model space, convert to object space
     double mod_res = 4.0 / (1 << octree->get_max_levels());
     mod_res *= ((max_x - min_x) +
                 (max_y - min_y)+
                 (max_z - min_z))
                 / 3.0;

     snprintf(msg, MSG_SIZE, "Voxel Resolution: %f\n", mod_res);
     zvmessage(msg, "");
     //printf("mod_res = %f\n", mod_res);

    
   //!!!! tiling
   //  forest_quadtree(tilesize, node_cb, &bv, mod_res);
  
     // double gc[3], cc[3];
     MatPreMult(modelToWorld, xform);
      
     // map camera position from object coords to model space
     octree->GetObjectToModelTransform(xform);
     ZMatrix xxform;
     octree->GetModelToObjectTransform(xxform);
     MultPoints(c_pt, xform, mdlcam);


     // The first run of lod loop is to create and initialize octree
     // while processing the whole image
     if (cnt_lod == 0 && adaptive)
         continue;

     // Read through the range image and create voxels
     // Loop:  Read the range file one pixel at a time
     // for each pixel
     //    create a voxel
     //    get color and calculate edge

     int counter = 0;

     // Some xyz's will be "bad" and need to be tossed. kfs
     for(int j=0; j<yres; j++) {
        edge_index = j * xres;
        if (xres>1) {
            prow[xres-2] = pns;
            prow[xres-1] = ns;
        }
        ns = NULL;

        //for(int i=0; i<xres; i++, edge_index++) {
        for(int i=0; i<xres; i+=1, edge_index+=1) {
            if (i>1)
                prow[i-2] = pns;
            pns = ns;

            // Skip invalid points
            if (edge_matrix[edge_index] <= 0.0) {
                ns = NULL;
            } else {
                ns = new NodeSpec();

                // Get color from rgb file, or default
                //if (rgb)
                //        rgb->get_color(i, j,
                //                        &red, &green, &blue);
                ns->set_color(red, green, blue);
                ns->use_alpha(TRUE);

                // Division is for scaling.
                ns->edge_length = edge_matrix[edge_index]/scale;

                //getxyz(i, j, cntr);
                getPoint3D(xyz_image, j, i, imgxyz);
                ns->set_global_center(imgxyz);

                // compute voxel surface normal vector
                double norm[3];
//!!!! i, j                 
range_normal(xyz_image, i, j, xres, yres, norm);
                ns->set_normal(norm);

                octree->add_voxel(ns);
                counter++;
            }

            // update mesh, if not left column or top row
            if (/*(flags & R2O_MESH) &&*/ i>0 && j>0)
                add_mesh(prow[i-1], prow[i], pns, ns, tm, max_angle, mdlcam);
        }
    }  // tossing bad xyz

    if (octree->get_data())
        octree->add_pt_list();

    // clean up
    delete [] edge_matrix;
    //Free memory for each individual object pointed to by the elements of prow
    for (int i = 0; i < xres; i++) {
        //delete prow[i];
    }
    // Free memory for the prow array itself.
    delete [] prow;

    verbose = true;
    if (verbose) {
        snprintf(msg, MSG_SIZE, "Found %d invalid points\n", invalid_counter);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "Found %d <%f(min-range), %d >%f(max-range) points\n",
        minrange_counter, range_min, maxrange_counter, range_max);
        zvmessage(msg, "");
        //fprintf(stderr, "Discarded %d sliver triangles\n",
        //        sliver_count);
        //fprintf(stderr, "Rejected %d triangles for bad direction\n",
        //       angle_count);
        snprintf(msg, MSG_SIZE, "Added total of %d points\n", counter);
        snprintf(msg, MSG_SIZE,"Full-res mesh is %d triangles\n",octree->mesh.tri_count());
        zvmessage(msg, "");
    } //end of verbose
    
     // get voxel resolution in model space, convert to object space
    double resolution = 4.0 / (1 << octree->get_max_levels());
    resolution *= (octree->xscale + octree->yscale + octree->zscale) / 3.0;

}   // end of LOD loop
    //delete float_image[cnt_lod];
}   // end of tile res loop
    //int cnt_lod = 0;
    delete orig_tile;
    delete out_image;
    FILE *output_lbl_file = NULL;
    if (num_lod != 0) {
       sprintf(output_name_lod_str, "%s_LOD%02d_03\0", base_name_str, num_lod);
       sprintf(output_obj_name_lod_str, "%s_LOD%02d_03.obj\0", base_name_str, num_lod);
       sprintf(output_mtl_name_lod_str, "%s_LOD%02d_03.mtl\0", base_name_str, num_lod);
    }
    else {
        sprintf(output_name_lod_str, "%s\0", base_name_str);
        sprintf(output_obj_name_lod_str, "%s.obj\0", base_name_str);
        sprintf(output_mtl_name_lod_str, "%s.mtl\0", base_name_str);
        sprintf(output_lbl_name_lod_str, "%s.lbl\0", base_name_str);
        output_lbl_file = fopen(output_lbl_name_lod_str, "w");
    }

    FILE *tfile = fopen(output_obj_name_lod_str, "w");
    FILE *output_mtl_file = fopen(output_mtl_name_lod_str, "w");
    //FILE *output_lbl_file = fopen(output_lbl_name_lod_str, "w");
    

    //if (!tfile || !output_mtl_file || !output_lbl_file || !output_iv_file) {
    if (!tfile || !output_mtl_file ) {
        zvmessage("Unable to create or open output files", "");
        zabend();
    }
    //tm->print_obj_file(tfile1, NULL);
    //fclose(output_lbl_file);

    // Track the current position in the output file for PDS-4 label
    unsigned long lbl_offset = 0;

    //write out .mtl file
    char input_skin_str[PIG_MAX_FILENAME_SIZE];
    zvp("IN_SKIN", input_skin_str, &count);
    PigFileModel* skin_fm = PigFileModel::create(input_skin_str);
    if (skin_fm == NULL) {
        sprintf(msg, "Unable to read input file %s\nWill assume texture image size is the same as input XYZ", input_skin_str);
        zvmessage(msg, "");
    }
    else {
        skin_fm->openFile();
        xres_tex = skin_fm->getNS();
        yres_tex = skin_fm->getNL();
        skin_fm->closeFile();
    }
    //sprintf(msg, "Input texture file name: %s\n", input_skin_str);
    //zvmessage(msg, "");
    fprintf(output_mtl_file, "#lbl %s%s\r\n", basename(output_lbl_name_str),".lbl");
    fprintf(output_mtl_file, "#obj %s%s\r\n", basename(output_lbl_name_str),".obj");

    strcpy(input_xyz_name_str, basename((char*)file_models[0]->getFilename()));
    // remove extension by inserting null-character 
    char* ext_str = strrchr(input_xyz_name_str, '.');
    if (ext_str != NULL) *ext_str='\0';
    fprintf(output_mtl_file, "#xyz %s%s\r\n", input_xyz_name_str,".IMG");

    fprintf(output_mtl_file, "#mtl %s%s\r\n\n", basename(output_mtl_name_str),".mtl");
    if (lbl_identification != NULL) {
        fprintf(output_mtl_file, "#INSTRUMENT_ID %s\r\n", file_models[0]->getInstrumentId());
        if (lbl_identification->StartTime.Valid)
            fprintf(output_mtl_file, "#START_TIME %s\r\n", lbl_identification->StartTime.Value);
        if (lbl_identification->StopTime.Valid)
            fprintf(output_mtl_file, "#STOP_TIME %s\r\n", lbl_identification->StopTime.Value);
    }

    if (lbl_identification != NULL) {
        fprintf(output_mtl_file, "#ROVER_MOTION_COUNTER_NAME ( ");
        int i = 0;
        while (lbl_identification->RoverMotionCounter[i].Valid) {
            if ( i > 0)  //make entries comma-separated
                fprintf(output_mtl_file, ", ");
            fprintf(output_mtl_file, "%s ", file_models[0]->getRoverMotionCounterName(i));
            i++;
        }
        fprintf(output_mtl_file, ")\r\n");
        fprintf(output_mtl_file, "#ROVER_MOTION_COUNTER ( ");
        i = 0;
        while (lbl_identification->RoverMotionCounter[i].Valid) {
            if ( i > 0) //make entries comma-separated
                fprintf(output_mtl_file, ", ");
            fprintf(output_mtl_file, "%d ", lbl_identification->RoverMotionCounter[i].Value);
            i++;
        }
        fprintf(output_mtl_file, ")\r\n\n");
    }

    if (lbl_derived_image != NULL) {
        if (lbl_derived_image->ReferenceCoordSystemName.Valid)
            fprintf(output_mtl_file, "#REFERENCE_COORD_SYSTEM_NAME %s\r\n", lbl_derived_image->ReferenceCoordSystemName.Value);
        fprintf(output_mtl_file, "#REFERENCE_COORD_SYSTEM_INDEX_NAME ( ");
        int i = 0;
        while (lbl_derived_image->ReferenceCoordSystemIndex[i].Valid) {
            if ( i > 0)  //make entries comma-separated
                fprintf(output_mtl_file, ", ");
            fprintf(output_mtl_file, "%s ", file_models[0]->getRoverMotionCounterName(i));
            i++;
        }
        fprintf(output_mtl_file, ")\r\n");
        fprintf(output_mtl_file, "#REFERENCE_COORD_SYSTEM_INDEX ( ");
        i = 0;
        while (lbl_derived_image->ReferenceCoordSystemIndex[i].Valid) {
            if ( i > 0) //make entries comma-separated
                fprintf(output_mtl_file, ", ");
            fprintf(output_mtl_file, "%d ", lbl_derived_image->ReferenceCoordSystemIndex[i].Value);
            i++;
        }
        fprintf(output_mtl_file, ")\r\n\n");
    }
    dot = strrchr(basename(input_skin_str), '.');
    basename_file = basename(input_skin_str);
    ext_length = strlen(dot);
    id_length = strlen(basename_file);
    char base_skin[id_length];
    char skin_name_no_ext[id_length];
    memset(base_skin, '\0', id_length);
    strncpy(base_skin, basename_file, id_length-strlen(dot));
    //fprintf(output_mtl_file, "newmtl %s\r\n", base_skin);
    fprintf(output_mtl_file, "newmtl %s\r\n", output_name_lod_str);
    strcpy(skin_name_no_ext, base_skin);
    //strcat(base_skin, ".png");
    //sprintf(msg, "input texture file name: %s\n", base_skin);
    //zvmessage(msg, "");
    fprintf(output_mtl_file, "       Ka 0.2 0.2 0.2 1\r\n");
    fprintf(output_mtl_file, "       Kd 0.8 0.8 0.8 1\r\n");
    fprintf(output_mtl_file, "       Ks 0 0 0 1\r\n");
    fprintf(output_mtl_file, "       map_Kd %s%s\r\n", base_skin,".png");
    fclose(output_mtl_file);
    //if (num_lod != 0) {
    //    remove(output_obj_name_lod_str);
    //    remove(output_mtl_name_lod_str);
    //}

    // Get the current position in the output file for PDS-4 label
    lbl_offset = ftell(tfile);

    if (output_iv_file != NULL && num_lod == 0) {
        //write .iv header
        fprintf(output_iv_file, "#Inventor V2.0 ascii\n");
        fprintf(output_iv_file, "DEF Texture_%s\n", skin_name_no_ext);
        fprintf(output_iv_file, "Texture2 {filename \"%s%s\" "
                                "wrapS CLAMP wrapT CLAMP}\n", base_skin, ".png");
        fprintf(output_iv_file,"ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }\n");
        fprintf(output_iv_file,"TextureCoordinateBinding { value PER_VERTEX }\n");
        fprintf(output_iv_file,"LevelOfDetail {\n");
        //fprintf(output_iv_file,"screenArea [ 256000, 54000, 16000, 4000, 1000 ]\n");
        fprintf(output_iv_file,"screenArea [ 30000000, 10000000, 1000000, 100000, 10000 ]\n");
        //fprintf(output_iv_file, "Separator { # LevelOfDetail 0\n");
    }
        fprintf(output_iv_file, "Separator { # LevelOfDetail %d\n", num_lod);

    //Write corresponding name of .mtl file
    fprintf(tfile, "mtllib %s%s\r\n", output_name_lod_str, ".mtl");

    //do not write out output label if there is no geometry
    if (octree->mesh.tri_count() < 1) {
        output_lbl_file = NULL;
        //output_iv_file = NULL;
    }

    if (output_lbl_file != NULL) {
        //write .lbl header tables for PDS-4
        fprintf(output_lbl_file, "<Table_Delimited>\n");
        fprintf(output_lbl_file, "\t<name>OBJ Material Reference Table</name>\n");
        fprintf(output_lbl_file, "\t<offset unit=\"byte\">%lu</offset>\n", 
                                  lbl_offset);
        fprintf(output_lbl_file, "\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
        fprintf(output_lbl_file, "\t<description>Material file for the OBJ file</description>\n");
        fprintf(output_lbl_file, "\t<records>1</records>\n");
        fprintf(output_lbl_file, "\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf(output_lbl_file, "\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf(output_lbl_file, "\t<Record_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<fields>1</fields>\n");
        fprintf(output_lbl_file, "\t\t<groups>0</groups>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>mtllib</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>1</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Description of .mtl file.</description>\n");
        fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t</Record_Delimited>\n");
        fprintf(output_lbl_file, "</Table_Delimited>\n");

        //Write corresponding material name into the output file
        // Get the current position in the output file for PDS-4 label
        lbl_offset = ftell(tfile);
        //fprintf(tfile, "usemtl %s\r\n", base_skin);
        //fprintf(tfile, "usemtl %s\r\n", output_name_lod_str);
        fprintf(output_lbl_file, "<Table_Delimited>\n");
        fprintf(output_lbl_file, "\t<name>OBJ Material Name Table</name>\n");
        fprintf(output_lbl_file, "\t<offset unit=\"byte\">%lu</offset>\n", 
            lbl_offset);
        fprintf(output_lbl_file, "\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
        fprintf(output_lbl_file, "\t<description>Material name referenced by OBJ file</description>\n");
        fprintf(output_lbl_file, "\t<records>1</records>\n");
        fprintf(output_lbl_file, "\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf(output_lbl_file, "\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf(output_lbl_file, "\t<Record_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<fields>1</fields>\n");
        fprintf(output_lbl_file, "\t\t<groups>0</groups>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>usemtl</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>1</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Describes which material to use.</description>\n");
        fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t</Record_Delimited>\n");
        fprintf(output_lbl_file, "</Table_Delimited>\n");
    }
    //fprintf(tfile, "usemtl %s\r\n", base_skin);
    fprintf(tfile, "usemtl %s\r\n", output_name_lod_str);
    
    //write vertices
    // Get the current position in the output file for PDS-4 label
    lbl_offset = ftell(tfile);
    if(output_iv_file != NULL)
        fprintf(output_iv_file, "Coordinate3 {point [\n");
    // Get the current position in the output file for PDS-4 label
    int vindex = 0;
    int count_vertices = 0;
    NodeSpec_List_Element *nle;
    if (octree->mesh.tri_count() > 1) {
      for (nle = tm->pt_list; nle; nle = nle->next) {
            nle->nodespec->id = ++vindex;   // save for face index
            double gc[3], sc[3];
            nle->nodespec->get_global_center(gc);
            ZMatrix xxform;
            octree->GetModelToObjectTransform(xxform);
            MultPoints(gc, xxform, sc);
            fprintf(tfile,"v \t%f\t%f\t%f\r\n", sc[0], sc[1], sc[2]);
            // write in Open Inventor Format
            if(output_iv_file != NULL)
                fprintf(output_iv_file,"%f %f %f,\n", sc[0], sc[1], sc[2]);
            count_vertices++;
      } // end of write vertices
    }

    if(output_iv_file != NULL)
        fprintf(output_iv_file, "]}\n");


    // only write out PDS tables if there is data
    if (count_vertices > 0 && output_lbl_file != NULL) {
        // write out PDS-4 Label Fragement for Vertices Table
        fprintf (output_lbl_file, "<Table_Delimited>\n");
        fprintf (output_lbl_file, "\t<name>OBJ Vertices Table</name>\n");
        fprintf (output_lbl_file, "\t<offset unit=\"byte\">%lu</offset>\n", 
                lbl_offset);
        fprintf(output_lbl_file,"\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
        fprintf(output_lbl_file,"\t<description>Table of Vertices of the OBJ file</description>\n");
        fprintf(output_lbl_file, "\t<records>%d</records>\n",count_vertices);
        fprintf(output_lbl_file,"\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf(output_lbl_file,"\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf(output_lbl_file,"\t<Record_Delimited>\n");
        fprintf(output_lbl_file,"\t\t<fields>4</fields>\n");
        fprintf(output_lbl_file,"\t\t<groups>0</groups>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>OBJ Datatype</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>1</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>v letter for vertex</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>VertX</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>2</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>X coordinate of the Vertex in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Vertex Y</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>3</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Y coordinate of the vertex in Specific Site Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Vertex Z</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>4</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Z coordinate of the vertex in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t</Record_Delimited>\n");
        fprintf(output_lbl_file,"</Table_Delimited>\n");
    }

    // the only table that is optional 
    if ((count_vertices > 0) && write_out_normals && num_lod == 0) {
        //write normals
        // Get the current position in the output file for PDS-4 label
        lbl_offset = ftell(tfile);
        int count_normals = 0;
        nle = NULL;
        double norm[3] /*, norm_cs[3] */ ;
        for (nle = tm->pt_list; nle; nle = nle->next) {
            //  nle->nodespec->id = ++vindex;   // save for face index
            nle->nodespec->get_normal(norm);
            //MultPoints(norm, xxform, norm_cs);
            fprintf(tfile,"vn \t%f\t%f\t%f\r\n", norm[0], norm[1], norm[2]);
            count_normals++;
        } // end of write normals 


        // write out PDS-4 Label Fragement for normals Table
        if (output_lbl_file != NULL) {
        fprintf (output_lbl_file, "<Table_Delimited>\n");
        fprintf (output_lbl_file, "\t<name>OBJ Normals Table</name>\n");
        fprintf (output_lbl_file, "\t<offset unit=\"byte\">%lu</offset>\n", lbl_offset);
        fprintf(output_lbl_file,"\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
        fprintf(output_lbl_file,"\t<description>Table of Normals of the OBJ file</description>\n");
        fprintf(output_lbl_file, "\t<records>%d</records>\n",count_normals);
        fprintf(output_lbl_file,"\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf(output_lbl_file,"\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf(output_lbl_file,"\t<Record_Delimited>\n");
        fprintf(output_lbl_file,"\t\t<fields>4</fields>\n");
        fprintf(output_lbl_file,"\t\t<groups>0</groups>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>OBJ Datatype</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>1</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>vn letter for normal</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Normal component Vx</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>2</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Vx coordinate of the Normal in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Normal component Vy</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>3</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Vy coordinate of the normal in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Normal component Vz</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>4</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Vz coordinate of the normal in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t</Record_Delimited>\n");
        fprintf(output_lbl_file,"</Table_Delimited>\n");
        }
    }

    //write texture coordinates
    if (output_iv_file != NULL) { 
        fprintf(output_iv_file, "DEF %s Separator {\n", skin_name_no_ext);
        fprintf(output_iv_file, "USE Texture_%s\n", skin_name_no_ext);
        fprintf(output_iv_file, "TextureCoordinate2 { point [\n");
    }

    // Get the current position in the output file for PDS-4 label
    lbl_offset = ftell(tfile);
    unsigned long counter_texture_coords = 0;
    Mesh_Triangle *t;
    for (t = tm->tri_list; t; t = t->next) {
        NodeSpec *ns1 = t->get_vertex_1();
        if (ns1 == NULL)        // skip if clipped
            continue;
        NodeSpec *ns2 = t->get_vertex_2();
        NodeSpec *ns3 = t->get_vertex_3();
        double st[6];
        //we always doing full-res texture
        vertex_texture(ns1, camera_in[0], c_pt, modelToWorld, xres_tex, yres_tex, st+0);
        vertex_texture(ns2, camera_in[0], c_pt, modelToWorld, xres_tex, yres_tex, st+2);
        vertex_texture(ns3, camera_in[0], c_pt, modelToWorld, xres_tex, yres_tex, st+4);

        if(output_iv_file != NULL)
            fprintf(output_iv_file, "%.4f %.4f, %.4f %.4f, %.4f %.4f, \n",
                             st[0], st[1], st[2], st[3], st[4], st[5]);

        fprintf(tfile, "vt \t%.4f\t%.4f\r\nvt \t%.4f\t%.4f\r\nvt \t%.4f\t%.4f\r\n",
                       st[0], st[1], st[2], st[3], st[4], st[5]);
        counter_texture_coords +=3;
    
    } //end of write texture coords
    if(output_iv_file != NULL)
        fprintf(output_iv_file, "]}\n");

    // only write out PDS tables if there is data
    if (count_vertices > 0 & output_lbl_file != NULL) {
    //write out PDS-4 Label Fragment for Texture Table
        fprintf (output_lbl_file, "<Table_Delimited>\n");
        fprintf (output_lbl_file, "\t<name>OBJ Texture Coordinates Table</name>\n");
        fprintf (output_lbl_file, "\t<offset unit=\"byte\">%lu</offset>\n", lbl_offset);
        fprintf (output_lbl_file, "\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
        fprintf (output_lbl_file, "\t<description>Table of Texture Coordinates of the OBJ file</description>\n");
        fprintf (output_lbl_file, "\t<records>%ld</records>\n", counter_texture_coords);
        fprintf (output_lbl_file, "\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf (output_lbl_file, "\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf (output_lbl_file, "\t<Record_Delimited>\n");
        fprintf (output_lbl_file, "\t\t<fields>3</fields>\n");
        fprintf (output_lbl_file, "\t\t<groups>0</groups>\n");
        fprintf (output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t\t<name>OBJ Datatype</name>\n");
        fprintf (output_lbl_file, "\t\t\t<field_number>1</field_number>\n");
        fprintf (output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf (output_lbl_file, "\t\t\t<description>vt letter for UV Texture Mapping</description>\n");
        fprintf (output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t\t<name>TextU</name>\n");
        fprintf (output_lbl_file, "\t\t\t<field_number>2</field_number>\n");
        fprintf (output_lbl_file, "\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf (output_lbl_file, "\t\t\t<description>U coordinate of the UV Texture Mapping.</description>\n");
        fprintf (output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t\t<name>Texture Mapping Coordinate V</name>\n");
        fprintf (output_lbl_file, "\t\t\t<field_number>3</field_number>\n");
        fprintf (output_lbl_file, "\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf (output_lbl_file, "\t\t\t<description>V Coordinate of the UV Texture Mapping.</description>\n");
        fprintf (output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf (output_lbl_file, "\t</Record_Delimited>\n");
        fprintf (output_lbl_file, "</Table_Delimited>\n");
    }

    //write faces
    if (output_iv_file != NULL)
        fprintf(output_iv_file, "IndexedTriangleStripSet {coordIndex [\n");
    // Get the current position in the output file for PDS-4 label
    lbl_offset = ftell(tfile);
    unsigned long tc = 0;
    for (t = tm->tri_list; t; t = t->next) {
        NodeSpec *ns1 = t->get_vertex_1();
        if (ns1 == NULL)        // skip if clipped
            continue;
        NodeSpec *ns2, *ns3;
        int flip_tris = 0;  //confirm that we do NOT need to flip
        if (flip_tris) {
            ns2 = t->get_vertex_3();
            ns3 = t->get_vertex_2();
        } else {
            ns2 = t->get_vertex_2();
            ns3 = t->get_vertex_3();
        }
        if (write_out_normals) { 
            // vertex coordinate indices saved in nodespec id field
            fprintf(tfile,"f \t%d/%ld/%d\t%d/%ld/%d\t%d/%ld/%d\r\n", 
                    ns1->id, tc+1, ns1->id, 
                    ns2->id, tc+2, ns2->id, 
                    ns3->id, tc+3, ns3->id);
        } else {
            // vertex coordinate indices saved in nodespec id field
            if (output_iv_file != NULL) {
                fprintf(output_iv_file,"%d,%d,%d,", (ns1->id)-1, (ns2->id)-1, (ns3->id)-1);
                fprintf(output_iv_file, "-1,\n");   // end of strip
            }
            fprintf(tfile,"f \t%d/%ld\t%d/%ld\t%d/%ld\r\n", 
                    ns1->id, tc+1,  
                    ns2->id, tc+2,
                    ns3->id, tc+3);
        }
        tc+=3;
    } //end of write out faces
    if (output_iv_file != NULL) {
        fprintf(output_iv_file, "]}\n");
        fprintf(output_iv_file, "}\n");
        fprintf(output_iv_file, "}\n");
        //fprintf(output_iv_file, "}\n");
    }

    // only write out PDS tables if there is data
    if (count_vertices > 0 && output_lbl_file != NULL) {
        //write out PDS-4 Label Fragment for Faces Table
        fprintf(output_lbl_file, "<Table_Delimited>\n");
        fprintf(output_lbl_file, "\t<name>Faces Table</name>\n");
        fprintf(output_lbl_file, "\t<offset unit=\"byte\">%lu</offset>\n", lbl_offset);
        fprintf(output_lbl_file, "\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\t");
        fprintf(output_lbl_file, "\t<description>Table of Faces of the OBJ file</description>\n");
        fprintf(output_lbl_file, "\t<records>%ld</records>\n", (tc-1)/3);
        fprintf(output_lbl_file, "\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf(output_lbl_file, "\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf(output_lbl_file, "\t<Record_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<fields>4</fields>\n");
        fprintf(output_lbl_file, "\t\t<groups>0</groups>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>OBJ Datatype</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>1</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Face Element</description>\n");
        fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>Index 1</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>2</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Index of the first vertex of the face.</description>\n");
        fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>Index 2</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>3</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Index of the second vertex of the face.</description>\n");
        fprintf(output_lbl_file, "\t\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>Index 3</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>4</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Index of the third vertex of the face.</description>\n");
        fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t</Record_Delimited>\n");
        fprintf(output_lbl_file, "</Table_Delimited>\n");
    }
      //do clean up
      while (cnt_lod > 1) {
        delete float_image[cnt_lod];
        cnt_lod--;
      }
      if (num_lod != 0) {
        remove(output_obj_name_lod_str);
        remove(output_mtl_name_lod_str);
      }
    } // end of num_lod loop

    float_image[0]->free();
    //close all files now
    //fclose(output_lbl_file);
    //fclose(tfile);
    //take it outside the loop
    if (output_iv_file != NULL)
        fprintf(output_iv_file, "}\n");
        fclose(output_iv_file);
} 

bool convertWorldToObjFrame(double* world_pt, double* pt_offset, 
                            double rot[3][3], double* cm_pt) {
    //translate first
    double tmp[3];
    sub3(world_pt, pt_offset, tmp);
    //rotate now
    mult133(tmp, rot, cm_pt);
    //points behind camera frame are bogus
    if (cm_pt[0] < 0.0)
        return false;

    return true;
}
bool convertObjToWorldFrame(double* cm_pt, double* pt_offset, 
                            double rot[3][3], double* world_pt) {
  // char msg[256];
    double rot_t[3][3];
    trans33(rot, rot_t);
    double tmp[3];
    //rotate first
    mult133(cm_pt, rot_t, tmp);
    //translate now
    add3(tmp, pt_offset, world_pt);

    return true;
}
PigPoint convertWorldToObjFrame(PigPoint* world_pt, PigPoint* pt_offset, 
                                double rot[3][3]) {

         //translate first
         PigPoint *xyz = world_pt;
         *xyz -= *pt_offset;
         double xyz_in[3];
         xyz->getXYZ(xyz_in);
         //rotate now
         double xyz_out[3];
         mult133(xyz_in, rot, xyz_out);
         //points behind camera frame are bogus
         if (xyz_out[0] < 0.0)
             return PigPoint();
         return PigPoint(xyz_out);
}

// Input camera_model is in World Frame
void create_camera_centric_frame(PigCAHV* camera_model, double r[3][3]) {
    PigPoint point_C;
    PigVector vector_A;
    PigVector vector_H;
    PigVector vector_V;
    PigVector vector_H_proj;
    PigVector vector_Temp;
    PigVector vector_Z;
    // char msg[256];
    
    camera_model->getCurrentCAHV(point_C, vector_A, vector_H, vector_V);
    // normalize A, make sure it's not zero -> new X (forward)
    vector_A.normalize();
    // project H onto image plane -> new Y (left)
    // dot product of normalized A and H
    double ah_dot_product = vector_A%vector_H;
    vector_Temp = vector_A*ah_dot_product;
    vector_H_proj = vector_Temp - vector_H;
    vector_H_proj.normalize();
    
    vector_Z = vector_A*vector_H_proj;
    
    // now construct transform matrix
    // double r_t[3][3];
    r[0][0] = vector_A.getX();
    r[1][0] = vector_A.getY();
    r[2][0] = vector_A.getZ();

    r[0][1] = vector_H_proj.getX();
    r[1][1] = vector_H_proj.getY();
    r[2][1] = vector_H_proj.getZ();

    r[0][2] = vector_Z.getX();
    r[1][2] = vector_Z.getY();
    r[2][2] = vector_Z.getZ();


    //transform matrix
    //double roll = atan2(r[2][1], r[2][2]);
    //double pitch = atan2(-1.0*r[2][0], sqrt(r[0][0]*r[0][0]+r[1][0]*r[1][0]));
    //double yaw = atan2(r[1][0], r[0][0]);

    //Use transform matrix to get Euler Angles
    double roll = atan2(r[1][2], r[2][2]);
    double pitch = atan2(-1.0*r[0][2], sqrt(r[0][0]*r[0][0]+r[0][1]*r[0][1]));
    double yaw = atan2(r[0][1], r[0][0]);


    //sprintf(msg, "roll=%lf, pitch=%lf, yaw=%lf\n", 
    //        roll*180/PI, pitch*180/PI, yaw*180/PI);
    //zvmessage(msg, "");
 
    PigQuaternion quat(1.0, 0.0, 0.0, 0.0);
    quat.setEulerAngles(roll, pitch, yaw);
    double v[4];
    quat.getComponents(v);

    PigPoint final_point(0.0, 0.0, 0.0);
    PigQuaternion initial_orientation(1.0, 0.0, 0.0, 0.0);
    camera_model->moveCamera(point_C, initial_orientation, 
                             final_point, quat, camera_model->getCoordSystem());
    PigPoint ccc;
    PigVector aaa, hhh, vvv;
    camera_model->getCurrentCAHV(ccc, aaa, hhh, vvv);
    
    return;
}

// Compute normal for one triangle in XYZ image
// (2-1-3 = counterclockwise corner in left-handed image), accumulate
// for average cell normal. Return 1 if okay (all points are valid)
/*
static int add_one_normal(SimpleImage<float> *image, int x1, int y1, int x2,
                                int y2, int x3, int y3, int valid, double *norm)
{
        // Minimum cosine of angle between normal vectors. Smaller cosine
        // (larger angle) indicates a discontinuity.
        double min_cos_norm = 0.5;       // default = 60 degrees

        double p1[3], p2[3], p3[3], tnorm[3];

        if (!getPoint3D(image, y1, x1, p1) || !getPoint3D(image, y2, x2, p2) 
            || !getPoint3D(image, y3, x3, p3))
                return 0;
        surface_normal(p1, p3, p2, tnorm);

        if (!valid) {           // first normal for this cell
                memcpy(norm, tnorm, sizeof(tnorm));

        // Accumulate, unless apparently a discontinuity.
        } else if (fabs(dot_product(tnorm, norm)) > min_cos_norm) {
                vector_sum(norm, tnorm, norm);
        }

        return 1;
}
*/

// Estimate surface normal for XYZ image at cell x, y by averaging
// normals of the 4 triangles using that cell (or fewer, at the edges
// and adjacent to invalid points).
/*
void range_normal(SimpleImage<float> *image, int x, int y, int xres, int yres, double *norm)
{
        int valid = 0;                  // found a valid normal yet?

        if (x > 0 && y > 0)             // upper left triangle
            valid += add_one_normal(image, x, y, x-1, y, x, y-1, valid, norm);
        if (x+1 < xres && y > 0)        // upper right triangle
            valid += add_one_normal(image, x, y, x, y-1, x+1, y, valid, norm);
        if (x > 0 && y+1 < yres)        // lower left triangle
            valid += add_one_normal(image, x, y, x, y+1, x-1, y, valid, norm);
        if (x+1 < xres && y+1 < yres)   // lower right triangle
            valid += add_one_normal(image, x, y, x+1, y, x, y+1, valid, norm);

        if (valid) {            // normalize accumulated result
                normalize_vector(norm);
        } else {                // none valid, set default vertical normal
                norm[0] = norm[1] = 0.0;
                norm[2] = 1.0;
        }
}
*/

void make_LODs(FILE *fp, Range *bvol) {

    int num_tris;
    int i, lod;
    int lod_res;    // X resolution at highest LOD

    //ozp! fix it
    int maxres = 1024;
    int MAX_LOD = 20;
    int verbose = 1;
    int maxgap = 2;
    int minres = 200;
    // read in XYZ image
    SimpleImage<float> *float_image[0];


    //!!!!ozp
    fprintf(fp, "LevelOfDetail {\n");
    fprintf(fp, "screenArea [ 256000, 54000, 16000, 4000, 1000 ]\n");


    // create simple mesh for this tile?
    //int simple_mesh = npatch==1 || no_merge;
    int simple_mesh = 1;

    int LODres = maxres;    // estimated resolution at this LOD

    for (lod=0; lod<MAX_LOD; lod++) {
        if (verbose)
            fprintf(stderr, "Building LOD %d\n", lod);
        //track_memory("start LOD");

        // Create octree for merging data. If only one patch overlaps
        // this tile, a connectivity mesh is built into the sfcmodel
        // mesh member. Otherwise, a separate Marching Triangles mesh
        // will be built.
        //SfcModel *sfc = setup_octree(bvol, LODres);
        //!!!! ozp 
        SfcModel *sfc = new SfcModel(LODres);

        //PInfo *pi = plist;
        int npatch = 1;
        //for (i=0; i<npatch; i++, pi++) {
        for (i=0; i<npatch; i++) {
            if (lod == 0) { // first LOD (highest res)
                //pi->xyz = get_patch_xyz(pi->p, simple_mesh);
                //lod_res = pi->xyz->get_res();
                //ozp
                lod_res = 1024;

                // transform XYZ's to site frame for clipping
                //!!!!ozp xform_world(pi);

                // clip data to tile boundary + margin
                // ** possible optimization TBD:
                // ** if no points survive clipping, remove
                // ** patch from this tile (or just delete xyz)
                //!!!! ozp clip_patch(pi->xyz, bvol);
           } else if (true /*pi->xyz*/) {  // not first LOD
                //pi->xyz = rng_resample(pi->xyz);
            }
            if (false /*pi->xyz == NULL*/)    // too small, failure, etc.
                continue;
            // resample range data for this LOD
            //if (maxgap)
                //summitt_range_interpolate(pi->xyz, maxgap);
             //   image_range_interpolate(float_image[0], maxgap);
            // merge to octree, optionally create mesh
            //!!!! ozp merge_points(sfc, i, simple_mesh);
        //}
        } //end of for (i=0.....

        //!!!! ozp -testing
        //if (verbose == 2 && npatch > 1) { // dump octree
        //if (true) { // dump octree
        //    static int octid;
        //    char buf[32];
        //    sprintf(buf, "tm%04d.oct", ++octid);
        //    FILE_Dataport fp;
        //    fp.wopen(buf);
        //    ((Octree *)sfc)->parse_out(&fp);
        //    fp.close();
        //}

        // extract point list (for printing simple mesh,
        // or for marching tris)
        if (sfc->get_data())
            sfc->add_pt_list();

        //!!!! ozp testing
        Triangle_Model *tm;              // triangle mesh
        tm = &(sfc->mesh);
        NodeSpec_List_Element *nle;
        nle = tm->pt_list;
        double gc[3];
        nle->nodespec->get_global_center(gc);

        Triangle_Model *mesh;
        if (simple_mesh)    // use simple mesh in sfcmodel
            mesh = &(sfc->mesh);
        //else            // build mesh from merged points
        //    mesh = merged_mesh(sfc);

        num_tris = mesh ? mesh->tri_count() : 0;
        if (lod > 0 && num_tris < minres/2) {
            if (verbose)
                fprintf(stderr, "Discarding level, "
                    "only %d triangles\n", num_tris);
            lod--;
            if (mesh)
                mesh->clean_up();
            delete sfc;
            break;
        }

        // don't output bogus LOD below top level
        if (verbose)
            fprintf(stderr, "Writing mesh, "
                "%d triangles before clipping\n", num_tris);
        //!!!! ozp fprintf(fp, "Separator { # LevelOfDetail %d\n", lod);
        // output mesh; flip Marching triangles if +Z is down
        if (mesh && mesh->tri_list)
            //!!!! ozp write_mesh(fp, mesh, sfc, bvol, !simple_mesh && zdown);
            int test = 1;   //!!!! ozp just to have proper if statement
        //!!!! ozp fprintf(fp, "}\n"); // end of this level
      if (mesh)
            mesh->clean_up();
        delete sfc;

        // estimate resolution for next level
        LODres = num_tris / 2;
        // reached desired coarsest LOD yet?
        if (LODres < minres)
            break;
    //}
    } // end of for (lod=0....

        //!!!!ozp
    // go back and write range field at start of LOD node
    //if (fseek(fp, range_pos, SEEK_SET) < 0)
    //  perror("tilemesh seek failed");
    //fprintf(fp, "%-*s]\n", RANGE_LEN, range_field);
    //fseek(fp, 0, SEEK_END);
    //!!!! ozp fprintf(fp, "}\n"); // finish LOD node

    // clean up
    //for (i=0; i<npatch; i++)
    //    delete plist[i].xyz;
    //track_memory("Tile done");
}
