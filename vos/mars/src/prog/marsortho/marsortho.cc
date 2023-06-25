////////////////////////////////////////////////////////////////////////////////
//marsortho.cc
//
//Generates Orthographic Projection mosaic. Either OrthoPhoto output
//is generated using intensity/color associated with a given XYZ point, or DEM
//is generated using Z-coord of a given XYZ point.  1-to-1 correspondence is 
//expected between SKIN List and XYZ List.  Moreover, every entry in Image list 
//should be geometrically registered to XYZ entry.
////////////////////////////////////////////////////////////////////////////////

#include "vicmain_c"
#include "mars_support.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigPointingCorrections.h"
#include "RadiometryModel.h"
#include "PigBrtCorrModel.h"
#include "PigBrtCorrLinear.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include "SimpleImage.h"
#include "PigRoverStateManager.h"
#include "zvproto.h"
#include <stdlib.h>
#include <iostream>
using namespace std;

#define MAX_INPUTS 2000
#define MAX_NS 0
#define MAX_NL 0
#define MAXFLOAT 3.40282E+38
#define X_BAND 1
#define Y_BAND 2
#define Z_BAND 3
#define PI 3.14159265
#define SPAN_NODATA -1.0

typedef enum {
    CLOSEST,
    AVG,
    MIN,
    MAX,
    FIRST,
    LAST
} OverlayMode;

OverlayMode getOverlayMode();

void env_setup(int &nids, PigFileModel *files[], PigCameraModel *cameras[],
               PigPointingModel *pointings[], PigSurfaceModel *&surface_model,
               RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr_models[],
               PigCoordSystem *&def_cs, char *mission, char *instrument,
               int &homogeneous_inputs, const int max_nl, const int max_ns,
               const int max_inputs);
void initializeSimpleImage(SimpleImage<float> *image, double nodata);
void initializeSimpleImage(SimpleImage<short int> *image, short int nodata);
void doOrrProjOverlayFirstAndLast(SimpleImage<float> *geometry_x, 
               SimpleImage<float> *geometry_y, SimpleImage<float> *geometry_z,
               SimpleImage<float> *skin_image, SimpleImage<float> *global_image,
               PigPointingModel *pointing_in, PigCameraModel *camera_in, 
               PigCoordSystem *cs, PigCoordSystem *z_cs,
               PigFileModel *xyz_file_model, PigMission *mission_instance, 
               int fill_size, double nodata, double range_cut, double minx, 
               double miny, double scale, int nlo, int nso, double erode_factor,
               OverlayMode overlay_mode, int do_dem, int do_circle, 
               int do_erosion, int do_z_coord, SimpleImage<short int> *idx_out,
               int img_idx, int do_idx, SimpleImage<short int> *icm_line_out,
               SimpleImage<short int> *icm_samp_out, int do_icm, 
               int do_camera_center, double camera_center[], int do_pixel_angle,
               double pixel_angle, short int do_elongated_fill);
void doOrrProj(SimpleImage<float> *geometry_x, SimpleImage<float> *geometry_y,
               SimpleImage<float> *geometry_z, SimpleImage<float> *skin_image,
               SimpleImage<float> *global_image, SimpleImage<float> *quality_matrix,
               SimpleImage<float> *range_matrix, SimpleImage<float> *span_matrix,
               SimpleImage<float> *compare_matrix,
               PigPointingModel *pointing_in, PigCameraModel *camera_in,
               PigCoordSystem *cs, PigCoordSystem *z_cs,
               PigFileModel *xyz_file_model, PigMission *mission_instance, 
               int fill_size, double nodata, double range_cut, double minx, 
               double miny, double scale, int nlo, int nso, 
               OverlayMode overlay_mode, int do_dem, int do_circle, 
               int do_z_coord, SimpleImage<short int> *idx_out, int img_idx, 
               int do_idx, SimpleImage<short int> *icm_line_out, 
               SimpleImage<short int> *icm_samp_out, int do_icm,
               int do_camera_center, double camera_center[], int do_pixel_angle,
               double pixel_angle, short int do_elongated_fill);
int getOutFileUnit(char *output_parameter);
void negateDEM(SimpleImage<float> *global_dem, double dem_nodata);
void writeOutput(SimpleImage<float> *image_result, int out_unit, char *format, 
               int nids, int nlo, int nso, int current_band, int total_band, 
               double scale, double maxx, double maxy, PigCoordSystem *cs, 
               PigFileModel *file_models[], PigMission *mission_instance,
               RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr[], 
               int is_dem, int do_zup, PigCoordSystem *write_cs, 
               double missing_constant, PigCoordSystem *z_cs, 
               SimpleImage<short int> *idx_out, int idx_out_unit, int do_idx, 
               SimpleImage<short int> *icm_line_out,
               SimpleImage<short int> *icm_samp_out, int icm_out_unit, 
               int do_icm);
void avgModeHandler(SimpleImage<float> *image, SimpleImage<float> *image_qm,
               SimpleImage<float> *image_sp, int xpos, int ypos, double value, 
               int span_line, int span_samp, double span_angle, double nodata, 
               int nlo, int nso, short int do_elongated_fill);
void minModeHandler(SimpleImage<float> *image, SimpleImage<float> *image_qm,
               SimpleImage<float> *image_sp, int xpos, int ypos, 
               double value_compare, double value_set, int span_line, 
               int span_samp, double span_angle, double nodata, 
               int nlo, int nso, SimpleImage<short int> *idx_out, int img_idx, 
               int do_idx, SimpleImage<short int> *icm_line_out, 
               SimpleImage<short int> *icm_samp_out, int do_icm, int line, 
               int samp, SimpleImage<float> *image_cp, 
               short int do_elongated_fill);
void maxModeHandler(SimpleImage<float> *image, SimpleImage<float> *image_qm,
               SimpleImage<float> *image_sp, int xpos, int ypos, 
               double value_compare, double value_set, int span_line, 
               int span_samp, double span_angle, double nodata, 
               int nlo, int nso, SimpleImage<short int> *idx_out, int img_idx, 
               int do_idx, SimpleImage<short int> *icm_line_out, 
               SimpleImage<short int> *icm_samp_out, int do_icm, int line, 
               int samp, SimpleImage<float> *image_cp, 
               short int do_elongated_fill);
void firstAndLastModesHandler(SimpleImage<float> *image, 
               SimpleImage<float> *image_qm, SimpleImage<float> *image_sp, 
               int xpos, int ypos, double value, int span_line, int span_samp, 
               double span_angle, double nodata, int nlo, int nso, 
               SimpleImage<short int> *idx_out, int img_idx, int do_idx, 
               SimpleImage<short int> *icm_line_out, 
               SimpleImage<short int> *icm_samp_out, int do_icm, int line,
               int samp, short int do_elongated_fill);
void closestModeHandler(SimpleImage<float> *image, SimpleImage<float> *image_qm,
               SimpleImage<float> *image_rm, SimpleImage<float> *image_sp, 
               int xpos, int ypos, double value, double range, int span_line,
               int span_samp, double span_angle,  double nodata, int nlo, 
               int nso, SimpleImage<short int> *idx_out, int img_idx, 
               int do_idx, SimpleImage<short int> *icm_line_out, 
               SimpleImage<short int> *icm_samp_out, int do_icm, int line, 
               int samp, short int do_elongated_fill);
void square_fill(SimpleImage<float> *image, SimpleImage<float> *image_qm,
                 SimpleImage<float> *image_sp, int xpos, int ypos,
                 double fill_value, double nodata, int span, int nlo, int nso,
                 SimpleImage<short int> *idx_out, int img_idx, int do_idx,
                 SimpleImage<short int> *icm_line_out,
                 SimpleImage<short int> *icm_samp_out, int do_icm, int line,
                 int samp, SimpleImage<float> *image_cp, double new_cp);
void elongated_fill(SimpleImage<float> *image, SimpleImage<float> *image_qm,
                    SimpleImage<float> *image_sp, int xpos, int ypos,
                    double fill_value, double nodata, int span_line,
                    int span_samp, double span_angle, int nlo, int nso,
                    SimpleImage<short int> *idx_out, int img_idx, int do_idx,
                    SimpleImage<short int> *icm_line_out,
                    SimpleImage<short int> *icm_samp_out, int do_icm, int line,
                    int samp, SimpleImage<float> *image_cp, double new_cp);
void square_fill_closest(SimpleImage<float> *image,
                         SimpleImage<float> *image_qm,
                         SimpleImage<float> *image_rm,
                         SimpleImage<float> *image_sp,
                         int xpos, int ypos, double fill_value, double range,
                         double nodata, int span, int nlo, int nso,
                         SimpleImage<short int> *idx_out, int img_idx, int do_idx,
                         SimpleImage<short int> *icm_line_out,
                         SimpleImage<short int> *icm_samp_out, int do_icm,
                         int line, int samp);
void elongated_fill_closest(SimpleImage<float> *image,
                            SimpleImage<float> *image_qm,
                            SimpleImage<float> *image_rm,
                            SimpleImage<float> *image_sp,
                            int xpos, int ypos, double fill_value, double range,
                            double nodata, int span_line, int span_samp,
                            double span_angle, int nlo, int nso,
                            SimpleImage<short int> *idx_out, int img_idx,
                            int do_idx, SimpleImage<short int> *icm_line_out,
                            SimpleImage<short int> *icm_samp_out, int do_icm,
                            int line, int samp);
SimpleImage<short int> *genStructuringElement(int stru_elem_size, int nlo, int nso,
                                              float span_line, float span_samp,
                                              short int do_elongated_fill);
SimpleImage<float> *erosion(SimpleImage<float> *image,
                            SimpleImage<float> *span_matrix, double nodata, 
                            double erode_factori, short int do_elongated_fill);
int isContained(SimpleImage<float> *image, SimpleImage<short int> *stru_elem,
                int center_nl, int center_ns, int offset_width, int offset_height, 
                int nlo, int nso, double nodata);
void printErosionMessage(double erode_factor);
const char *enumToString(OverlayMode overlay_mode);
float calculateWeightedSpan(double value_x, double value_y, double value_z, 
                            double camera_position[], double pixel_angle, 
                            double scale, double max_angle, double max_span);
float calculateLineSpan(double value_x, double value_y, double value_z,
                        double camera_position[], double pixel_angle,
                        double scale, double max_angle, double max_span);
float calculateSampleSpan(double value_x, double value_y, double value_z,
                          double camera_position[], double pixel_angle,
                          double scale, double max_angle, double max_span);

//Global variable
int omp_on;

void main44()
{
    zvmessage("MARSORTHO version 1.14.0\n", "");

    // turn off vicar primary input mechanism
    zvselpi(0);

    //define commonly used variables
    const size_t msgLen = 256;
    char msg[msgLen];
    int count = 0;
    int def = 0; 
    int status = 0;

    //1. Check parameters INP and OUT.
    //2. Check parameters IN_XYZ and OUT_DEM.
    int out_dem_count = 0, out_count = 0;
    int inp_count = 0, xyz_count = 0;
    int do_dem = FALSE, do_ortho = FALSE;

    zvpcnt("OUT", &out_count);
    zvpcnt("INP", &inp_count);
    if (out_count == 1 && inp_count > 0) {
        do_ortho = TRUE;
    }

    zvpcnt("OUT_DEM", &out_dem_count);
    zvpcnt("IN_XYZ", &xyz_count);
    if (out_dem_count == 1 && xyz_count > 0) {
        do_dem = TRUE;
    }

    if (out_dem_count == 0 && out_count == 0) {
        zvmessage("Output parameters are not defined.", "");
        zabend();
    }

    omp_on = zvptst("OMP_ON");

    //mission setup
    PigFileModel *xyz_file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface_model = NULL;
    PigCoordSystem *cs = NULL;
    char mission[64], instrument[64];
    int xyz_nids = 0;
    int homogeneous_inputs = TRUE;

    env_setup(xyz_nids, xyz_file_models, camera_in, pointing_in, surface_model,
              NULL, NULL, cs, mission, instrument, homogeneous_inputs, MAX_NL,
              MAX_NS, MAX_INPUTS);
    PigMission *mission_instance = PigMission::getMissionObject(mission);

    //Find out the INP data format. We check the data type of the first input
    //and assume that ALL inputs have the same data type. 
    char xyz_format[8];
    xyz_file_models[0]->openFile();

    int xyz_band = xyz_file_models[0]->getNB();
    if (xyz_band != 3) {
    	zvmessage("IN_XYZ band error. IN_XYZ must be 3 band.", "");
    	zabend();
    }

    status = zvget(xyz_file_models[0]->getUnit(), "FORMAT", xyz_format, NULL);
    if (status == 1) {
        if ((strcmp(xyz_format, "HALF") == 0) ||
            (strcmp(xyz_format, "BYTE") == 0)) {
            strcpy(xyz_format, "HALF");
        } else {
            strcpy(xyz_format, "REAL");
        }
    } else {
        zvmessage("Unable to determine IN_XYZ data format", "");
        zabend();
    }

    xyz_file_models[0]->closeFile();

    //Find out the SKIN data format if defined. We check the data type of the
    //first input and assume that ALL inputs have the same data type.
    PigFileModel *skin_file_models[MAX_INPUTS];
    char skin_format[10];
    char **skin_filenames = new char *[MAX_INPUTS];
    int skin_nids = 0;
    int short_int_data = 1; 

    if (do_ortho) {
    	mars_get_filelist("INP", skin_nids, skin_filenames, MAX_INPUTS, FALSE);

    	if (skin_nids != xyz_nids) {
            zvmessage("IMG and XYZ lists need to have 1-to-1 relationship.", "");
            zabend();
        }

        for (int i = 0; i < skin_nids; i++) {
            skin_file_models[i] = PigFileModel::create(skin_filenames[i]);
        }

        if (skin_file_models[0] == NULL) {
            zvmessage("Unable to create IMG file model.", "");
            zabend();
        }

        skin_file_models[0] -> openFile();
        status = zvget(skin_file_models[0]->getUnit(), "FORMAT", skin_format, NULL);
        if (status == 1) {
    	    if (strcmp(skin_format, "REAL") == 0) {
                short_int_data = 0;
            }
    	} else {
    	    zvmessage("Unable to determine INP data format", "");
            zabend();
    	}
        skin_file_models[0]->closeFile();
    }

    //Find out if MINX, MINY, MAXX, MAXY are defined. If so, compute extent
    //based on them. Otherwise, compute extent based on parameter NL. If all of
    //them are not given, then throw error message.
    double minx = 0.0;
    double miny = 0.0;
    double maxx = 0.0;
    double maxy = 0.0;
    double scale = 0.0;
    double scale_thresh = 0.0;
    double range_cut = 0.0;
    double global_scale = 0.0;
    double global_minx = MAXFLOAT;
    double global_maxx = -MAXFLOAT;
    double global_miny = MAXFLOAT;
    double global_maxy = -MAXFLOAT;
    double range_max = 0.0;
    short int compute_extent = FALSE;
    int nlo = 0, nso = 0;
    int nl_thresh = 0;
    int do_circle = zvptst("CIRCLE");	// false == SAWTOOTH
    int do_erosion = zvptst("EROSION");
    int do_zup = zvptst("ZUP");           
              
    SimpleImage<float> *image_x_band[MAX_INPUTS];
    SimpleImage<float> *image_y_band[MAX_INPUTS];
    SimpleImage<float> *image_z_band[MAX_INPUTS];
    PigCoordSystem *xyz_cs = NULL;

    zvparmd("RANGE", &range_max, &count, &def, 1, 0);
    if (range_max < 0.0) {
        zvmessage("RANGE cannot be smaller than 0.", "");
        zabend();
    }

    zvparmd("CUT", &range_cut, &count, &def, 1, 0);
    if (count == 0) {
        range_cut = range_max;
    }

    double camera_center[3];
    double camera_position[3];
    int do_camera_center = FALSE;
    zvparmd("CAMERA_CENTER", camera_center, &count, &def, 3, 0);
    if (count == 3) {
        do_camera_center = TRUE;
        snprintf(msg, msgLen, "Camera Center OVERRIDE to %f, %f, %f", camera_center[0],
                camera_center[1], camera_center[2]);
        zvmessage(msg, "");
    }

    double pixel_angle;
    int do_pixel_angle = FALSE;
    zvparmd("PIXEL_ANGLE", &pixel_angle, &count, &def, 1, 0);
    if (count == 1) {
        do_pixel_angle = TRUE;
        snprintf(msg, msgLen, "Pixel Angle OVERRIDE to %f", pixel_angle);
        zvmessage(msg, "");
    }

    memset(image_x_band, 0, sizeof(image_x_band)); //init to NULL
    memset(image_y_band, 0, sizeof(image_y_band)); //init to NULL
    memset(image_z_band, 0, sizeof(image_z_band)); //init to NULL

    zvmessage("The program is computing output geometry ...", "");
    for (int iter = 0; iter < xyz_nids; iter++) {
        int file_nl = xyz_file_models[iter]->getNL();
        int file_ns = xyz_file_models[iter]->getNS();

        mars_read_inputs(iter, iter, xyz_file_models, image_x_band, MAX_NL,
                         MAX_NS, X_BAND, NULL, NULL);
        mars_read_inputs(iter, iter, xyz_file_models, image_y_band, MAX_NL,
                         MAX_NS, Y_BAND, NULL, NULL);
        mars_read_inputs(iter, iter, xyz_file_models, image_z_band, MAX_NL,
                         MAX_NS, Z_BAND, NULL, NULL);          

        PigCSReference *ref = NULL;
        xyz_file_models[iter]->getDerivedImageCS(ref);
        xyz_cs = mission_instance->getCoordSystem(ref);

        PigPoint range_origin;
        if (do_camera_center) {
            camera_position[0] = camera_center[0];
            camera_position[1] = camera_center[1];
            camera_position[2] = camera_center[2];
        } else {
            if (pointing_in[iter] != NULL) {
                range_origin = pointing_in[iter]->getCameraPosition(cs);
                range_origin.getXYZ(camera_position);
            } else {
                zvmessage("Camera model not found in input files; CAMERA_CENTER "
                          "parameter required", "");
                zabend();
            }
        }      

        double diff_x, diff_y, diff_z;
        double value_x, value_y, value_z;
        double range;    //3d distance from CS origin to XYZ point
        int inp_nl = image_x_band[0]->getNL();
        int inp_ns = image_x_band[0]->getNS();

        for (int line = 0; line < inp_nl; line++) {
            for (int samp = 0; samp < inp_ns; samp++) {
                value_x = image_x_band[0]->get(line, samp);
                value_y = image_y_band[0]->get(line, samp);
                value_z = image_z_band[0]->get(line, samp);

                if (value_x == 0.0 && value_y == 0.0 && value_z == 0.0) {
                    continue;
                }

                //convert coordinate system if necessary
                if (cs != xyz_cs) {
                    PigPoint old_xyz(value_x, value_y, value_z);
                    if (value_x != 0.0 || value_y != 0.0 || value_z != 0.0) {
                        PigPoint new_xyz = cs->convertPoint(old_xyz, xyz_cs);
                        value_x = new_xyz.getX();
                        value_y = new_xyz.getY();
                        value_z = new_xyz.getZ();
                    }
                }

		// Compute range from camera
                diff_x = value_x - camera_position[0];
                diff_y = value_y - camera_position[1];
                diff_z = value_z - camera_position[2];
                range = sqrt(diff_x * diff_x + diff_y * diff_y + diff_z * diff_z);

		// skip if point is outside the range
		if (range_cut > 0) {
		    double range_check = range;

		    if (do_circle) {		// 2D distance from CS origin
		        range_check = sqrt(value_x * value_x + value_y * value_y);
		    }

                    if (range_check > range_cut) {
                        continue;
                    }
                } 

                if (value_x != 0.0 && value_x < global_minx) {
                    global_minx = value_x;
                }

                if (value_y != 0.0 && value_y < global_miny) {
                    global_miny = value_y;
                }

                if (value_x != 0.0 && value_x > global_maxx) {
                    global_maxx = value_x;
                }

                if (value_y != 0.0 && value_y > global_maxy) {
                    global_maxy = value_y;
                }
            }
        }

        image_x_band[0]->free();
        image_y_band[0]->free();
        image_z_band[0]->free();
    }
    
    // If any of these did not get set, the input is all 0 so we just
    // arbitrarily set the range to -1 .. +1.  We use MAXFLOAT/2 just
    // to avoid any issues with floating-point equality comparisons.

    if (global_minx >=  MAXFLOAT/2 || global_miny >=  MAXFLOAT/2 ||
	global_maxx <= -MAXFLOAT/2 || global_maxx <= -MAXFLOAT/2) {

	global_minx = -1.0;
	global_miny = -1.0;
	global_maxx = 1.0;
	global_maxy = 1.0;
    }


    if (range_max > 0) {
	snprintf(msg,msgLen,"Computed size: X=%f,%f Y=%f,%f", global_minx, global_maxx,
                global_miny, global_maxy);
	zvmessage(msg,"");
        snprintf(msg, msgLen, "Overriding size based on range_max to %f meters", range_max);
	zvmessage(msg, "");
        global_minx = -range_max;
        global_miny = -range_max;
        global_maxx = range_max;
        global_maxy = range_max;
    }

    zvparmd("MINX", &minx, &count, &def, 1, 0);
    if (count == 0) {
        minx = global_minx;
        snprintf(msg, msgLen, "MINX is computed from XYZ data. MINX=%f.", minx);
        zvmessage(msg, "");
    } else {
        snprintf(msg, msgLen, "MINX is provided in the command. MINX=%f.", minx);
        zvmessage(msg, "");
    }
    zvparmd("MAXX", &maxx, &count, &def, 1, 0);
    if (count == 0) {
        maxx = global_maxx;
        snprintf(msg, msgLen, "MAXX is computed from XYZ data. MAXX=%f.", maxx);
        zvmessage(msg, "");
    } else {
        snprintf(msg, msgLen, "MAXX is provided in the command. MAXX=%f", maxx);
        zvmessage(msg, "");
    }
    zvparmd("MINY", &miny, &count, &def, 1, 0);
    if (count == 0) {
        miny = global_miny;
        snprintf(msg, msgLen, "MINY is computed from XYZ data. MINY=%f.", miny);
        zvmessage(msg, "");
    } else {
        snprintf(msg, msgLen, "MINY is provided in the command. MINY=%f.", miny);
        zvmessage(msg, "");
    }
    zvparmd("MAXY", &maxy, &count, &def, 1, 0);
    if (count == 0) {
        maxy = global_maxy;
        snprintf(msg, msgLen, "MAXY is computed from XYZ data. MAXY=%f.", maxy);
        zvmessage(msg, "");
    } else {
        snprintf(msg, msgLen, "MAXY is provided in the command. MAXY=%f.", maxy);
        zvmessage(msg, "");
    }
    
    zvparmd("NL", &nlo, &count, &def, 1, 0);
    if (count == 1) {
        snprintf(msg, msgLen, "NL is provided in the command. NL=%d.", nlo);
        zvmessage(msg, "");

        global_scale = fabs(nlo / (maxx - minx));
        zvparmd("SCALE", &scale, &count, &def, 1, 0);
        if (count == 1) {
            maxx = minx + nlo * scale;
            scale = 1.0 / scale;  //convert meter per pixel to pixle per meter
            snprintf(msg, msgLen, "SCALE is provided in the command. SCALE=%f.", 1.0 / scale);
            zvmessage(msg, "");
            snprintf(msg, msgLen, "Both NL and SCALE are provided in the command, so"
             " MAXX will be re-computed based on NL and SCALE. MAXX=%f", maxx); 
            zvmessage(msg, "");
        } else {
            scale = global_scale;
            snprintf(msg, msgLen, "SCALE is computed based on MINX, MAXX, and NL."
                    " SCALE=%f.", 1.0 / scale);
            zvmessage(msg, "");
            zvparmd("SCALE_THRESH", &scale_thresh, &count, &def, 1, 0);
            if ((count == 1) && scale < 1.0 / scale_thresh) {
                scale = 1.0 / scale_thresh;
                snprintf(msg, msgLen, "SCALE is exceeding maximum allowed value, override to maximum allowed value:"
                             " SCALE=%f.", 1.0 / scale);
                zvmessage(msg, "");
                // recomputing NL
                nlo = (int)((maxx - minx) * scale + 0.5);
                zvparmd("NL_THRESH", &nl_thresh, &count, &def, 1, 0);
                if ((count == 1) && (nlo > nl_thresh))
                    nlo = nl_thresh;
                snprintf(msg, msgLen, "NL is recomputed based on new SCALE value and maximum allowed value if specified: NL=%d", nlo);
                zvmessage(msg, "");
            }
          }
        nso = (int)((maxy - miny) * scale + 0.5);
        snprintf(msg, msgLen, "NS is computed based on MINY, MAXY, and SCALE. NS=%d", nso);
        zvmessage(msg, "");
    } else {
        zvparmd("SCALE", &scale, &count, &def, 1, 0);
        if (count == 1) {
            scale = 1.0 / scale;
            snprintf(msg, msgLen, "SCALE is provided in the command. SCALE=%f.", 
                    1.0 / scale);
            zvmessage(msg, "");
            nlo = (int)((maxx - minx) * scale + 0.5);
            nso = (int)((maxy - miny) * scale + 0.5);
            snprintf(msg, msgLen, "NL is computed based on MINX, MAXX, and SCALE. NL=%d.", 
                    nlo); 
            zvmessage(msg, "");
            snprintf(msg, msgLen, "NS is computed based on MINY, MAXY, and SCALE. NS=%d.", 
                    nso);
        } else {
            zvmessage("Neither NL nor SCALE is defined. Cannot compute"
                      " output's geometry. Abort.", "");
            zabend();
        }
    }
    
    maxx = minx + nlo * (1.0 / scale);
    maxy = miny + nso * (1.0 / scale);
    snprintf(msg, msgLen, "MAXX is adjusted based on MINX, NL, and SCALE. MAXX=%f.", maxx);
    zvmessage(msg, "");
    snprintf(msg, msgLen, "MAXY is adjusted based on MINY, NS, and SCALE. MAXY=%f.", maxy);
    zvmessage(msg, "");    

    zvmessage("Final geometry: ", "");   
    snprintf(msg, msgLen, "COORD: %s", cs->getFrameName());
    zvmessage(msg, "");

    //READ z_coord
    PigCoordSystem *z_cs = NULL;
    char z_coord[256];
    int do_z_coord = FALSE;
    zvp("Z_COORD", z_coord, &count);
    if (do_dem && count > 0) {
        z_cs = mission_instance->getCoordSystem(xyz_file_models[0], z_coord);
        do_z_coord = TRUE;
        if (z_cs == NULL) {
            snprintf(msg, msgLen, "Invalid Z_COORD: %s", z_cs);
            zvmessage(msg, "");
            zabend();
        }
        snprintf(msg, msgLen, "Z_COORD: %s", z_cs->getFrameName());
        zvmessage(msg, "");
    }

    // Read write_cs
    PigCoordSystem *write_cs = NULL;
    char write_coord[256];
    zvp("WRITE_COORD", write_coord, &count);
    if (count > 0) {
	write_cs = mission_instance->getCoordSystem(xyz_file_models[0],
								write_coord);
    }

    snprintf(msg, msgLen, "MINX=%f MAXX=%f MINY=%f MAXY=%f", minx, maxx, miny, maxy);
    zvmessage(msg,"");
    snprintf(msg, msgLen, "SCALE=%f", 1.0 / scale);
    zvmessage(msg, "");
    snprintf(msg, msgLen, "Output NL: %d, Output NS: %d", nlo, nso);
    zvmessage(msg, "");
    
    //READ fill_size
    int fill_size;
    zvparmd("FILL_SIZE", &fill_size, &count, &def, 1, 0);

    //Reads INPUT_RANGE parameter
    int input_range[2];
    int min_input = 0;
    int max_input = xyz_nids;
    zvparm("INPUT_RANGE", input_range, &count, &def, 2, 0);
    if (count >= 1) {
        min_input = input_range[0] - 1; 
    }

    if (count >= 2) {
        max_input = input_range[1];
    }

    if (min_input < 0) {
        min_input = 0;
    }

    if (max_input > xyz_nids) {
        max_input = xyz_nids;
    }
    snprintf(msg, msgLen, "Images from %d to %d (inclusive) will be processed.", 
           min_input + 1, max_input);
    zvmessage(msg, "");

    //Read erode_factor
    double erode_factor = 0.0;
    zvparmd("ERODE_FACTOR", &erode_factor, &count, &def, 1, 0);

    //define common variables for OUT and OUT_DEM
    OverlayMode overlay_mode = getOverlayMode();  //overlay mode
    RadiometryModel *radiometric[MAX_INPUTS];     //rad correction mode
    PigBrtCorrModel *brt_corr[MAX_INPUTS];        //brt correction mode
    double range;                                 //range from a pixel to camera

    for (int i=0; i < MAX_INPUTS; i++) {
	radiometric[i] = NULL;
	brt_corr[i] = NULL;
    }

    // idx_out and icm_out
    zvpcnt("IDX_OUT", &count);
    SimpleImage<short int> *idx_out = NULL;
    int idx_out_unit;
    int do_idx = 0;
    if (count == 1) {
        if (overlay_mode != AVG) {
            idx_out = new SimpleImage<short int>(nlo, nso); 
            for (int line = 0; line < nlo; line++) {
                for (int samp = 0; samp < nso; samp++) {
                    idx_out->set(line, samp, 0);
                }
            }

            idx_out_unit = getOutFileUnit("IDX_OUT");
            do_idx = 1;
        } else {
            zvmessage("IDX_OUT is not available for OVERLAY mode AVG.", "");
        }
    }

    zvpcnt("ICM_OUT", &count);
    SimpleImage<short int> *icm_line_out = NULL;
    SimpleImage<short int> *icm_samp_out = NULL;
    int icm_out_unit;
    int do_icm = 0;
    if (count == 1) {
        if (overlay_mode != AVG) {
            icm_line_out = new SimpleImage<short int>(nlo, nso);
            icm_samp_out = new SimpleImage<short int>(nlo, nso);
            for (int line = 0; line < nlo; line++) {
                for (int samp = 0; samp < nso; samp++) {
                    icm_line_out->set(line, samp, 0);
                    icm_samp_out->set(line, samp, 0);
                }
            }

            icm_out_unit = getOutFileUnit("ICM_OUT");
            do_icm = 1;
        } else {
            zvmessage("ICM_OUT is not available for OVERLAY mode AVG.", "");
        }
    }

    // Figuring out if we are doing elonagated fill or square fill. If it is
    // elongated_fill, the span SimpleImage needs to be 3-band to store the
    // width, height, rotation angle for erosion.
    short int do_elongated_fill;
    if (zvptst("ELONGATED")) {
        do_elongated_fill = TRUE;
    } else {
        do_elongated_fill = FALSE;
    }


    //Generate DEM
    if (do_dem) {
        SimpleImage<float> *global_dem = NULL;     //DEM output
        SimpleImage<float> *global_dem_qm  = NULL; //DEM quality matrix
        SimpleImage<float> *global_dem_rm  = NULL; //DEM range matrix
        SimpleImage<float> *global_dem_sp  = NULL; //DEM span matrix
        SimpleImage<float> *global_dem_cp  = NULL; //DEM compare matrix
        double dem_nodata;                         //value for DEM nodata areas
        int dem_fill_size;                         //value for expand pixels in DEM
        double dem_fill_factor;                    //value for expand factor in DEM
        int index;                                 //index of images 
        int file_nl;                               //number of lines
        int file_ns;                               //number of samples

        memset(image_x_band, 0, sizeof(image_x_band)); //init to NULL
        memset(image_y_band, 0, sizeof(image_y_band)); //init to NULL
        memset(image_z_band, 0, sizeof(image_z_band)); //init to NULL

        zvparmd("DEM_NODATA", &dem_nodata, &count, &def, 1, 0);
        global_dem = new SimpleImage<float>(nlo, nso);
        initializeSimpleImage(global_dem, dem_nodata);

        //overlay mode FIRST and LAST don't need global quality matrix,
        //range maxtrix, and span maxtrix.
        if (overlay_mode != FIRST && overlay_mode != LAST) {
            global_dem_qm = new SimpleImage<float>(nlo, nso);
            global_dem_rm = new SimpleImage<float>(nlo, nso);
            
            initializeSimpleImage(global_dem_qm, MAXFLOAT);
            initializeSimpleImage(global_dem_rm, MAXFLOAT);

            if(do_elongated_fill) {
                // Store the width, height, and rotation angle in 3-bands image
                global_dem_sp = new SimpleImage<float>(3, nlo, nso);
            } else {
                global_dem_sp = new SimpleImage<float>(nlo, nso);
            }
            initializeSimpleImage(global_dem_sp, SPAN_NODATA);

            if (overlay_mode == MIN) {
                global_dem_cp = new SimpleImage<float>(nlo, nso);
                initializeSimpleImage(global_dem_cp, MAXFLOAT);
            }

            if (overlay_mode == MAX) {
               global_dem_cp = new SimpleImage<float>(nlo, nso);
               initializeSimpleImage(global_dem_cp, -MAXFLOAT);
            }
        }

        //Check if DEM_FILL parameter is specified.
        //If so, use it, otherwise, use the value of FILL_SIZE.
        zvparmd("DEM_FILL", &dem_fill_size, &count, &def, 1, 0);
        if (count == 0) {
            dem_fill_size = fill_size;
        }

        zvmessage("=== DEM Options ===", "");
        snprintf(msg, msgLen, "NODATA: %.3f", dem_nodata);
        zvmessage(msg, "");
        if (zvptst("WEIGHTED")) {
            zvmessage("FILL METHOD: WEIGHTED", "");
        } else {
            zvmessage("FILL METHOD: UNIFORM", "");
        }
        snprintf(msg, msgLen, "OVERLAY MODE: %s", enumToString(overlay_mode));
        zvmessage(msg, "");

        for (int iter = min_input; iter < max_input; iter++) {
            index = zvptst("LAST") ? iter : max_input + min_input - iter - 1;
            file_nl = xyz_file_models[index]->getNL();
            file_ns = xyz_file_models[index]->getNS();           
            
            snprintf(msg, msgLen, "Image index: %d, Geometry: %s", index + 1, 
                    xyz_file_models[index]->getFilename());
            zvmessage(msg, "");
            snprintf(msg, msgLen, "Image index: %d,     Skin: %s", index + 1, 
                    xyz_file_models[index]->getFilename());
            zvmessage(msg, "");

            mars_read_inputs(index, index, xyz_file_models, image_x_band, MAX_NL,
                             MAX_NS, X_BAND, NULL, NULL);
            mars_read_inputs(index, index, xyz_file_models, image_y_band, MAX_NL,
                             MAX_NS, Y_BAND, NULL, NULL);
            mars_read_inputs(index, index, xyz_file_models, image_z_band, MAX_NL, 
                             MAX_NS, Z_BAND, NULL, NULL);

            if (overlay_mode == FIRST || overlay_mode == LAST) {
                doOrrProjOverlayFirstAndLast(image_x_band[0],
                          image_y_band[0], image_z_band[0], 
                          image_z_band[0], global_dem, pointing_in[index], 
                          camera_in[index], cs, z_cs, xyz_file_models[index], 
                          mission_instance, dem_fill_size, dem_nodata, 
                          range_cut, minx, miny, scale, nlo, nso,
                          erode_factor, overlay_mode, TRUE, do_circle, 
                          do_erosion, do_z_coord, idx_out, index + 1, do_idx, 
                          icm_line_out, icm_samp_out, do_icm, do_camera_center,
                          camera_center, do_pixel_angle, pixel_angle, 
                          do_elongated_fill);
            } else {
                doOrrProj(image_x_band[0], image_y_band[0],
                          image_z_band[0], image_z_band[0],
                          global_dem, global_dem_qm, global_dem_rm, 
                          global_dem_sp, global_dem_cp, pointing_in[index], 
                          camera_in[index], cs, z_cs, xyz_file_models[index], 
                          mission_instance, dem_fill_size, dem_nodata, 
                          range_cut, minx, miny, scale, nlo, nso, overlay_mode, 
                          TRUE, do_circle, do_z_coord, idx_out, index + 1, 
                          do_idx, icm_line_out, icm_samp_out, do_icm, 
                          do_camera_center, camera_center, do_pixel_angle, 
                          pixel_angle, do_elongated_fill);
            }

            image_x_band[0]->free();
            image_y_band[0]->free();
            image_z_band[0]->free();
        }

        //No global erosion will be applied to overlay FIRST and LAST. Instead,
        //local erosion is applied to each frame in overlay FIRST and LAST.
        SimpleImage<float> *eroded_dem = NULL;
        int out_unit = getOutFileUnit("OUT_DEM");

        if (do_erosion && overlay_mode != FIRST && overlay_mode != LAST) {
            eroded_dem = erosion(global_dem, global_dem_sp, dem_nodata,
                                 erode_factor, do_elongated_fill);

            // use eroded_dem to mask idx_out
            if (do_idx) {
                for (int line = 0; line < nlo; line++) {
                    for (int samp = 0; samp < nso; samp++) {
                        if (eroded_dem->get(line, samp) == dem_nodata) {
                            idx_out->set(line, samp, 0);
                        }
                    }
                }  
            }

            // use eroded_dem to mask icm_out
            if (do_icm) {
                for (int line = 0; line < nlo; line++) {
                    for (int samp = 0; samp < nso; samp++) {
                        if (eroded_dem->get(line, samp) - dem_nodata) {
                            icm_line_out->set(line, samp, 0);
                            icm_samp_out->set(line, samp, 0);
                        }
                    }
                }
            }

            if (do_zup) {
                negateDEM(eroded_dem, dem_nodata);
            }
 
            if (do_z_coord) {
                writeOutput(eroded_dem, out_unit, xyz_format, xyz_nids, nlo,nso,
                            1, 1, scale, maxx, maxy, cs, xyz_file_models, 
                            mission_instance, NULL, NULL, TRUE, do_zup, 
                            write_cs, dem_nodata, z_cs, idx_out, idx_out_unit, 
                            do_idx, icm_line_out, icm_samp_out, icm_out_unit, 
                            do_icm);
            } else {
                writeOutput(eroded_dem, out_unit, xyz_format, xyz_nids, nlo,nso,
                            1, 1, scale, maxx, maxy, cs, xyz_file_models, 
                            mission_instance, NULL, NULL, TRUE, do_zup, 
                            write_cs, dem_nodata, NULL, idx_out, idx_out_unit, 
                            do_idx, icm_line_out, icm_samp_out, icm_out_unit, 
                            do_icm);
            }
            eroded_dem->free();
            global_dem->free();
            global_dem_qm->free();
            global_dem_rm->free();
            global_dem_sp->free();
            delete global_dem;
            delete global_dem_qm;
            delete global_dem_rm;
            delete global_dem_sp;
            if (do_idx) {
                do_idx = 0;  // only need to do it once.
                idx_out->free();
                delete idx_out;
            }
            if (do_icm) {
                do_icm = 0;  // only need to do it once.
                icm_line_out->free();
                icm_samp_out->free();
                delete icm_line_out;
                delete icm_samp_out;
            }
            if (overlay_mode == MIN or overlay_mode == MAX) {
                global_dem_cp->free();
                delete global_dem_cp;
            }
        } else {
            if (do_zup) {
                negateDEM(global_dem, dem_nodata);
            }
            
            if (do_z_coord) {
                writeOutput(global_dem, out_unit, xyz_format, xyz_nids, nlo,nso,
                            1, 1, scale, maxx, maxy, cs, xyz_file_models, 
                            mission_instance, NULL, NULL, TRUE, do_zup, 
                            write_cs, dem_nodata, z_cs, idx_out, idx_out_unit, 
                            do_idx, icm_line_out, icm_samp_out, icm_out_unit, 
                            do_icm);
            } else {
                writeOutput(global_dem, out_unit, xyz_format, xyz_nids, nlo,nso,
                            1, 1, scale, maxx, maxy, cs, xyz_file_models, 
                            mission_instance, NULL, NULL, TRUE, do_zup, 
                            write_cs, dem_nodata, NULL, idx_out, idx_out_unit, 
                            do_idx, icm_line_out, icm_samp_out, icm_out_unit,
                            do_icm);
            }
            global_dem->free();
            delete global_dem;
            if (do_idx) {
                do_idx = 0;  // only need to do it once.
                idx_out->free();
                delete idx_out;
            } 
            if (do_icm) {
                do_icm = 0;  // only need to do it once.
                icm_line_out->free();
                icm_samp_out->free();
                delete icm_line_out;
                delete icm_samp_out;
            }
            if (overlay_mode == MIN or overlay_mode == MAX) {
                global_dem_cp->free();
                delete global_dem_cp;
            }
        }
    }

    //Generate ORTHO
    if (do_ortho) {
        SimpleImage<float> *global_ortho = NULL;      //ORTHO output
        SimpleImage<float> *global_ortho_qm = NULL;   //ORTHO quality matrix
        SimpleImage<float> *global_ortho_rm = NULL;   //ORTHO range matrix
        SimpleImage<float> *global_ortho_sp = NULL;   //ORTHO span matrix
        SimpleImage<float> *global_ortho_cp = NULL;   //ORTHO compare matrix
        SimpleImage<float> *image_skin[MAX_INPUTS];   //SKIN input
        double ortho_nodata = 0.0;                    //value for ORTHO nodata
        int index;                                    //index of images
        int band;                                     //band of images
        int band_index = 1;                           //the index of band
        int xyz_file_nl;                              //xyz number of lines
        int xyz_file_ns;                              //xyz number of samples
        int skin_file_nl;                             //skin number of lines
        int skin_file_ns;                             //skin number of samples

        memset(image_x_band, 0, sizeof(image_x_band)); //init to NULL
        memset(image_y_band, 0, sizeof(image_y_band)); //init to NULL
        memset(image_z_band, 0, sizeof(image_z_band)); //init to NULL
        memset(image_skin, 0, sizeof(image_skin));     //init to NULL
        
        zvparmd("IMG_NODATA", &ortho_nodata, &count, &def, 1, 0);

        zvmessage("=== ORTHO Options ===", "");
        snprintf(msg, msgLen, "NODATA: %.3f", ortho_nodata);
        zvmessage(msg, "");
        if (zvptst("WEIGHTED")) {
            zvmessage("FILL METHOD: WEIGHTED", "");
        } else {
            zvmessage("FILL METHOD: UNIFORM", "");
        }
        snprintf(msg, msgLen, "OVERLAY MODE: %s", enumToString(overlay_mode));
        zvmessage(msg, "");

        zvparmd("BAND", &band, &count, &def, 1, 0);
        if (count == 0) {
            //Assume all input skin images have the same band.
            band = skin_file_models[0]->getNB();
            
            while (band_index <= band) {
                global_ortho = new SimpleImage<float>(nlo, nso);
                initializeSimpleImage(global_ortho, ortho_nodata);

                //overlay mode FIRST and LAST don't need global quality matrix,
                //range maxtrix, and span maxtrix.
                if (overlay_mode != FIRST && overlay_mode != LAST) {
                    global_ortho_qm = new SimpleImage<float>(nlo, nso);
                    global_ortho_rm = new SimpleImage<float>(nlo, nso);

                    initializeSimpleImage(global_ortho_qm, MAXFLOAT);
                    initializeSimpleImage(global_ortho_rm, MAXFLOAT);

                    if (do_elongated_fill) {
                        global_ortho_sp = new SimpleImage<float>(3, nlo, nso);
                    } else {
                        global_ortho_sp = new SimpleImage<float>(nlo, nso);
                    }
                    initializeSimpleImage(global_ortho_sp, SPAN_NODATA);

                    if (overlay_mode == MIN) {
                        global_ortho_cp = new SimpleImage<float>(nlo, nso);
                        initializeSimpleImage(global_ortho_cp, MAXFLOAT);
                    }

                    if (overlay_mode == MAX) {
                        global_ortho_cp = new SimpleImage<float>(nlo, nso);
                        initializeSimpleImage(global_ortho_cp, -MAXFLOAT);
                    }
                }

                // Create radiometric models for skin images
                for (int iter = min_input; iter < max_input; iter++) {
                    index = zvptst("LAST") ? iter : max_input + min_input - iter - 1;
 
                    if (zvptst("NORAD")) {
                        radiometric[index] = NULL;
                    } else {
                        radiometric[index] = RadiometryModel::create(skin_file_models[index]);
             
                        if (radiometric[index] == NULL) {
                            snprintf(msg, msgLen, "Unable to create radiometric model input %d", index);
                            zvmessage(msg, "");
                        }
                    }

                    PigBrtCorrModel::createBrtCorrModels(skin_nids, skin_file_models, 
                                                         brt_corr, radiometric);
 
                    if (!short_int_data) {
                        radiometric[index] = NULL;
                        brt_corr[index] = NULL;
                    }
                }
                
                // figure out dnscaling factor
                RadiometryModel::setAllDnscalingFactor(radiometric, skin_file_models, skin_nids);
 
                for (int iter = min_input; iter < max_input; iter++) {
                    index = zvptst("LAST") ? iter : max_input + min_input - iter - 1;
                    xyz_file_nl = xyz_file_models[index]->getNL();
                    xyz_file_ns = xyz_file_models[index]->getNS();
                    skin_file_nl = skin_file_models[index]->getNL();
                    skin_file_ns = skin_file_models[index]->getNS();

                    snprintf(msg, msgLen, "Image index: %d, Geometry: %s", index + 1,
                            xyz_file_models[index]->getFilename());
                    zvmessage(msg, "");
                    snprintf(msg, msgLen, "Image index: %d,     Skin: %s, Band: %d", index + 1,
                            skin_file_models[index]->getFilename(), band_index);
                    zvmessage(msg, "");

                    mars_read_inputs(index, index, xyz_file_models, image_x_band, 
                                     MAX_NL, MAX_NS, X_BAND, NULL, NULL);
                    mars_read_inputs(index, index, xyz_file_models, image_y_band, 
                                     MAX_NL, MAX_NS, Y_BAND, NULL, NULL);
                    mars_read_inputs(index, index, xyz_file_models, image_z_band, 
                                     MAX_NL, MAX_NS, Z_BAND, NULL, NULL); 
                    mars_read_inputs(index, index, skin_file_models, image_skin,
                                     MAX_NL, MAX_NS, band_index, radiometric, 
                                     brt_corr);

                    if (overlay_mode == FIRST || overlay_mode == LAST) {
                        doOrrProjOverlayFirstAndLast(image_x_band[0], 
                                  image_y_band[0], image_z_band[0],
                                  image_skin[0], global_ortho, 
                                  pointing_in[index], camera_in[index], cs, z_cs, 
                                  xyz_file_models[index], mission_instance, 
                                  fill_size, ortho_nodata, range_cut, minx, 
                                  miny, scale, nlo, nso, erode_factor, 
                                  overlay_mode, FALSE, do_circle, do_erosion,
                                  do_z_coord, idx_out, index + 1, do_idx, 
                                  icm_line_out, icm_samp_out, do_icm, 
                                  do_camera_center, camera_center, 
                                  do_pixel_angle, pixel_angle, do_elongated_fill);
                    } else {
                        doOrrProj(image_x_band[0], image_y_band[0],
                                  image_z_band[0], image_skin[0],
                                  global_ortho, global_ortho_qm, global_ortho_rm,
                                  global_ortho_sp, global_ortho_cp, 
                                  pointing_in[index], camera_in[index], cs, z_cs, 
                                  xyz_file_models[index], mission_instance, 
                                  fill_size, ortho_nodata, range_cut, minx, miny,
                                  scale, nlo, nso, overlay_mode, FALSE, 
                                  do_circle, do_z_coord, idx_out, index + 1, 
                                  do_idx, icm_line_out, icm_samp_out, do_icm,
                                  do_camera_center, camera_center, 
                                  do_pixel_angle, pixel_angle, do_elongated_fill);
                    }

                    image_x_band[0]->free();
                    image_y_band[0]->free();
                    image_z_band[0]->free();
                    image_skin[0]->free();
                }

                //No global erosion will be applied to overlay FIRST and LAST. 
                //Instead, local erosion is applied to each frame in overlay 
                //FIRST and LAST.
                SimpleImage<float> *eroded_ortho = NULL;
                int out_unit = getOutFileUnit("OUT");

                if (do_erosion && overlay_mode != FIRST && overlay_mode != LAST) {
                    eroded_ortho = erosion(global_ortho, global_ortho_sp,
                                           ortho_nodata, erode_factor, 
                                           do_elongated_fill);

                    // use eroded_ortho to mask idx_out
                    if (do_idx) {
                        for (int line = 0; line < nlo; line++) {
                            for (int samp = 0; samp < nso; samp++) {
                                if (eroded_ortho->get(line, samp) == ortho_nodata) {
                                    idx_out->set(line, samp, 0);
                                }
                            }
                        }
                    }

                    // use eroded_ortho to mask icm_out
                    if (do_icm) {
                        for (int line = 0; line < nlo; line++) {
                            for (int samp = 0; samp < nso; samp++) {
                                if (eroded_ortho->get(line, samp) == ortho_nodata) {
                                    icm_line_out->set(line, samp, 0);
                                    icm_samp_out->set(line, samp, 0);
                                }
                            }
                        }
                    }

                    writeOutput(eroded_ortho, out_unit, skin_format, skin_nids, 
                                nlo, nso, band_index, band, scale, maxx, maxy, 
                                cs, skin_file_models, mission_instance, 
                                radiometric, brt_corr, FALSE, do_zup, write_cs, 
                                ortho_nodata, NULL, idx_out, idx_out_unit, 
                                do_idx, icm_line_out, icm_samp_out, icm_out_unit, 
                                do_icm);
                    eroded_ortho->free();
                    global_ortho->free();
                    global_ortho_qm->free();
                    global_ortho_rm->free();
                    global_ortho_sp->free();
                    delete eroded_ortho;
                    delete global_ortho;
                    delete global_ortho_qm;
                    delete global_ortho_rm;
                    delete global_ortho_sp;
                    if (do_idx) {
                        do_idx = 0;  // only need to do it once.
                        idx_out->free();
                        delete idx_out;
                    }
                    if (do_icm) {
                        do_icm = 0;  // only need to do it once.
                        icm_line_out->free();
                        icm_samp_out->free();
                        delete icm_line_out;
                        delete icm_samp_out;
                    }
                } else {
 
                    writeOutput(global_ortho, out_unit, skin_format, skin_nids, 
                                nlo, nso, band_index, band, scale, maxx, maxy, 
                                cs, skin_file_models, mission_instance, 
                                radiometric, brt_corr, FALSE, do_zup, write_cs, 
                                ortho_nodata, NULL, idx_out, idx_out_unit, 
                                do_idx, icm_line_out, icm_samp_out, icm_out_unit, 
                                do_icm);
                    global_ortho->free();
                    delete global_ortho;
                    if (do_idx) {
                        do_idx = 0;  // only need to do it once.
                        idx_out->free();
                        delete idx_out;
                    }
                    if (do_icm) {
                        do_icm = 0;
                        icm_line_out->free();
                        icm_samp_out->free();
                        delete icm_line_out;
                        delete icm_samp_out;
                    }
                }

                band_index++;
            }
        } else {
            global_ortho = new SimpleImage<float>(nlo, nso);
            initializeSimpleImage(global_ortho, ortho_nodata);
 
            if (overlay_mode != FIRST && overlay_mode != LAST) {
                global_ortho_qm = new SimpleImage<float>(nlo, nso);
                global_ortho_rm = new SimpleImage<float>(nlo, nso);

                initializeSimpleImage(global_ortho_qm, MAXFLOAT);
                initializeSimpleImage(global_ortho_rm, MAXFLOAT);
                
                if (do_elongated_fill) {
                        global_ortho_sp = new SimpleImage<float>(3, nlo, nso);
                    } else {
                        global_ortho_sp = new SimpleImage<float>(nlo, nso);
                }
                initializeSimpleImage(global_ortho_sp, SPAN_NODATA);
         

                if (overlay_mode == MIN) {
                    global_ortho_cp = new SimpleImage<float>(nlo, nso);
                    initializeSimpleImage(global_ortho_cp, MAXFLOAT);
                }

                if (overlay_mode == MAX) {
                    global_ortho_cp = new SimpleImage<float>(nlo, nso);
                    initializeSimpleImage(global_ortho_cp, -MAXFLOAT);
                }
            }

            // Create radiometric models for skin images
            for (int iter = min_input; iter < max_input; iter++) {
                 index = zvptst("LAST") ? iter : max_input + min_input - iter - 1;

                 if (zvptst("NORAD")) {
                     radiometric[index] = NULL;
                 } else {
                     radiometric[index] = RadiometryModel::create(skin_file_models[index]);

                     if (radiometric[index] == NULL) {
                         snprintf(msg, msgLen, "Unable to create radiometric model input %d", index);
                         zvmessage(msg, "");
                     }
                 }

                 PigBrtCorrModel::createBrtCorrModels(skin_nids, skin_file_models,
                                                          brt_corr, radiometric);

                 if (!short_int_data) {
                     radiometric[index] = NULL;
                     brt_corr[index] = NULL;
                 }
            }

            // figure out dnscaling factor
            RadiometryModel::setAllDnscalingFactor(radiometric, skin_file_models, skin_nids);
            
            for (int iter = min_input; iter < max_input; iter++) {
                index = zvptst("LAST") ? iter : max_input + min_input - iter - 1;
                xyz_file_nl = xyz_file_models[index]->getNL();
                xyz_file_ns = xyz_file_models[index]->getNS();
                skin_file_nl = skin_file_models[index]->getNL();
                skin_file_ns = skin_file_models[index]->getNS();

                snprintf(msg, msgLen, "Image index: %d, Geometry: %s", index + 1,
                        xyz_file_models[index]->getFilename());
                zvmessage(msg, "");
                snprintf(msg, msgLen, "Image index: %d,     Skin: %s, Band: %d", index + 1,
                        skin_file_models[index]->getFilename(), band);
                zvmessage(msg, "");

                mars_read_inputs(index, index, xyz_file_models, image_x_band, 
                                 MAX_NL, MAX_NS, X_BAND, NULL, NULL);
                mars_read_inputs(index, index, xyz_file_models, image_y_band,
                                 MAX_NL, MAX_NS, Y_BAND, NULL, NULL);
                mars_read_inputs(index, index, xyz_file_models, image_z_band,
                                 MAX_NL, MAX_NS, Z_BAND, NULL, NULL);
                mars_read_inputs(index, index, skin_file_models, image_skin, 
                                 MAX_NL, MAX_NS, band, radiometric, brt_corr);
                
                if (overlay_mode == FIRST || overlay_mode == LAST) {
                    doOrrProjOverlayFirstAndLast(image_x_band[0],
                              image_y_band[0], image_z_band[0],
                              image_skin[0], global_ortho,
                              pointing_in[index], camera_in[index], cs, z_cs, 
                              xyz_file_models[index], mission_instance, 
                              fill_size, ortho_nodata, range_cut, minx, miny, 
                              scale, nlo, nso, erode_factor, overlay_mode, 
                              FALSE, do_circle, do_erosion, do_z_coord, 
                              idx_out, index + 1, do_idx, icm_line_out, 
                              icm_samp_out, do_icm, do_camera_center, 
                              camera_center, do_pixel_angle, pixel_angle, 
                              do_elongated_fill);
                } else {
                    doOrrProj(image_x_band[0], image_y_band[0],
                              image_z_band[0], image_skin[0],
                              global_ortho, global_ortho_qm, global_ortho_rm,
                              global_ortho_sp, global_ortho_cp, pointing_in[index],
                              camera_in[index], cs, z_cs, xyz_file_models[index],
                              mission_instance, fill_size, ortho_nodata, 
                              range_cut, minx, miny, scale, nlo, nso, 
                              overlay_mode, FALSE, do_circle, do_z_coord, 
                              idx_out, index + 1, do_idx, icm_line_out, 
                              icm_samp_out, do_icm, do_camera_center,
                              camera_center, do_pixel_angle, pixel_angle, 
                              do_elongated_fill);
                }
                
                image_x_band[0]->free();
                image_y_band[0]->free();
                image_z_band[0]->free();
                image_skin[0]->free();
            }

            //No global erosion will be applied to overlay FIRST and LAST. 
            //Instead, local erosion is applied to each frame in overlay 
            //FIRST and LAST.
            SimpleImage<float> *eroded_ortho;
            int out_unit = getOutFileUnit("OUT");

            if (do_erosion && overlay_mode != FIRST && overlay_mode != LAST) {
                eroded_ortho = erosion(global_ortho, global_ortho_sp, 
                                       ortho_nodata, erode_factor, 
                                       do_elongated_fill);

                // use eroded_ortho to mask idx_out
                if (do_idx) {
                    for (int line = 0; line < nlo; line++) {
                        for (int samp = 0; samp < nso; samp++) {
                            if (eroded_ortho->get(line, samp) == ortho_nodata) {
                                idx_out->set(line, samp, 0);
                            }
                        }
                    }
                }

                // use eroded_ortho to mask icm_out
                if (do_icm) {
                    for (int line = 0; line < nlo; line++) {
                        for (int samp = 0; samp < nso; samp++) {
                            if (eroded_ortho->get(line, samp) == ortho_nodata) {
                                icm_line_out->set(line, samp, 0);
                                icm_samp_out->set(line, samp, 0);
                            }
                        }
                    }
                }

                writeOutput(eroded_ortho, out_unit, skin_format, skin_nids, nlo,
                            nso, 1, 1, scale, maxx, maxy, cs, skin_file_models, 
                            mission_instance, radiometric, brt_corr, FALSE, 
                            do_zup, write_cs, ortho_nodata, NULL, idx_out, 
                            idx_out_unit, do_idx, icm_line_out, icm_samp_out,
                            icm_out_unit, do_icm);

                eroded_ortho->free();
                global_ortho->free();
                global_ortho_qm->free();
                global_ortho_rm->free();
                global_ortho_sp->free();
                delete eroded_ortho;
                delete global_ortho;
                delete global_ortho_qm;
                delete global_ortho_rm;
                delete global_ortho_sp;
                if (do_idx) {
                    do_idx = 0;  // only need to do it once.
                    idx_out->free();
                    delete idx_out;
                }
                if (do_icm) {
                    do_icm = 0;
                    icm_line_out->free();
                    icm_samp_out->free();
                    delete icm_line_out;
                    delete icm_samp_out;
                }
                if (overlay_mode == MIN or overlay_mode == MAX) {
                    global_ortho_cp->free();
                    delete global_ortho_cp;
                }
            } else {
                writeOutput(global_ortho, out_unit, skin_format, skin_nids, nlo,
                            nso, 1, 1, scale, maxx, maxy, cs, skin_file_models, 
                            mission_instance, radiometric, brt_corr, FALSE, 
                            do_zup, write_cs, ortho_nodata, NULL, idx_out, 
                            idx_out_unit, do_idx, icm_line_out, icm_samp_out, 
                            icm_out_unit, do_icm);
                global_ortho->free();
                delete global_ortho;
                if (do_idx) {
                    do_idx = 0;  // only need to do it once.
                    idx_out->free();
                    delete idx_out;
                }
                if (do_icm) {
                    do_icm = 0;
                    icm_line_out->free();
                    icm_samp_out->free();
                    delete icm_line_out;
                    delete icm_samp_out;
                }
                if (overlay_mode == MIN or overlay_mode == MAX) {
                    global_ortho_cp->free();
                    delete global_ortho_cp;
                }
            }
        }
    }
}

OverlayMode getOverlayMode()
{
    OverlayMode overlay_mode;

    if(zvptst("CLOSEST")) {
        overlay_mode = CLOSEST;
    } else if (zvptst("AVG")) {
        overlay_mode = AVG;
    } else if (zvptst("MIN")) {
        overlay_mode = MIN;
    } else if (zvptst("MAX")) {
        overlay_mode = MAX;
    } else if (zvptst("FIRST")) {
        overlay_mode = FIRST;
    } else if (zvptst("LAST")) {
        overlay_mode = LAST;
    } else {
        zvmessage("Overlay mode is undefined.\n", "");
        zabend();
    }

    return overlay_mode;
}

//This function is exactly the same as mars_setup except this one reads IN_XYZ,
//instead of INP. Maybe should be put in mars_setup.cc.
void env_setup(int &nids, PigFileModel *files[], PigCameraModel *cameras[],
                PigPointingModel *pointings[], PigSurfaceModel *&surface_model,
                RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr_models[],
                PigCoordSystem *&def_cs, char *mission, char *instrument,
                int &homogeneous_inputs, const int max_nl, const int max_ns,
                const int max_inputs)
{
    int i;
    int status = 0;
    const size_t msgLen = PIG_MAX_FILENAME_SIZE+1;
    char msg[msgLen];
    
    homogeneous_inputs = TRUE;
    
    ////////////////////////////////////////////////////////////////////////
    // Get the list of all files
    
    char **filenames = new char *[max_inputs];
    if (filenames == NULL) {
        zvmessage("Memory error in mars_setup, filename array", "");
        zabend();
    }
    mars_get_filelist("IN_XYZ", nids, filenames, max_inputs, FALSE);
    
    ////////////////////////////////////////////////////////////////////////
    // Open all files
    
    for (i = 0; i < nids; i++) {
        
        files[i] = PigFileModel::create(filenames[i]);
        if (files[i] == NULL) {
            snprintf(msg, msgLen, "Unable to create file model for input %d", i);
            zvmessage(msg, "");
            zabend();
        }
        
        if ((max_nl != 0 && files[i]->getNL() > max_nl) ||
            (max_ns != 0 && files[i]->getNS() > max_ns)) {
            snprintf(msg, msgLen, "Input image %d exceeds buffer limit size", i);
            zvmessage(msg, "");
            zabend();
        }
    }
    
    ////////////////////////////////////////////////////////////////////////
    // Coord system stuff
    
    // Initialize the RSF's, if necessary.
    //!!!! THIS SHOULD BE DONE ONCE PER MISSION !!!!
    
    mars_read_rsf(files[0]->getMissionName());
    
    // Default coordinates
    
    def_cs = mars_setup_coords(files[0]->getMissionName(), nids, files);
    
    PigMission *m = PigMission::getMissionObject(mission);
    PigPointingCorrections *pointing_corrections =
			mars_get_pointing_corrections(m);
    
    char solution_id[256];
    int count;
    zvp("SOLUTION_ID", solution_id, &count);
    char *sid = NULL;
    if (count != 0) {
        sid = solution_id;
    }
    else if (pointing_corrections != NULL) {
        // no solution_id found on a command line
        // default to highest priority in the xml nav file if it exists
        sid = pointing_corrections->getHighestPriority();
    }
    
    // construct surface model
    if (pointing_corrections != NULL) {
        //look for surface in the nav file
        PigSurfaceModelParams *smp = pointing_corrections->getSurfaceModel(sid);
        
        //!!!! This is a hack to get around the problem that we can't create
        // proper coordinate system knowing only it's name.  For example
        // specifying on a command line SURF_COORD=ROVER is not enough to create
        // proper Rover Coordinate System.  Because of that we check for command
        // line parameter here, and if it's given we discard NAV file surface model
        // if any and go directly to PigSurfaceModel::create(file).  That one knows
        // how to create proper CS.  For more info see comments in PigMission.cc
        // -ozp

	PigMission *m = pointing_corrections->getMission();

        int cnt=0;
        char surface_coord[20];
        PigModelBase::getStaticParam("SURF_COORD", surface_coord, &cnt, 1, 0);
        if ((smp == NULL) || (cnt==1)) {
            // no surface model definition found in the nav file,
            // or surface model definition has been specified
            // on the command line
            //create surface model using label info from the first
            // input file
            surface_model = PigSurfaceModel::create(files[0]);
        }
        else {
            PigCSReference *csr = smp->getCoordSystemParams();
	    PigCSReference csRef(m, csr->getFrameName(),
				sid,
				csr->getIndices(),
				csr->getNumIndices(),
				csr->getInstId());
            
            surface_model = PigSurfaceModel::create(
                              m->getMissionName(), instrument,
                              smp->getPointingParams()->getType(),
                              smp->getPointingParams()->getParameters(),
                              smp->getPointingParams()->getPointingParamCount(),
                              m->getCoordSystem(&csRef));
        }
    }
    else {
        // no nav file in xml format exists,
        //create default one using label info from the first
        // input file
        surface_model = PigSurfaceModel::create(files[0]);
    }
    
    ////////////////////////////////////////////////////////////////////////
    // Now loop through and initialize all files.  This must be done after
    // the above coord system setups
    
    for (i = 0; i < nids; i++) {
        
        // Clear fields (or set to default) in case label not present
        
        cameras[i] = NULL;
        pointings[i] = NULL;
        if (radiometric != NULL)
            radiometric[i] = NULL;
        strcpy(mission, "");
        strcpy(instrument, "");
        if (files[i]->getMissionName() != NULL)
            strcpy(mission, files[i]->getMissionName());
        // it's ID in the file model but becomes name in the camera model, sigh
        if (files[i]->getInstrumentId() != NULL)
            strcpy(instrument, files[i]->getInstrumentId());
        
        ////////////////////////////////////////////////////////////////////////
        // Compute initial camera and pointing models
        
        cameras[i] = PigCameraModel::create(files[i], NULL);
        
        if (cameras[i] == NULL) {
            snprintf(msg, msgLen, "Unable to create camera model for input %d", i);
            zvmessage(msg, "");
            continue;
        }
        
        pointings[i] = mars_create_pointing_model(cameras[i], files[i], sid,
                                                  pointing_corrections, status);
        
        if (pointings[i] == NULL) {
            snprintf(msg,msgLen, "Unable to create pointing model for input %d", i+1);
            zvmessage(msg, "");
            continue;
        }
        
        if (status == -1) {
            //nav file exists but no match has been found
            snprintf(msg, msgLen,"No match in the Navtable has been found for input %d",
                    i+1);
            zvmessage(msg, "");
            // Point the input camera
            pointings[i]->pointCamera(files[i]);
        }
        else if (status == 0) {
            // no pointing correction file present
            // Point the input camera
            pointings[i]->pointCamera(files[i]);
        }
        else if (status == 1) {
            // correction has been applied
            snprintf(msg, msgLen, "Pointing Correction has been applied for input %d", i+1);
            zvmessage(msg, "");
        }
        
        // Check for varying missions and instruments, for information only
        
        if (i != 0) {
            if (strcasecmp(mission, cameras[i]->getMissionName()) != 0) {
                snprintf(msg, msgLen,
                        "Note: Input list contains more than one mission: %s",
                        mission);
                zvmessage(msg, "");
                homogeneous_inputs = FALSE;
            }
            if (strcasecmp(instrument, cameras[i]->getInstrumentName()) != 0) {
                snprintf(msg, msgLen,
                        "Note: Input list contains more than one instrument: %s"
                        , instrument);
                zvmessage(msg, "");
                homogeneous_inputs = FALSE;
            }
        }
        strcpy(mission, cameras[i]->getMissionName());
        strcpy(instrument, cameras[i]->getInstrumentName());
        
        ////////////////////////////////////////////////////////////////////////
        // Create Radiometry Models for each input image, if requested
        
        if (radiometric != NULL) {
            if (zvptst("NORAD")) {              // No radiometric correction
                radiometric[i] = NULL;
            }
            else {
                radiometric[i] = RadiometryModel::create(files[i]);
                
                if (radiometric[i] == NULL) {
                    snprintf(msg, msgLen,
                            "Unable to create radiometric model for input %d", i);
                    zvmessage(msg, "");
                }
            }
        }
        
        ////////////////////////////////////////////////////////////////////////
        // Create Brightness Correction Models for each image, if requested.
        // Rad is needed by some corrections (HsiLin for example)
        
        if (brt_corr_models != NULL) {
            PigBrtCorrModel::createBrtCorrModels(nids, files,
                                                 brt_corr_models, radiometric);
        }
        
        
        files[i]->closeFile();                  // don't have too many open
    }
    
    // check for old style, text-based nav file, if it exists, apply it's values
    if (pointing_corrections == NULL) {
        //check for old format style and if nav file in old format found
        //print warning message and apply pointing corrections
        mars_apply_navtable(nids, pointings, NULL);
    }
    
    for (i=0; i < nids; i++)
        delete filenames[i];
    delete filenames;
    
    zvmessage("All input labels processed", "");
    
    return;
}

void initializeSimpleImage(SimpleImage<float> *image, double nodata)
{
    for (int band = 0; band < image->getNB(); band++) {
        for (int line = 0; line < image->getNL(); line++) {
            for (int samp = 0; samp < image->getNS(); samp++) {
                image->set(band, line, samp, nodata);
            }
        }
    }
}

void initializeSimpleImage(SimpleImage<short int> *image, short int nodata) 
{
    for (int line = 0; line < image->getNL(); line++) {
        for (int samp = 0; samp < image->getNS(); samp++) {
            image->set(line, samp, nodata);
        }
    }
}


void doOrrProjOverlayFirstAndLast(SimpleImage<float> *geometry_x,
               SimpleImage<float> *geometry_y, SimpleImage<float> *geometry_z,
               SimpleImage<float> *skin_image, SimpleImage<float> *global_image,
               PigPointingModel *pointing_in, PigCameraModel *camera_in,
               PigCoordSystem *cs, PigCoordSystem *z_cs,
               PigFileModel *xyz_file_model, PigMission *mission_instance, 
               int fill_size, double nodata, double range_cut, double minx, 
               double miny, double scale, int nlo, int nso, double erode_factor,
               OverlayMode overlay_mode, int do_dem, int do_circle, 
               int do_erosion, int do_z_coord, SimpleImage<short int> *idx_out,
               int img_idx, int do_idx, SimpleImage<short int> *icm_line_out,
               SimpleImage<short int> *icm_samp_out, int do_icm,
               int do_camera_center, double camera_center[], int do_pixel_angle,
               double pixel_angle, short int do_elongated_fill)
{
    SimpleImage<float> *local_image = NULL;            // individual ORR
    SimpleImage<float> *eroded_ortho = NULL;           // individual eroded ORR
    SimpleImage<float> *local_ortho_qm = NULL;         // individual quality matrix
    SimpleImage<float> *local_ortho_rm = NULL;         // individual range matrix
    SimpleImage<float> *local_ortho_sp = NULL;         // individual span matrix
    SimpleImage<short int> *local_idx_out = NULL;      // individual idx map
    SimpleImage<short int> *local_icm_line_out = NULL; // individual icm line map
    SimpleImage<short int> *local_icm_samp_out = NULL; // individual icm samp map

    local_image = new SimpleImage<float>(nlo, nso);
    local_ortho_qm = new SimpleImage<float>(nlo, nso);
    local_ortho_rm = new SimpleImage<float>(nlo, nso);
    
    if (do_elongated_fill) {
        local_ortho_sp = new SimpleImage<float>(3, nlo, nso);
    } else {
        local_ortho_sp = new SimpleImage<float>(nlo, nso);
    }

    if (do_idx) {
        local_idx_out = new SimpleImage<short int>(nlo, nso);
    }
    if (do_icm) {
        local_icm_line_out = new SimpleImage<short int>(nlo, nso);
        local_icm_samp_out = new SimpleImage<short int>(nlo, nso);
    }
    
    initializeSimpleImage(local_image, nodata);
    initializeSimpleImage(local_ortho_qm, MAXFLOAT);
    initializeSimpleImage(local_ortho_rm, MAXFLOAT);
    initializeSimpleImage(local_ortho_sp, SPAN_NODATA);

    doOrrProj(geometry_x, geometry_y, geometry_z, skin_image, local_image, 
              local_ortho_qm, local_ortho_rm, local_ortho_sp, NULL, pointing_in,
              camera_in, cs, z_cs, xyz_file_model, mission_instance, fill_size, 
              nodata, range_cut, minx, miny, scale, nlo, nso, overlay_mode, 
              do_dem, do_circle, do_z_coord, local_idx_out, img_idx, do_idx, 
              local_icm_line_out, local_icm_samp_out, do_icm, do_camera_center,
              camera_center, do_pixel_angle, pixel_angle, do_elongated_fill);

    if (do_erosion) {
        eroded_ortho = erosion(local_image, local_ortho_sp, nodata, erode_factor, 
                               do_elongated_fill);
        
        for (int line = 0; line < nlo; line++) {
            for (int samp = 0; samp < nso; samp++) {
                if (eroded_ortho->get(line, samp) == nodata) {
                    continue;
                }

                //merge local ORR into global ORR
                global_image->set(line, samp, eroded_ortho->get(line, samp));

                if (do_idx) {
                    idx_out->set(line, samp, local_idx_out->get(line, samp));
                }
                if (do_icm) {
                    icm_line_out->set(line, samp, local_icm_line_out->get(line, 
                                      samp));
                    icm_samp_out->set(line, samp, local_icm_samp_out->get(line, 
                                      samp));
                }
            }
        }
      
        eroded_ortho->free();
        delete eroded_ortho;
    } else {
        for (int line = 0; line < nlo; line++) {
            for (int samp = 0; samp < nso; samp++) {
                if (local_image->get(line, samp) == nodata) {
                    continue;
                }

                //merge local ORR into global ORR
                global_image->set(line, samp, local_image->get(line, samp));

                if (do_idx) {
                    idx_out->set(line, samp, local_idx_out->get(line, samp));
                }
                if (do_icm) {
                    icm_line_out->set(line, samp, local_icm_line_out->get(line,
                                      samp));
                    icm_samp_out->set(line, samp, local_icm_samp_out->get(line,
                                      samp));
                }
            }
        }
    }

    local_image->free();
    local_ortho_qm->free();
    local_ortho_rm->free();
    local_ortho_sp->free();
    delete local_image;
    delete local_ortho_qm;
    delete local_ortho_rm;
    delete local_ortho_sp;
    
    if (do_idx) {
        local_idx_out->free();
        delete local_idx_out;
    }
    if (do_icm) {
        local_icm_line_out->free();
        local_icm_samp_out->free();
        delete local_icm_line_out;
        delete local_icm_samp_out;
    }
}

void doOrrProj(SimpleImage<float> *geometry_x, SimpleImage<float> *geometry_y,
               SimpleImage<float> *geometry_z, SimpleImage<float> *skin_image,
               SimpleImage<float> *global_image, SimpleImage<float> *quality_matrix,
               SimpleImage<float> *range_matrix, SimpleImage<float> *span_matrix,
               SimpleImage<float> *compare_matrix,
               PigPointingModel *pointing_in, PigCameraModel *camera_in,
               PigCoordSystem *cs, PigCoordSystem *z_cs,
               PigFileModel *xyz_file_model, PigMission *mission_instance, 
               int fill_size, double nodata, double range_cut, double minx, 
               double miny, double scale, int nlo, int nso, 
               OverlayMode overlay_mode, int do_dem, int do_circle, 
               int do_z_coord, SimpleImage<short int> *idx_out, int img_idx, 
               int do_idx, SimpleImage<short int> *icm_line_out, 
               SimpleImage<short int> *icm_samp_out, int do_icm, 
               int do_camera_center, double camera_center[], int do_pixel_angle,
               double pixel_angle, short int do_elongated_fill)
{
    PigCSReference *ref = NULL;           // Reference to identify coordinate system
    PigPoint range_origin;        // where is PigPoint file????????
    double camera_position[3];    // XYZ position for the camera
    PigCoordSystem *xyz_cs;       // XYZ coordinate system
    double angle;                 // pixel angle
    int fileNL;                   // Number of lines
    int fileNS;                   // Number of samples
    double value_x;               // Value of x band
    double value_y;               // Value of y band
    double value_z;               // Value of z band
    double new_z;                 // Value of converted z band
    double value_compare;         // Value to use for comparsion in OVERLAY mode
    double value_set;             // Value to set for ORR or DEM
    double range;                 // Range from a position to camera       
    int xpos;                     // line in ortho projection
    int ypos;                     // samp in ortho projection
    int span_line;                // line span in pixel (int)
    int span_samp;                // sample span in pixel (int)
    double span_angle;            // angle for rotating filled extents
    double dbl_span_line;         // line span in pixel (double)
    double dbl_span_samp;         // sample span in pixel (double)
    short int do_weighted_fill;
    double diff_x;
    double diff_y;
    double diff_z;
    int count, def;
    double fill_factor;
    double max_angle;
    double max_span;

    zvparmd("FILL_FACTOR", &fill_factor, &count, &def, 1, 0);
    zvparmd("MAX_ANGLE", &max_angle, &count, &def, 1, 0);
    zvparmd("MAX_SPAN", &max_span, &count, &def, 1, 0);

    xyz_file_model->getDerivedImageCS(ref);
    xyz_cs = mission_instance->getCoordSystem(ref);
    fileNL = geometry_x->getNL();
    fileNS = geometry_y->getNS();

    if (do_camera_center) {
        camera_position[0] = camera_center[0];
        camera_position[1] = camera_center[1];
        camera_position[2] = camera_center[2];
    } else {
        if (pointing_in != NULL) {
            range_origin = pointing_in->getCameraPosition(cs);
            range_origin.getXYZ(camera_position);
        } else {
            zvmessage("Camera model not found in input files; CAMERA_CENTER "
                          "parameter required", "");
            zabend();
        }
    }
    
    if (do_pixel_angle) {
        angle = pixel_angle;
    } else {
        if (camera_in != NULL) {
            angle = camera_in->getPixelAngle(0);
        } else {
            zvmessage("Camera model not found in input files; PIXEL ANGLE "
                      "parameter required", "");
        }
    }

    if (zvptst("WEIGHTED")) {
        do_weighted_fill = TRUE;
    } else {
        do_weighted_fill = FALSE;
    }

    for (int line = 0; line < fileNL; line++) {
        for (int samp = 0; samp < fileNS; samp++) {
            value_x = geometry_x->get(line, samp);
            value_y = geometry_y->get(line, samp);
            value_z = geometry_z->get(line, samp);

            //convert coordinate system if necessary
            if(cs != xyz_cs){
                PigPoint old_xyz(value_x, value_y, value_z);
                if(value_x != 0.0 || value_y != 0.0 || value_z != 0.0){
                    PigPoint new_xyz = cs->convertPoint(old_xyz, xyz_cs);
                    value_x = new_xyz.getX();
                    value_y = new_xyz.getY();
                    value_z = new_xyz.getZ();
                }
            }

            //check invalid value 
            if (value_x == 0.0 && value_y == 0.0 && value_z == 0.0) {
                continue;
            }

            if (do_z_coord && z_cs != NULL) {
                PigPoint old_xyz(value_x, value_y, value_z);
                PigPoint new_xyz = z_cs->convertPoint(old_xyz, cs);
                new_z = new_xyz.getZ();
            } else {
                new_z = value_z;
            }

            if (do_dem) {
                value_compare = new_z;
                value_set = new_z;
            } else {
                value_compare = new_z;
                value_set = skin_image->get(line, samp);
            }

	    // Compute range from camera
            diff_x = value_x - camera_position[0];
            diff_y = value_y - camera_position[1];
            diff_z = value_z - camera_position[2];
            range = sqrt(diff_x * diff_x + diff_y * diff_y + diff_z * diff_z);

	    // skip if point is outside the range
	    if (range_cut > 0) {
		double range_check = range;

		if (do_circle) {		// 2D distance from CS origin
		    range_check = sqrt(value_x * value_x + value_y * value_y);
		}

                if (range_check > range_cut) {
                    continue;
                }
            } 

            xpos = (int)((value_x - minx) * (-scale) + nlo - 1);
            ypos = (int)((value_y - miny) * scale);

            if(xpos < 0 || xpos >= nlo || ypos < 0 || ypos >= nso){
                continue;
            }

            span_angle = 0; 
            if (do_weighted_fill) {
                if (do_elongated_fill) {
                    dbl_span_line = calculateLineSpan(value_x, value_y, value_z,
                        camera_position, angle, scale, max_angle, max_span);
                    dbl_span_samp = calculateSampleSpan(value_x, value_y, 
                        value_z, camera_position, angle, scale, max_angle, max_span);
                    dbl_span_line = fill_factor * dbl_span_line;
                    dbl_span_samp = fill_factor * dbl_span_samp;
                    span_angle = atan2((value_y - camera_position[1]),  
                                       (value_x - camera_position[0]));
                } else {
                    // square fill uses the same line/sample span.
                    dbl_span_line = calculateWeightedSpan(value_x, value_y, 
                        value_z, camera_position, angle, scale, max_angle, max_span);
                    dbl_span_line = fill_factor * dbl_span_line;
                    dbl_span_samp = dbl_span_line;
                }
       
                span_line = (int)round(dbl_span_line);
                span_samp = (int)round(dbl_span_samp);
            } else {
                dbl_span_line = fill_factor * fill_size;
                dbl_span_samp = fill_factor * fill_size;

                span_line = (int)round(dbl_span_line);
                span_samp = (int)round(dbl_span_samp);
            }


            if (overlay_mode == AVG) {
                avgModeHandler(global_image, quality_matrix, span_matrix, xpos,
                               ypos, value_set, span_line, span_samp, 
                               span_angle, nodata, nlo, nso, do_elongated_fill);
            } else if (overlay_mode == MIN) {
                minModeHandler(global_image, quality_matrix, span_matrix, xpos,
                               ypos, value_compare, value_set, span_line, 
                               span_samp, span_angle, nodata, nlo, nso, idx_out,
                               img_idx, do_idx, icm_line_out, icm_samp_out, 
                               do_icm, line, samp, compare_matrix, 
                               do_elongated_fill);
            } else if (overlay_mode == MAX) {
                maxModeHandler(global_image, quality_matrix, span_matrix, xpos,
                               ypos, value_compare, value_set, span_line, 
                               span_samp, span_angle, nodata, nlo, nso, idx_out,
                               img_idx, do_idx, icm_line_out, icm_samp_out, 
                               do_icm, line, samp, compare_matrix, 
                               do_elongated_fill);
            } else if (overlay_mode == FIRST || overlay_mode == LAST) {
                firstAndLastModesHandler(global_image, quality_matrix,
                                         span_matrix, xpos, ypos, value_set,
                                         span_line, span_samp, span_angle, 
                                         nodata, nlo, nso, idx_out, img_idx, 
                                         do_idx, icm_line_out, icm_samp_out, 
                                         do_icm, line, samp, do_elongated_fill);
            } else {
                closestModeHandler(global_image, quality_matrix, range_matrix,
                                   span_matrix, xpos, ypos, value_set, range,
                                   span_line, span_samp, span_angle, nodata, 
                                   nlo, nso, idx_out, img_idx, do_idx, 
                                   icm_line_out, icm_samp_out, do_icm, line, 
                                   samp, do_elongated_fill);
            }
        }
    }
}

int getOutFileUnit(char *output_parameter)
{
    int out_unit;
    int count;
    char filename[PIG_MAX_FILENAME_SIZE + 1];

    zvp(output_parameter, filename, &count);
    zvunit(&out_unit, output_parameter, 1, "U_NAME", filename, NULL);

    return out_unit;
}

void negateDEM(SimpleImage<float> *global_dem, double dem_nodata) {
    for (int line = 0; line < global_dem->getNL(); line++) {
        for (int samp = 0; samp < global_dem->getNS(); samp++) {
            float value = global_dem->get(line, samp);
            //do not negate no data value
            if (value == dem_nodata) {
                continue;
            }

            global_dem->set(line, samp, -1 * value);
        }
    }
}

void writeOutput(SimpleImage<float> *image_result, int out_unit, char *format, 
                 int nids, int nlo, int nso, int current_band, int total_band, 
                 double scale, double maxx, double maxy, PigCoordSystem *cs, 
                 PigFileModel *file_models[], PigMission *mission_instance,
                 RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr[],
                 int is_dem, int do_zup, PigCoordSystem *write_cs,
                 double missing_constant, PigCoordSystem *z_cs, 
                 SimpleImage<short int> *idx_out, int idx_out_unit, int do_idx, 
                 SimpleImage<short int> *icm_line_out, 
                 SimpleImage<short int> *icm_samp_out, int icm_out_unit, 
                 int do_icm)
{
    if (current_band == 1) {
        zvopen(out_unit, "OP", "WRITE", "U_NS", nso, "U_NL", nlo, "U_NB", total_band,
               "OPEN_ACT", "SA", "U_ORG", "BSQ", "U_FORMAT", "REAL", "O_FORMAT",
               format, NULL);
        zvplabel(out_unit, 0, 1);
        PigLabelModel *labelModel = mission_instance->createLabelModel(out_unit);
        labelModel->setMosOrtho(file_models, radiometric, brt_corr, cs, z_cs, 
                                write_cs, NULL, NULL, scale, maxx, maxy, 
                                missing_constant, nids, nlo, nso, total_band, 
                                is_dem, do_zup); 

        // idx_out
        if (do_idx) {
            zvopen(idx_out_unit, "OP", "WRITE", "U_FORMAT", "HALF", "U_NS", nso,
                   "U_NL", nlo, "OPEN_ACT", "AS", "U_NB", 1, "U_ORG", "BSQ", 
                   "O_FORMAT", "HALF", NULL);
            PigLabelModel *idxLabel = mission_instance->createLabelModel(idx_out_unit);
            idxLabel->setMosOrtho(file_models, radiometric, brt_corr, cs, z_cs,
                                  write_cs, NULL, NULL, scale, maxx, maxy, 0.0, 
                                  nids, nlo, nso, total_band, is_dem, do_zup);
            idxLabel->setDerivedImageType("IDX_MAP");
            zvplabel(idx_out_unit, 0, 1);
           
            for (int line = 0; line < nlo; line++) {
                zvwrit(idx_out_unit, idx_out->linePtr(line), "BAND", 1, "LINE", 
                       line + 1, NULL);
            }

            zvclose(idx_out_unit, NULL);
        }

        // icm_out
        if (do_icm) {
            zvopen(icm_out_unit, "OP", "WRITE", "U_FORMAT", "HALF", "U_NS", nso,
                   "U_NL", nlo, "OPEN_ACT", "AS", "U_NB", 2, "U_ORG", "BSQ", 
                   "O_FORMAT", "REAL", NULL);
            PigLabelModel *icmLabel = mission_instance->createLabelModel(icm_out_unit);
            icmLabel->setMosOrtho(file_models, radiometric, brt_corr, cs, z_cs, 
                                  write_cs, NULL, NULL, scale, maxx, maxy, 0.0, 
                                  nids, nlo, nso, total_band, is_dem, do_zup);
            icmLabel->setDerivedImageType("ICM_MAP");
            zvplabel(icm_out_unit, 0, 1);

            for (int line = 0; line < nlo; line++) {
                zvwrit(icm_out_unit, icm_line_out->linePtr(line), "BAND", 1, 
                       "LINE", line + 1, NULL);
                zvwrit(icm_out_unit, icm_samp_out->linePtr(line), "BAND", 2,
                       "LINE", line + 1, NULL);
            }
 
            zvclose(icm_out_unit, NULL);
        }
    }

    for (int line = 0; line < nlo; line++) {
        zvwrit(out_unit, image_result->linePtr(line), "BAND", current_band, "LINE",
               line + 1, NULL);
    }
    
    if (current_band == total_band) {    
        zvclose(out_unit, NULL);
    }
}

//handle AVG mode
void avgModeHandler(SimpleImage<float> *image, SimpleImage<float> *image_qm,
                    SimpleImage<float> *image_sp, int xpos, int ypos,
                    double value, int span_line, int span_samp, 
                    double span_angle, double nodata, int nlo, int nso, 
                    short int do_elongated_fill)
{
    double new_value;
    double old_value = image->get(xpos, ypos);

    if ((old_value != nodata) && image_qm->get(xpos, ypos) == 0.0) {
        new_value = (old_value + value) / 2;
    } else {
        new_value = value;
    }

    image->set(xpos, ypos, new_value);
    image_qm->set(xpos, ypos, 0.0);

    if (do_elongated_fill) {
        image_sp->set(0, xpos, ypos, (float)span_line);
        image_sp->set(1, xpos, ypos, (float)span_samp);
        image_sp->set(2, xpos, ypos, (float)span_angle);
    } else {
        image_sp->set(xpos, ypos, (float)span_line);
    }

    if (span_line >= 1 || span_samp >= 1) {
        if (do_elongated_fill) {
            elongated_fill(image, image_qm, image_sp, xpos, ypos, new_value, 
                           nodata, span_line, span_samp, span_angle, nlo, 
                           nso, NULL, 0, 0, NULL, NULL, 0, 0, 0, NULL, 0);
        } else {
            square_fill(image, image_qm, image_sp, xpos, ypos, new_value, 
                        nodata, span_line, nlo, nso, NULL, 0, 0, NULL, NULL, 
                        0, 0, 0, NULL, 0); 
        }
    }
}

//handle MIN mode
void minModeHandler(SimpleImage<float> *image, SimpleImage<float> *image_qm,
                    SimpleImage<float> *image_sp, int xpos, int ypos,
                    double value_compare, double value_set, int span_line, 
                    int span_samp, double span_angle, double nodata, int nlo, 
                    int nso, SimpleImage<short int> *idx_out, int img_idx, 
                    int do_idx, SimpleImage<short int> *icm_line_out, 
                    SimpleImage<short int> *icm_samp_out, int do_icm, int line, 
                    int samp, SimpleImage<float> *image_cp, 
                    short int do_elongated_fill)
{
    double new_value = 0.0;
    double new_cp = 0.0;
    double old_cp = image_cp->get(xpos, ypos);
    int new_idx = 0;
    int new_line = 0;
    int new_samp = 0;

    if (fabs(old_cp - MAXFLOAT) > fabs(MAXFLOAT / 100000000) && 
        image_qm->get(xpos, ypos) == 0.0) {
        new_cp = old_cp <= value_compare ? old_cp : value_compare;
        new_value = old_cp <= value_compare ? image->get(xpos, ypos) : value_set;

        if (do_idx) {
            new_idx = old_cp <= value_compare ? idx_out->get(xpos, ypos) : 
                      img_idx;
        }
        if (do_icm) {
            new_line = old_cp <= value_compare ? icm_line_out->get(xpos, ypos) :
                       line;
            new_samp = old_cp <= value_compare ? icm_samp_out->get(xpos, ypos) :
                       samp;
        }
    } else {
        new_cp = value_compare;
        new_value = value_set;
         
        if (do_idx) {
            new_idx = img_idx;
        }
        if (do_icm) {
            new_line = line; 
            new_samp = samp;
        }
    }

    image->set(xpos, ypos, new_value);
    image_cp->set(xpos, ypos, new_cp);
    image_qm->set(xpos, ypos, 0.0);

    if (do_elongated_fill) {
        image_sp->set(0, xpos, ypos, (float)span_line);
        image_sp->set(1, xpos, ypos, (float)span_samp);
        image_sp->set(2, xpos, ypos, (float)span_angle);
    } else {
        image_sp->set(xpos, ypos, (float)span_line);
    }
    
    if (do_idx) {
        idx_out->set(xpos, ypos, new_idx);
    }
    if (do_icm) {
        icm_line_out->set(xpos, ypos, new_line);
        icm_samp_out->set(xpos, ypos, new_samp);
    }

    if (span_line >= 1 || span_samp >= 1) {
        if (do_elongated_fill) {
            elongated_fill(image, image_qm, image_sp, xpos, ypos, new_value, 
                           nodata, span_line, span_samp, span_angle, nlo, nso, 
                           idx_out, new_idx, do_idx, icm_line_out, icm_samp_out,
                           do_icm, new_line, new_samp, image_cp, new_cp);
        } else {
            square_fill(image, image_qm, image_sp, xpos, ypos, new_value, 
                        nodata, span_line, nlo, nso, idx_out, new_idx, do_idx, 
                        icm_line_out, icm_samp_out, do_icm, new_line, new_samp, 
                        image_cp, new_cp);
        }
    }
}

//handle MAX mode
void maxModeHandler(SimpleImage<float> *image, SimpleImage<float> *image_qm,
                    SimpleImage<float> *image_sp, int xpos, int ypos,
                    double value_compare, double value_set, int span_line, 
                    int span_samp, double span_angle, double nodata, int nlo, 
                    int nso, SimpleImage<short int> *idx_out, int img_idx, 
                    int do_idx, SimpleImage<short int> *icm_line_out,
                    SimpleImage<short int> *icm_samp_out, int do_icm, int line,
                    int samp, SimpleImage<float> *image_cp, 
                    short int do_elongated_fill)
{
    double new_value = 0.0;
    double new_cp = 0.0;
    double old_cp = image_cp->get(xpos, ypos);
    int new_idx = 0;
    int new_line = 0;
    int new_samp = 0;

    if (fabs(old_cp + MAXFLOAT) > fabs(-MAXFLOAT / 100000000) &&
        image_qm->get(xpos, ypos) == 0.0) {
        new_cp = old_cp >= value_compare ? old_cp : value_compare;
        new_value = old_cp >= value_compare ? image->get(xpos, ypos) : value_set;

        if (do_idx) {
            new_idx = old_cp >= value_compare ? idx_out->get(xpos, ypos) : img_idx;
        }
        if (do_icm) {
            new_line = old_cp >= value_compare ? icm_line_out->get(xpos, ypos) : line;
            new_samp = old_cp >= value_compare ? icm_samp_out->get(xpos, ypos) : samp;
        }
    } else {
        new_cp = value_compare;
        new_value = value_set;
    
        if (do_idx) {
            new_idx = img_idx;
        }
        if (do_icm) {
            new_line = line;
            new_samp = samp;
        }
    }

    image->set(xpos, ypos, new_value);
    image_cp->set(xpos, ypos, new_cp);
    image_qm->set(xpos, ypos, 0.0);

    if (do_elongated_fill) {
        image_sp->set(0, xpos, ypos, (float)span_line);
        image_sp->set(1, xpos, ypos, (float)span_samp);
        image_sp->set(2, xpos, ypos, (float)span_angle);
    } else {
        image_sp->set(xpos, ypos, (float)span_line);
    }
 
    if (do_idx) {
        idx_out->set(xpos, ypos, new_idx);
    }
    if (do_icm) {
        icm_line_out->set(xpos, ypos, new_line);
        icm_samp_out->set(xpos, ypos, new_samp);
    }

    if (span_line >= 1 || span_samp >= 1) {
        if (do_elongated_fill) {
            elongated_fill(image, image_qm, image_sp, xpos, ypos, new_value, 
                           nodata, span_line, span_samp, span_angle, nlo, nso, 
                           idx_out, new_idx, do_idx, icm_line_out, icm_samp_out,
                           do_icm, new_line, new_samp, image_cp, new_cp);
        } else {
            square_fill(image, image_qm, image_sp, xpos, ypos, new_value,
                        nodata, span_line, nlo, nso, idx_out, new_idx, do_idx,
                        icm_line_out, icm_samp_out, do_icm, new_line, new_samp,
                        image_cp, new_cp); 
        }
    }
}

//handle FIRST and LAST modes.
void firstAndLastModesHandler(SimpleImage<float> *image,
                              SimpleImage<float> *image_qm,
                              SimpleImage<float> *image_sp, int xpos, int ypos,
                              double value, int span_line, int span_samp, 
                              double span_angle, double nodata, int nlo,
                              int nso, SimpleImage<short int> *idx_out, 
                              int img_idx, int do_idx, 
                              SimpleImage<short int> *icm_line_out,
                              SimpleImage<short int> *icm_samp_out,
                              int do_icm, int line, int samp, 
                              short int do_elongated_fill)
{
    image->set(xpos, ypos, value);
    image_qm->set(xpos, ypos, 0.0);

    if (do_elongated_fill) {
        image_sp->set(0, xpos, ypos, (float)span_line);
        image_sp->set(1, xpos, ypos, (float)span_samp);
        image_sp->set(2, xpos, ypos, (float)span_angle);
    } else {
        image_sp->set(xpos, ypos, (float)span_line);
    }

    if (do_idx) {
        idx_out->set(xpos, ypos, img_idx);
    }
    if (do_icm) {
        icm_line_out->set(xpos, ypos, line);
        icm_samp_out->set(xpos, ypos, samp);
    }

    if (span_line >= 1 || span_samp >= 1) {
        if (do_elongated_fill) {
            elongated_fill(image, image_qm, image_sp, xpos, ypos, value, nodata,
                           span_line, span_samp, span_angle, nlo, nso, idx_out, 
                           img_idx, do_idx, icm_line_out, icm_samp_out, do_icm, 
                           line, samp, NULL, 0);
        } else {
            square_fill(image, image_qm, image_sp, xpos, ypos, value,
                        nodata, span_line, nlo, nso, idx_out, img_idx, do_idx,
                        icm_line_out, icm_samp_out, do_icm, line, samp,
                        NULL, 0); 
        }
    }
}

//handle Closest mode
void closestModeHandler(SimpleImage<float> *image, SimpleImage<float> *image_qm,
                        SimpleImage<float> *image_rm, SimpleImage<float> *image_sp,
                        int xpos, int ypos, double value, double range, 
                        int span_line, int span_samp, double span_angle, 
                        double nodata, int nlo, int nso, 
                        SimpleImage<short int> *idx_out, int img_idx, int do_idx, 
                        SimpleImage<short int> *icm_line_out, 
                        SimpleImage<short int> *icm_samp_out, int do_icm, 
                        int line, int samp, short int do_elongated_fill)
{
    if (range < image_rm->get(xpos, ypos)) {
        image->set(xpos, ypos, value);
        image_qm->set(xpos, ypos, 0.0);
        image_rm->set(xpos, ypos, range);

        if (do_elongated_fill) {
            image_sp->set(0, xpos, ypos, (float)span_line);
            image_sp->set(1, xpos, ypos, (float)span_samp);
            image_sp->set(2, xpos, ypos, (float)span_angle);
        } else {
            image_sp->set(xpos, ypos, (float)span_line);
        }
    
        if (do_idx) {
            idx_out->set(xpos, ypos, img_idx);
        }
        if (do_icm) {
            icm_line_out->set(xpos, ypos, line); 
            icm_samp_out->set(xpos, ypos, samp);
        }

        if (span_line >= 1 || span_samp >= 1) {
            if (do_elongated_fill) {
                elongated_fill_closest(image, image_qm, image_rm, image_sp, 
                    xpos, ypos, value, range, nodata, span_line, span_samp, 
                    span_angle, nlo, nso, idx_out, img_idx, do_idx, 
                    icm_line_out, icm_samp_out, do_icm, line, samp);
            } else {
                square_fill_closest(image, image_qm, image_rm, image_sp, xpos, 
                    ypos, value, range, nodata, span_line, nlo, nso, idx_out, 
                    img_idx, do_idx, icm_line_out, icm_samp_out, do_icm, line, 
                    samp);
            }
        }
    }
}

void square_fill(SimpleImage<float> *image, SimpleImage<float> *image_qm, 
                 SimpleImage<float> *image_sp, int xpos, int ypos, 
                 double fill_value, double nodata, int span, int nlo, int nso, 
                 SimpleImage<short int> *idx_out, int img_idx, int do_idx, 
                 SimpleImage<short int> *icm_line_out, 
                 SimpleImage<short int> *icm_samp_out, int do_icm, int line,
                 int samp, SimpleImage<float> *image_cp, double new_cp)
{
    int radius;
    int new_line;
    int new_samp;  

    for (int win_line = -span; win_line <= span; win_line++) {
        for (int win_samp = -span; win_samp <= span; win_samp++) {
            radius = win_line * win_line + win_samp * win_samp;
            new_line = xpos + win_line;
            new_samp = ypos + win_samp;

            if (new_line <= 0 || new_line >= nlo || new_samp <= 0 ||
                new_samp >= nso) {
                continue;
            }

            if (radius > image_qm->get(new_line, new_samp)) {
                continue;
            }

            image->set(new_line, new_samp, fill_value);
            image_qm->set(new_line, new_samp, radius);
            image_sp->set(new_line, new_samp, (float)span);

            if (image_cp != NULL) {
                image_cp->set(new_line, new_samp, new_cp);
            }
            
            if (do_idx) { 
                idx_out->set(new_line, new_samp, img_idx);
            }
            if (do_icm) {
                icm_line_out->set(new_line, new_samp, line);
                icm_samp_out->set(new_line, new_samp, samp);
            }
        }
    }
}
 
void elongated_fill(SimpleImage<float> *image, SimpleImage<float> *image_qm,
                    SimpleImage<float> *image_sp, int xpos, int ypos, 
                    double fill_value, double nodata, int span_line, 
                    int span_samp, double span_angle, int nlo, int nso, 
                    SimpleImage<short int> *idx_out, int img_idx, int do_idx, 
                    SimpleImage<short int> *icm_line_out, 
                    SimpleImage<short int> *icm_samp_out, int do_icm, int line, 
                    int samp, SimpleImage<float> *image_cp, double new_cp)
{
    if (span_line < 1) span_line = 1;
    if (span_samp < 1) span_samp = 1;

    double radius;
    int new_line;
    int new_samp;  
    float r_line;
    float r_samp;

    double nsin = sin(-1.0 * span_angle);
    double ncos = cos(-1.0 * span_angle);
    int nl = 2 * (span_line) + 1;
    int ns = 2 * (span_samp) + 1;  
    float rnl_float = fabs(ns * sin(span_angle)) + fabs(nl * cos(span_angle));
    float rns_float = fabs(ns * cos(span_angle)) + fabs(nl * sin(span_angle));
    int rnl = (int)ceil(rnl_float);
    int rns = (int)ceil(rns_float);
    int rnlh = (int)round(rnl / 2.0);
    int rnsh = (int)round(rns / 2.0);

    for (int l = -rnlh; l <= rnlh; l++) {
        for (int s = -rnsh; s <= rnsh; s++) {
            float ol = s * nsin + l * ncos;
            float os = s * ncos - l * nsin;
            int oli = (int)round(ol);
            int osi = (int)round(os);
            double sl = span_line;
            double ss = span_samp;
            radius = (ol * ol / (sl * sl) + os * os / (ss * ss));

            if (oli > -span_line && oli <= span_line && osi > -span_samp && 
                osi <= span_samp) {
                new_line = l + xpos;
                new_samp = s + ypos;

                if (new_line <= 0 || new_line >= nlo || new_samp <= 0 ||
                    new_samp >= nso) {
                    continue;
                }	

                if (radius > image_qm->get(new_line, new_samp)) {
                    continue;
            	}

                image->set(new_line, new_samp, fill_value);
                image_qm->set(new_line, new_samp, radius);
                image_sp->set(0, new_line, new_samp, (float)span_line);
                image_sp->set(1, new_line, new_samp, (float)span_samp);
                image_sp->set(2, new_line, new_samp, (float)span_angle);

                if (do_idx) {
                    idx_out->set(new_line, new_samp, img_idx);
                }

                if (do_icm) {
                    icm_line_out->set(new_line, new_samp, line);
                    icm_samp_out->set(new_line, new_samp, samp);
                }
            } 
        }
    }
}

void square_fill_closest(SimpleImage<float> *image, 
                         SimpleImage<float> *image_qm,
                         SimpleImage<float> *image_rm,
                         SimpleImage<float> *image_sp,
                         int xpos, int ypos, double fill_value, double range, 
                         double nodata, int span, int nlo, int nso,  
                         SimpleImage<short int> *idx_out, int img_idx, int do_idx,
                         SimpleImage<short int> *icm_line_out, 
                         SimpleImage<short int> *icm_samp_out, int do_icm, 
                         int line, int samp)
{
    int radius;
    int new_line;
    int new_samp;

    for (int win_line = -span; win_line <= span; win_line++) {
        for (int win_samp = -span; win_samp <= span; win_samp++) {
            radius = win_line * win_line + win_samp * win_samp;
            new_line = xpos + win_line;
            new_samp = ypos + win_samp;

            if (new_line <= 0 || new_line >= nlo || new_samp <= 0 ||
                new_samp >= nso) {
                continue;
            }

            if (radius > image_qm->get(new_line, new_samp)) {
                continue;
            }

            image->set(new_line, new_samp, fill_value);
            image_qm->set(new_line, new_samp, radius);
            image_rm->set(new_line, new_samp, range);
            image_sp->set(new_line, new_samp, (float)span);

            if (do_idx) {
                idx_out->set(new_line, new_samp, img_idx);
            }
            if (do_icm) {
                icm_line_out->set(new_line, new_samp, line);
                icm_samp_out->set(new_line, new_samp, samp);
            }
        }
    }
}

void elongated_fill_closest(SimpleImage<float> *image, 
                            SimpleImage<float> *image_qm,
                            SimpleImage<float> *image_rm, 
                            SimpleImage<float> *image_sp,
                            int xpos, int ypos, double fill_value, double range,
                            double nodata, int span_line, int span_samp, 
                            double span_angle, int nlo, int nso, 
                            SimpleImage<short int> *idx_out, int img_idx, 
                            int do_idx, SimpleImage<short int> *icm_line_out,
                            SimpleImage<short int> *icm_samp_out, int do_icm, 
                            int line, int samp)
{
    int radius;
    int new_line;
    int new_samp;
    float r_line;
    float r_samp;

    double nsin = sin(-1.0 * span_angle);
    double ncos = cos(-1.0 * span_angle);
    int nl = 2 * (span_line) + 1;
    int ns = 2 * (span_samp) + 1;
    float rnl_float = fabs(ns * sin(span_angle)) + fabs(nl * cos(span_angle));
    float rns_float = fabs(ns * cos(span_angle)) + fabs(nl * sin(span_angle));
    int rnl = (int)ceil(rnl_float);
    int rns = (int)ceil(rns_float);
    int rnlh = (int)round(rnl / 2.0);
    int rnsh = (int)round(rns / 2.0);

    for (int l = -rnlh; l <= rnlh; l++) {
        for (int s = -rnsh; s <= rnsh; s++) {
            float ol = s * nsin + l * ncos;
            float os = s * ncos - l * nsin;
            int oli = (int)round(ol);
            int osi = (int)round(os);
            radius = int(ol * ol + os * os);

            if (oli > -span_line && oli <= span_line && osi > -span_samp &&
                osi <= span_samp) {
                new_line = l + xpos;
                new_samp = s + ypos;

                if (new_line <= 0 || new_line >= nlo || new_samp <= 0 ||
                    new_samp >= nso) {
                    continue;
                }

                if (radius > image_qm->get(new_line, new_samp)) {
                    continue;
                }

                image->set(new_line, new_samp, fill_value);
                image_qm->set(new_line, new_samp, radius);
                image_rm->set(new_line, new_samp, range);
                image_sp->set(new_line, new_samp, (float)span_line);
                image_sp->set(new_line, new_samp, (float)span_samp);
                image_sp->set(new_line, new_samp, (float)span_angle);

                if (do_idx) {
                    idx_out->set(new_line, new_samp, img_idx);
                }

                if (do_icm) {
                    icm_line_out->set(new_line, new_samp, line);
                    icm_samp_out->set(new_line, new_samp, samp);
                }
            }
        }
    }
}

SimpleImage<short int> *genStructuringElement(int stru_elem_width, 
                                              int stru_elem_height, 
                                              int nlo, int nso, 
                                              float span_line, float span_samp, 
                                              float span_angle, 
                                              short int do_elongated_fill)
{
    SimpleImage<short int> *stru_elem;
    int count;
    int def;

    if (stru_elem_width < 0 && stru_elem_height < 0) {
        zvmessage("STRU_ELEM_HEIGHT and STRU_ELEM_WIDTH are invalid.", "");
        zabend();
    }
    
    if (stru_elem_height >= nlo || stru_elem_width >= nso) {
        zvmessage("STRU_ELEM_HEIGHT or STRU_ELEM_WIDTH exceeds output's "
                  "boundaries.", "");
        zabend();
    }
    
    stru_elem = new SimpleImage<short int>(stru_elem_height, stru_elem_width);
    initializeSimpleImage(stru_elem, 0);
    
    if (do_elongated_fill) {
        double nsin = sin(-1.0 * span_angle);
        double ncos = cos(-1.0 * span_angle);

        int center = (int)round(stru_elem_height / 2);
        for (int line = -center; line < center; line++) {
            for (int samp = -center; samp < center; samp++) {
                float ol = samp * nsin + line * ncos;
                float os = samp * ncos - line * nsin;

                int oli = (int)round(ol);
                int osi = (int)round(os);
                if (osi > -span_samp && osi < span_samp) {
                     stru_elem->set(line + center, samp + center, 1);
                }
            }
        }
    } else {
        if (zvptst("SQUARE")) {
            //generate square shape
            for (int line = 0; line < stru_elem_height; line++) {
                for (int samp = 0; samp < stru_elem_width; samp++) {
                    stru_elem->set(line, samp, 1);
                }
            }
        } else if (zvptst("DIAMOND")) {
            //generate diamond shape
            for (int line = 0; line < stru_elem_height; line++) {
                int num = 0;
                if (line <= stru_elem_height / 2) {
                    num = 2 * (line + 1) - 1;
                } else {
                    num = 2 * (stru_elem_height - line) - 1;
                }

                int samp_start = (stru_elem_width - num) / 2;
                int samp_end = samp_start + num;
                for (int samp = samp_start; samp < samp_end; samp++) {
                    stru_elem->set(line, samp, 1);
                }
            }
        }
    }
    
    return stru_elem;
}

SimpleImage<float> *erosion(SimpleImage<float> *image, 
                            SimpleImage<float> *span_matrix, double nodata, 
                            double erode_factor, short int do_elongated_fill)
{
    int image_nl = image->getNL();
    int image_ns = image->getNS();
    SimpleImage<float> *eroded_image = new SimpleImage<float>(image_nl, image_ns);
    initializeSimpleImage(eroded_image, nodata);
    printErosionMessage(erode_factor);

#pragma omp parallel for schedule(guided,16) if (omp_on)
    for (int line = 0; line < image_nl; line++) {
        float span_line, span_samp, span_angle;
        int stru_elem_width, stru_elem_height;
        int offset_width, offset_height;  
        SimpleImage<short int> *stru_elem = NULL;
        int old_stru_elem_width = 0;
        int old_stru_elem_height = 0;

        for (int samp = 0; samp < image_ns; samp++) {
            if (do_elongated_fill) {
                span_line = span_matrix->get(0, line, samp);
                span_samp = span_matrix->get(1, line, samp);
                span_angle = span_matrix->get(2, line, samp);
                if (span_line == SPAN_NODATA && span_samp == SPAN_NODATA &&
                    span_angle == SPAN_NODATA) {
                    continue;
                }

                //Figure out the structure element size after rotation
                double nsin = sin(-1.0 * span_angle);
                double ncos = cos(-1.0 * span_angle);
                int nl = 2 * (span_line) + 1;
                int ns = 2 * (span_samp) + 1;
                float rns_f = fabs(ns * cos(span_angle)) - fabs(nl * sin(span_angle));
                float rnl_f = fabs(ns * sin(span_angle)) + fabs(nl * cos(span_angle));
                stru_elem_width = (int)ceil(rns_f);
                stru_elem_height = (int)ceil(rnl_f);                
                if (stru_elem_width < 0) {
                    stru_elem_width = 0;
                } 
                if (stru_elem_height < 0) {
                    stru_elem_height = 0;
                }
                if (stru_elem_width >= stru_elem_height) {
                    stru_elem_height = stru_elem_width;
                } else {
                    stru_elem_width = stru_elem_height;
                }
            } else {
                span_line = span_matrix->get(line, samp);
                span_samp = span_line;
                span_angle = 0.0;
                if (span_line == SPAN_NODATA || span_samp == SPAN_NODATA || 
                    span_angle == SPAN_NODATA) {
                    continue;
                }

                stru_elem_width = 2 * (int)round(span_line * erode_factor) + 1; 
                stru_elem_height = stru_elem_width;
            }
            offset_width = (int)(stru_elem_width / 2);
            offset_height = (int)(stru_elem_height / 2); 

            if (stru_elem == NULL || (stru_elem_width != old_stru_elem_width && 
                stru_elem_height != old_stru_elem_height)) {
                if (stru_elem != NULL) {
                    stru_elem->free();
                    delete stru_elem;
                }
                stru_elem = genStructuringElement(stru_elem_width, stru_elem_height, 
                                                  image_nl, image_ns, span_line, 
                                                  span_samp, span_angle, 
                                                  do_elongated_fill);
                old_stru_elem_width = stru_elem_width;
                old_stru_elem_height = stru_elem_height;
            }

            if (isContained(span_matrix, stru_elem, line, samp, offset_width, 
                            offset_height, image_nl, image_ns, SPAN_NODATA)) {
                eroded_image->set(line, samp, image->get(line, samp));
            }
        }
        if (stru_elem != NULL) {
            stru_elem->free();
            delete stru_elem;
        }
    }

    return eroded_image;
}

int isContained(SimpleImage<float> *image, SimpleImage<short int> *stru_elem,
                int center_nl, int center_ns, int offset_width, int offset_height, 
                int nlo, int nso, double nodata)
{
    int top = center_nl - offset_height;
    int bottom = center_nl + offset_height;
    int left = center_ns - offset_width;
    int right = center_ns + offset_width;

    if (top < 0 || bottom >= nlo || left < 0 || right >= nso) {
        return FALSE;
    }

    int stru_elem_line;
    int stru_elem_samp;
    for (int line = top; line < bottom; line++) {
        for (int samp = left; samp < right; samp++) {
            stru_elem_line = line - top;
            stru_elem_samp = samp - left;
           
            if (stru_elem->get(stru_elem_line, stru_elem_samp) == 1 && 
                image->get(line, samp) == nodata) {
                return FALSE;
            }
        }
    }

    return TRUE;
}

void printErosionMessage(double erode_factor)
{
    char msg[256];
    char *erode_shape;

    zvmessage("=== Erosion Options ===", "");

    if (zvptst("SQUARE")) {
        erode_shape = "square";
    } else {
        erode_shape = "diamond";
    }
    snprintf(msg, 256, "Structuring element shape: %s", erode_shape);
    zvmessage(msg, "");

    snprintf(msg, 256, "Erode factor: %f", erode_factor);
    zvmessage(msg, "");
}

const char *enumToString(OverlayMode overlay_mode)
{
    switch (overlay_mode) {
        case AVG: return "average";
        case FIRST: return "first";
        case LAST: return "last";
        case CLOSEST: return "closest";
        case MIN: return "minimum";
        case MAX: return "maximum";
        default: return "Unknown";
    }
}

float calculateWeightedSpan(double value_x, double value_y, double value_z,
                            double camera_position[], double pixel_angle, 
                            double scale, double max_angle, double max_span) 
{
    double diff_x = value_x - camera_position[0];
    double diff_y = value_y - camera_position[1];
    double diff_z = 0 - camera_position[2];
    double dist = sqrt(diff_x * diff_x + diff_y * diff_y);
    double phi = atan(dist / fabs(diff_z));
    double full_angle = phi + pixel_angle / 2;
    if (full_angle > max_angle) {
        full_angle = max_angle;
        dist = fabs(diff_z) * tan(full_angle - pixel_angle / 2);
    }

    double dist2 = fabs(diff_z) * tan(full_angle);
    double dist3 = fabs(dist2 - dist) * scale;
    if (dist3 > max_span) {
        dist3 = max_span;
    }

    return dist3;
}

float calculateLineSpan(double value_x, double value_y, double value_z, 
                        double camera_position[], double pixel_angle, 
                        double scale, double max_angle, double max_span)
{
    return calculateWeightedSpan(value_x, value_y, value_z, camera_position, 
                                 pixel_angle, scale, max_angle, max_span);
}

float calculateSampleSpan(double value_x, double value_y, double value_z,
                          double camera_position[], double pixel_angle, 
                          double scale, double max_angle, double max_span)
{
    double angle = pixel_angle;
    double diff_x = value_x - camera_position[0];
    double diff_y = value_y - camera_position[1];
    double diff_z = value_z - camera_position[2];
    double r = sqrt(diff_x * diff_x + diff_y * diff_y + diff_z * diff_z);
    if (angle > max_angle) {
        angle = max_angle;
    }

    double sample_span = scale * r * sin(angle / 2);
    if (sample_span > max_span) {
        sample_span = max_span;
    }

    return sample_span;
}
