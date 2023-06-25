/* marstile */
#include "vicmain_c"

#include "mars_support.h"

#include "PigModelBase.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "RadiometryModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include "lbl_image_data.h"
#include "lbl_derived_geometry.h"
#include "del_prop_grp.h"
#include "return_status.h"
#include "SimpleImage.h"

#include <stdio.h>
#include <algorithm>
#include <cmath>
#include <functional>
#include <string>
#include <tuple>
#include <vector>
#include <map>

#define MAX_INPUTS 100
#define MAX_NL 0
#define MAX_NS 0
#define MAX_OUT 5
#define MAX_ZOOM 512 

struct PigTileParms {
    int first_line;
    int nl;
    int first_samp;
    int ns;
    int num_bands;
    int zoom_line;
    int zoom_samp;
    std::string upsample_method;
    std::string product_id;
};

namespace marstile {
    int writeOneParm(int _unit, char *parm_name, void *val, char *type,
								int index);
    void writeTileParms(int _unit, PigTileParms *in_parms, int nids, int index);
    std::vector<PigFileModel*> sort_tiles(int nids, PigFileModel **file_models);
    void upsample_replication(SimpleImage<float> *in, SimpleImage<float> *out,
                              int upsample_x, int upsample_y);
    void upsample_bilinear(SimpleImage<float> *in, SimpleImage<float> *out,
                           int upsample_x, int upsample_y);
    void delete_prop_groups(int unit);
}

static int get_zoom_index(int downsample_factor) {
    int ndx = int(round(log(downsample_factor) / log(2)));
    if ((1 << ndx) != downsample_factor) {
        //downsample_factor was not an exact power of two
        //this happens sometimes e.g. with monochrome thumbnail tiles on M20
        //so don't use this tile for any of the power of two fixed-res outputs
        return 0;
    }
    ndx += 1; //first entry in arrays is reserved for multires output
    if (ndx >= MAX_OUT) {
        //downsample_factor was higher than supported
        //so don't use this tile for any of the power of two fixed-res outputs
        return 0;
    }
    return ndx;
}

void main44()
{
    const size_t msgLen = 500;
    char msg[msgLen];
    int i, j, k, b;
    int status, count, def;
    int nids;
    int use_bilinear, use_replication;
    int border, use_file;

    char mission[64], instrument[64];

    std::function<void(SimpleImage<float>*, SimpleImage<float>*, int, int)>
                       f_upsample = NULL;

    // Inputs
    char format[10];
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigLabelModel *labelModel = NULL;
    PigLabelModel *idmLabelModel = NULL, *idxLabelModel = NULL;
    int homogeneous_inputs = TRUE;
    PigCoordSystem *input_cs;
    int border_size = 8;                       // default value

    SimpleImage<float> *f_img = new SimpleImage<float>(3, MARS_MAX_NL,
						       MARS_MAX_NS);
    SimpleImage<float> *full_res_img = new SimpleImage<float>(3, MARS_MAX_NL,
							      MARS_MAX_NS);

    // Outputs
    std::vector<int> unit_out;
    int zoomX, zoomY, zoom; // optional zoom param
    int band;
    // Going to use 5 values for these params
    // to represent all possible output zoom levels
    int nbo[MAX_OUT] = { 0 };
    int nlo[MAX_OUT] = { 0 };
    int nso[MAX_OUT] = { 0 };
    // set the number high so it goes low after checking all of the tiles
    int slo[5] = { 10000, 10000, 10000, 10000, 10000 };
    int sso[5] = { 10000, 10000, 10000, 10000, 10000 };
    LblImageData_typ ImageData;
    PigCameraModel *camera_out;
    std::vector<int> use_out;
    int unit_idx_out = 0, unit_idm_out = 0;

    bool do_print = true;                // This is true if camera models exist and
                                         // the input info dump should be printed

    zvmessage("MARSTILE version 1", "");

    /*
        Get the input file list, and set up initial camera/pointing models for
        each input, as well as the radiometry model which will go unused.
    */
    mars_setup(nids, file_models, camera_in, pointing_in, NULL,
	       input_cs, mission, instrument, homogeneous_inputs, MAX_NL,
               MAX_NS, MAX_INPUTS);
    if (camera_in[0] == NULL) {
        zvmessage("WARNING: Camera models were not found for INP, proceeding without writing out FOV or camera model.", "");
        do_print = false;
    }


    // Get the mission pointer.
    PigMission *m = PigMission::getMissionObject(mission);

    std::string upsample_str = "NONE";
    use_replication = zvptst("REPLICATION");
    use_bilinear = zvptst("BILINEAR");
    if (use_bilinear) {
        upsample_str = "BILINEAR";
        f_upsample = &marstile::upsample_bilinear;
    } else if (use_replication) {
        upsample_str = "REPLICATION";
        f_upsample = &marstile::upsample_replication;
    } else {
        f_upsample = &marstile::upsample_replication;
    }

    if (do_print) {
        mars_print_inputs(nids, pointing_in, camera_in, file_models,
                          homogeneous_inputs, mission, instrument);
    }

    // Get a vector of parameters for each tile
    std::vector<PigTileParms*> parm_vec;

    // Use the camera mapping file to determine the maximum dimensions that the
    // reconstructed image can take
    int full_nlo = 3840;             // defaults
    int full_nso = 5120;
    PigXerces::initialize();
    PigCameraMapper *map = new PigCameraMapper(NULL, m->getHostID());
    if (map == NULL) {
        zvmessage("Unable to create CameraMapper.  Assuming 1024x1024 "
                  "nominal camera","");
    } else {
        PigCameraMapEntry *entry = map->findFromID(instrument);
        if (entry == NULL)
            zvmessage("Unable to find camera in CameraMap.  Assuming "
                      "5120x3840 nominal camera","");
        else {
            full_nlo = entry->getNL();
            full_nso = entry->getNS();
        }
    }

    // Get the size of the output image based on the total size of the tiles.
    zoomX = MAX_ZOOM;
    zoomY = MAX_ZOOM;
    for (k = 0; k < nids; k++) {
        int zoom_line = file_models[k]->getDownsampleYFactor(1);
        int zoom_samp = file_models[k]->getDownsampleXFactor(1);
        int num_bands = file_models[k]->getNB();
        // Initialize a PigTileParms struct with some fields not known
        // yet, these will be filled in later.
        PigTileParms *parms = new PigTileParms({1, 1, 1, 1, num_bands,
                              zoom_line, zoom_samp, "NONE",
                              std::string(file_models[k]->getProductId()) });

        int zoom_ndx = get_zoom_index(zoom_line);

        //this loop is prolly not the clearest code at first sight
        //I didn't originally write it, I just made it slightly more fun
        //note that 0 <= zoom_ndx < MAX_OUT
        //what the loop does is
        //(a) always update nbo[0]
        //(b) update nbo[zoom_ndx] iff zoom_ndx > 0
        //(c) not touch any other entries in nbo
        for (int ndx = 0; ndx <= zoom_ndx; ndx += max(1, zoom_ndx)) {
            nbo[ndx] = num_bands > nbo[ndx] ?
                       num_bands : nbo[ndx];
        }

        parm_vec.push_back(parms);
        zoomY = zoom_line < zoomY ?
                zoom_line : zoomY;
        zoomX =  zoom_samp < zoomX ?
                 zoom_samp : zoomX;
    }

    status = zvparm("BAND", &band, &count, &def, 1, 0);
    if (count == 0) {
        // no input band specified; process all bands
        snprintf(msg, msgLen, "Number of bands to be processed is (%d)", nbo[0]);
        zvmessage(msg, "");
        band = 0;
    } else {
        // check if input band number is greater than number of bands in input
        if (band > nbo[0]) {
            snprintf(msg, msgLen,
                    "Input band (%d) is greater than the number of bands in input image. Band set to 1.", band);
            zvmessage(msg, "");
            band = 1;
        }
        for (int x = 0; x < MAX_OUT; x++) {
            nbo[x] = 1;
        }
    }

    char idx_filename[PIG_MAX_FILENAME_SIZE];
    char idm_filename[PIG_MAX_FILENAME_SIZE];

    // check whether or not to produce reconstruction meta-products
    zvp("IDX_OUT", idx_filename, &count);
    int do_idx = (count != 0);
    zvp("IDM_OUT", idm_filename, &count);
    int do_idm = (count != 0);

    int trim_zero = zvptst("TRIM_ZERO");

    status = zvparm("ZOOM", &zoom, &count, &def, 1, 0);
    if (count != 0 && zoom > 0 && zoom <= 8) {
        // There is no downsampling, so zoom can only be used to increase
        // the resolution of the highest resolution tiles.
        zoomX = zoomX > zoom ? zoom : zoomX;
        zoomY = zoomY > zoom ? zoom : zoomY;
    }


    zvp("BORDER", &border, &count);
    snprintf(msg, msgLen, "Tile borders are (%d) pixels.", border);
    zvmessage(msg, "");

    // Create a map between the filemodels and tile parameters
    // This allows the parameters to get sorted with the file
    // models.
    std::map<PigFileModel*, PigTileParms*> file_map;
    for (k = 0; k < nids; k++) {
        // Line and sample are 1-base in the label, so convert them to 0-base
        int fl = int(file_models[k]->getFirstLine(1) - 1);
        int fs = int(file_models[k]->getFirstLineSample(1) - 1);
        int zoom_line = file_models[k]->getDownsampleYFactor(1);
        int zoom_samp = file_models[k]->getDownsampleXFactor(1);
        double el = file_models[k]->getNL() * zoom_line + fl;
        double es = file_models[k]->getNS() * zoom_samp + fs;
        // Adjust the fl el fs es according to the label
        if (fl > border) {
            fl += zoom_line * border;
        }
        if (fs > border) {
            fs += zoom_samp * border;
        }
        if (el < full_nlo - border) {
            el -= zoom_line * border;
        }
        if (es < full_nso - border) {
            es -= zoom_samp * border;
        }

        // Store the initial borders in the TileParms. Convert back to 1-index
        parm_vec[k]->first_line = fl + 1;
        parm_vec[k]->first_samp = fs + 1;
        parm_vec[k]->nl = el - fl;
        parm_vec[k]->ns = es - fs;
	// If our tile isn't big enough, ignore it
	if (parm_vec[k]->nl < 0 || parm_vec[k]->ns < 0) {
	    parm_vec[k]->nl = 0;
	    parm_vec[k]->ns = 0;
	}

        if (zoomX != zoom_samp || zoomY != zoom_line) {
            parm_vec[k]->upsample_method = upsample_str;
        }
        file_map.insert({file_models[k], parm_vec[k]});

        int zoom_ndx = get_zoom_index(zoom_line);

        //see comments above about looping like this with zoom_ndx
        for (int ndx = 0; ndx <= zoom_ndx; ndx += max(1, zoom_ndx)) {
            slo[ndx] = fl < slo[ndx] ?
                       fl : slo[ndx];
            sso[ndx] = fs < sso[ndx] ?
                       fs : sso[ndx];
            nlo[ndx] = int(el) > nlo[ndx] ?
                       int(el) : nlo[ndx];
            nso[ndx] = int(es) > nso[ndx] ?
                       int(es) : nso[ndx];
        }
    }
    std::vector<char*> out_parm_name = {"OUT", "OUT_1X", "OUT_2X",
                                        "OUT_4X", "OUT_8X"};
    std::vector<int> zoom_level = {zoomX, 1, 2, 4, 8};
    // nlo is currently the ending line, so subtracting the starting line
    // to get the true number of lines.
    for (int x = 0; x < MAX_OUT; x++) {
        char out_fname[PIG_MAX_FILENAME_SIZE];
        nlo[x] -= slo[x];
        nso[x] -= sso[x];
        zvp(out_parm_name[x], out_fname, &count);
        // Determine which zoom levels can actually be generated
        if ((nlo[x] > 0 || nso[x] > 0) && count != 0) {
            use_out.push_back(x);
        }
    }

    zvp("USE_FILE", &use_file, &count);
    if (use_file > nids - 1) {
        use_file = 0;
    }

    std::vector<PigFileModel*> sorted_files = marstile::sort_tiles(nids,
                                                                   file_models);

    zvselpiu(file_models[use_file]->getUnit());

    // if doing idx and/or idm files, initialize the matrices. There will only be one of each
    // as these outputs only apply to the multi-resolution reconstruction product.
    if (do_idx) {
        zvunit(&unit_idx_out, "IDX", 1, "U_NAME", idx_filename, NULL);
        zvopen(unit_idx_out, "OP", "WRITE", "U_FORMAT", "HALF",
               "U_NS", nso[0] / zoom_level[0], "U_NL", nlo[0] / zoom_level[0],
               "OPEN_ACT", "AS", "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT",
               "HALF", NULL);
	idxLabelModel = m->createLabelModel(unit_idx_out);
        zvplabel(unit_idx_out, 0, 1);
        std::vector<short int> zero_line(nso[0] / zoom_level[0], 0);
        for (int j = 0; j < nlo[0] / zoom_level[0]; j++) {
            zvwrit(unit_idx_out, zero_line.data(), "LINE", j+1, NULL);
        }
        zvclose(unit_idx_out, NULL);
        zvopen(unit_idx_out, "OP", "UPDATE", "U_FORMAT", "HALF",
               "U_NS", nso[0] / zoom_level[0], "U_NL", nlo[0] / zoom_level[0],
               "OPEN_ACT", "AS", "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT",
               "HALF", NULL);
    }
    if (do_idm) {
        zvunit(&unit_idm_out, "IDM", 1, "U_NAME", idm_filename, NULL);
        zvopen(unit_idm_out, "OP", "WRITE", "U_FORMAT", "BYTE",
	       "U_NS", nso[0] / zoom_level[0], "U_NL", nlo[0] / zoom_level[0], 
               "OPEN_ACT", "AS", "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT",
               "BYTE", NULL);
        idmLabelModel = m->createLabelModel(unit_idm_out);
        zvplabel(unit_idm_out, 0, 1);
	std::vector<unsigned char> zero_line(nso[0] / zoom_level[0], 0);
        for (int j = 0; j < nlo[0] / zoom_level[0]; j++) {
            zvwrit(unit_idm_out, zero_line.data(), "LINE", j + 1, NULL);
        }
        zvclose(unit_idm_out, NULL);
        zvopen(unit_idm_out, "OP", "UPDATE", "U_FORMAT", "BYTE",
               "U_NS", nso[0] / zoom_level[0], "U_NL", nlo[0] / zoom_level[0],
               "OPEN_ACT", "AS", "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT",
               "BYTE", NULL);
    }

    for (auto out = use_out.begin(); out != use_out.end(); out++) {
        int temp_unit = 0;
        char out_fname[PIG_MAX_FILENAME_SIZE];
        if (*out != 0) {
            zvp(out_parm_name[*out], out_fname, &count);
            if (count == 0) {
                continue;
            }
            zvunit(&temp_unit, out_parm_name[*out], 1,
                   "U_NAME", out_fname, NULL);
        } else {
            zvunit(&temp_unit, out_parm_name[*out], 1, NULL);
        }

        unit_out.push_back(temp_unit);
        zvopen(unit_out.back(), "op", "write",
               "u_ns", nso[*out] / zoom_level[*out],
               "u_nl", nlo[*out] / zoom_level[*out],
               "u_nb", nbo[*out], "open_act", "sa",
               "u_org", "bsq", "u_format", "real",
               NULL);

        labelModel = m->createLabelModel(unit_out.back());
        if (camera_in[use_file] != NULL) {
            PigCameraModel *camera_mod = camera_in[use_file]->clone();

            if (camera_mod != NULL) {
                // We have to do this first before sorting so that the order of the
                // file model and camera model lists matches.
                bool success = labelModel->setGeometryLarger(file_models[use_file],
							     camera_mod, zoom_level[*out], 
                                                             zoom_level[*out], slo[*out],
                                                             sso[*out]);
            }

        
            // Compute the FOV of the output based on the "use file" camera
            // model. Then write the FOV values to the label.
            PigPoint origin;
            PigVector direction_left, direction_right;
            PigVector direction_up, direction_down;
            double az_fov, el_fov;

	    int act_nlo = nlo[*out] / zoom_level[*out];
	    int act_nso = nso[*out] / zoom_level[*out];

            camera_mod->LStoLookVector(double(act_nlo) / 2.0, 0.0,
                                       origin, direction_left, input_cs);
            camera_mod->LStoLookVector(double(act_nlo) / 2.0, double(act_nso),
                                       origin, direction_right, input_cs);
            az_fov = PigRad2Deg(acos(direction_left % direction_right));

            camera_mod->LStoLookVector(0.0, double(act_nso) / 2.0,
                                       origin, direction_up, input_cs);
            camera_mod->LStoLookVector(double(act_nlo), double(act_nso) / 2.0,
                                       origin, direction_down, input_cs);
            el_fov = PigRad2Deg(acos(direction_up % direction_down));

            char *fov_unit = "deg";

            status = zldel(unit_out.back(), "PROPERTY", "AZIMUTH_FOV",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_out.back(), "PROPERTY", "AZIMUTH_FOV",
                           &az_fov, "FORMAT", "DOUB", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_out.back(), "PROPERTY", "AZIMUTH_FOV__UNIT",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_out.back(), "PROPERTY", "AZIMUTH_FOV__UNIT", fov_unit,
                           "FORMAT", "STRING", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_out.back(), "PROPERTY", "ELEVATION_FOV",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_out.back(), "PROPERTY", "ELEVATION_FOV",
                           &el_fov, "FORMAT", "DOUB", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_out.back(), "PROPERTY", "ELEVATION_FOV__UNIT",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_out.back(), "PROPERTY", "ELEVATION_FOV__UNIT", fov_unit,
                           "FORMAT", "STRING", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);

            if (do_idx and *out == 0) {

                status = zldel(unit_idx_out, "PROPERTY", "AZIMUTH_FOV",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
                status = zladd(unit_idx_out, "PROPERTY", "AZIMUTH_FOV",
                           &az_fov, "FORMAT", "DOUB", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
                status = zldel(unit_idx_out, "PROPERTY", "AZIMUTH_FOV__UNIT",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
                status = zladd(unit_idx_out, "PROPERTY", "AZIMUTH_FOV__UNIT", fov_unit,
                           "FORMAT", "STRING", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
                status = zldel(unit_idx_out, "PROPERTY", "ELEVATION_FOV",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
                status = zladd(unit_idx_out, "PROPERTY", "ELEVATION_FOV",
                           &el_fov, "FORMAT", "DOUB", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
                status = zldel(unit_idx_out, "PROPERTY", "ELEVATION_FOV__UNIT",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
                status = zladd(unit_idx_out, "PROPERTY", "ELEVATION_FOV__UNIT", fov_unit,
                           "FORMAT", "STRING", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
	    }

            if (do_idm and *out == 0) {

                status = zldel(unit_idm_out, "PROPERTY", "AZIMUTH_FOV",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
                status = zladd(unit_idm_out, "PROPERTY", "AZIMUTH_FOV",
                           &az_fov, "FORMAT", "DOUB", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
                status = zldel(unit_idm_out, "PROPERTY", "AZIMUTH_FOV__UNIT",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
                status = zladd(unit_idm_out, "PROPERTY", "AZIMUTH_FOV__UNIT", fov_unit,
                           "FORMAT", "STRING", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
                status = zldel(unit_idm_out, "PROPERTY", "ELEVATION_FOV",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
                status = zladd(unit_idm_out, "PROPERTY", "ELEVATION_FOV",
                           &el_fov, "FORMAT", "DOUB", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
                status = zldel(unit_idm_out, "PROPERTY", "ELEVATION_FOV__UNIT",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
                status = zladd(unit_idm_out, "PROPERTY", "ELEVATION_FOV__UNIT", fov_unit,
                           "FORMAT", "STRING", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
	    }


	    // Compute the instrument az/el in both rover and site frames

	    PigCoordSystem *site_cs = m->getCoordSystem(file_models[use_file],
									"SITE");

	    // Inst AZ/EL is defined as center of the nominal camera

	    int nl, ns;
	    PigFileModel::getNominalCameraSize(m, instrument, nl, ns);

	    // Adjust it for downsample and subframe to match actual cmod

	    double x_center = (ns / 2.0 - sso[*out]) /
					(double)zoom_level[*out] - 0.5;
	    double y_center = (nl / 2.0 - slo[*out]) /
					(double)zoom_level[*out] - 0.5;

	    PigVector look;
	    camera_mod->LStoLookVector(y_center,x_center,origin,look,input_cs);
	    double rover_az = PigRad2Deg(input_cs->getAz(look));
	    double rover_el = PigRad2Deg(input_cs->getEl(look));

	    if (rover_az >= 360.0) {
		int n = (int)(rover_az / 360);
		rover_az -= n * 360.0;
	    }
	    else if (rover_az < 0.0) {
		int n = (int)(rover_az / -360);
		rover_az += (n+1) * 360.0;
	    }

	    double site_az, site_el;
	    site_cs->convertAzElDegrees(rover_az, rover_el,
					site_az, site_el, input_cs);

	    if (site_az >= 360.0) {
		int n = (int)(site_az / 360);
		site_az -= n * 360.0;
	    }
	    else if (site_az < 0.0) {
		int n = (int)(site_az / -360);
		site_az += (n+1) * 360.0;
	    }

	    LblDerivedGeometry_typ lblDG;
	    memset(&lblDG, 0, sizeof(lblDG));

	    lblDG.LanderInstrumentAzimuth.Value = rover_az;
	    lblDG.LanderInstrumentAzimuth.Valid = 1;
	    lblDG.LanderInstrumentElevation.Value = rover_el;
	    lblDG.LanderInstrumentElevation.Valid = 1;
	    strcpy(lblDG.InstrumentAzimuthUnit.Value, "deg");
	    lblDG.InstrumentAzimuthUnit.Valid = 1;
	    strcpy(lblDG.InstrumentElevationUnit.Value, "deg");
	    lblDG.InstrumentElevationUnit.Valid = 1;

	    LblDerivedGeometryApi(unit_out.back(), LBL_WRITE, &lblDG, 1,
				"ROVER_DERIVED_GEOMETRY_PARMS");
            if (do_idx and *out == 0) {
	        LblDerivedGeometryApi(unit_idx_out, LBL_WRITE, &lblDG, 1,
				"ROVER_DERIVED_GEOMETRY_PARMS");
	    }
            if (do_idm and *out == 0) {
	        LblDerivedGeometryApi(unit_idm_out, LBL_WRITE, &lblDG, 1,
				"ROVER_DERIVED_GEOMETRY_PARMS");
	    }

	    memset(&lblDG, 0, sizeof(lblDG));
	    lblDG.SiteInstrumentAzimuth.Value = site_az;
	    lblDG.SiteInstrumentAzimuth.Valid = 1;
	    lblDG.SiteInstrumentElevation.Value = site_el;
	    lblDG.SiteInstrumentElevation.Valid = 1;
	    strcpy(lblDG.InstrumentAzimuthUnit.Value, "deg");
	    lblDG.InstrumentAzimuthUnit.Valid = 1;
	    strcpy(lblDG.InstrumentElevationUnit.Value, "deg");
	    lblDG.InstrumentElevationUnit.Valid = 1;

	    LblDerivedGeometryApi(unit_out.back(), LBL_WRITE, &lblDG, 1,
				"SITE_DERIVED_GEOMETRY_PARMS");
            if (do_idx and *out == 0) {
	        LblDerivedGeometryApi(unit_idx_out, LBL_WRITE, &lblDG, 1,
				"SITE_DERIVED_GEOMETRY_PARMS");
	    }
            if (do_idm and *out == 0) {
	        LblDerivedGeometryApi(unit_idm_out, LBL_WRITE, &lblDG, 1,
				"SITE_DERIVED_GEOMETRY_PARMS");
	    }


	    // Fix up other labels

            if (do_idx and *out == 0) {
                PigCameraModel *camera_mod = camera_in[use_file]->clone();
                bool success = idxLabelModel->setGeometryLarger(file_models[use_file],
							     camera_mod, zoom_level[0], 
                                                             zoom_level[0], slo[0],
                                                             sso[0]);

            }
            if (do_idm and *out == 0) {
                PigCameraModel *camera_mod = camera_in[use_file]->clone();
                bool success = idmLabelModel->setGeometryLarger(file_models[use_file],
							     camera_mod, zoom_level[0], 
                                                             zoom_level[0], slo[0],
                                                             sso[0]);

            }
        }

	// Fix up labels unrelated to the cmod

        zvplabel(unit_out.back(), 0, 1);
	labelModel->setIdentification(NULL);
	labelModel->writeProductIds(file_models, nids);
        status = zldel(unit_out.back(),
			"PROPERTY","EXPOSURE_READOUT_COUNT",
                        "PROPERTY", "INSTRUMENT_STATE_PARMS",
			"ERR_ACT", "", NULL);

        marstile::delete_prop_groups(unit_out.back());

        if (do_idx and *out == 0) {
            zvplabel(unit_idx_out, 0, 1);
	    idxLabelModel->setIdentification(NULL);
            idxLabelModel->setDerivedImageType("IDX_MAP");
	    idxLabelModel->writeProductIds(file_models, nids);

            status = zldel(unit_idx_out,
			       "PROPERTY","EXPOSURE_READOUT_COUNT",
                                "PROPERTY", "INSTRUMENT_STATE_PARMS",
			        "ERR_ACT", "", NULL);
            marstile::delete_prop_groups(unit_idx_out);
        }

        if (do_idm and *out == 0) {
            zvplabel(unit_idm_out, 0, 1);
	    idmLabelModel->setIdentification(NULL);
            idmLabelModel->setDerivedImageType("IDM_MAP");
	    idmLabelModel->writeProductIds(file_models, nids);

            status = zldel(unit_idm_out,
			       "PROPERTY","EXPOSURE_READOUT_COUNT",
                               "PROPERTY", "INSTRUMENT_STATE_PARMS",
			       "ERR_ACT", "", NULL);
            marstile::delete_prop_groups(unit_idm_out);
        }

        // While we're at it, write the detector line/sample values
        // out to the label also
        int start_line = slo[*out] + 1;
        int start_samp = sso[*out] + 1;
        status = zldel(unit_out.back(), "PROPERTY", "DETECTOR_LINES",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
        status = zladd(unit_out.back(), "PROPERTY", "DETECTOR_LINES",
                       &nlo[*out], "FORMAT", "INT", "MODE", "REPLACE",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
        status = zldel(unit_out.back(), "PROPERTY", "DETECTOR_LINE_SAMPLES",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
        status = zladd(unit_out.back(), "PROPERTY", "DETECTOR_LINE_SAMPLES",
                       &nso[*out], "FORMAT", "INT", "MODE", "REPLACE",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
        status = zldel(unit_out.back(), "PROPERTY", "DETECTOR_FIRST_LINE",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
        status = zladd(unit_out.back(), "PROPERTY", "DETECTOR_FIRST_LINE",
                       &start_line, "FORMAT", "INT", "MODE", "REPLACE",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
        status = zldel(unit_out.back(), "PROPERTY", "DETECTOR_FIRST_LINE_SAMPLE",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
        status = zladd(unit_out.back(), "PROPERTY", "DETECTOR_FIRST_LINE_SAMPLE",
                       &start_samp, "FORMAT", "INT", "MODE", "REPLACE",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);

        if (do_idx and *out == 0) {
            status = zldel(unit_idx_out, "PROPERTY", "DETECTOR_LINES",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_idx_out, "PROPERTY", "DETECTOR_LINES",
                           &nlo[0], "FORMAT", "INT", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_idx_out, "PROPERTY", "DETECTOR_LINE_SAMPLES",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_idx_out, "PROPERTY", "DETECTOR_LINE_SAMPLES",
                           &nso[0], "FORMAT", "INT", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_idx_out, "PROPERTY", "DETECTOR_FIRST_LINE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_idx_out, "PROPERTY", "DETECTOR_FIRST_LINE",
                           &start_line, "FORMAT", "INT", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_idx_out, "PROPERTY", "DETECTOR_FIRST_LINE_SAMPLE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_idx_out, "PROPERTY", "DETECTOR_FIRST_LINE_SAMPLE",
                           &start_samp, "FORMAT", "INT", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
        }
        if (do_idm and *out == 0) {
            status = zldel(unit_idm_out, "PROPERTY", "DETECTOR_LINES",
                       "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_idm_out, "PROPERTY", "DETECTOR_LINES",
                           &nlo[0], "FORMAT", "INT", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_idm_out, "PROPERTY", "DETECTOR_LINE_SAMPLES",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_idm_out, "PROPERTY", "DETECTOR_LINE_SAMPLES",
                           &nso[0], "FORMAT", "INT", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_idm_out, "PROPERTY", "DETECTOR_FIRST_LINE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_idm_out, "PROPERTY", "DETECTOR_FIRST_LINE",
                           &start_line, "FORMAT", "INT", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
            status = zldel(unit_idm_out, "PROPERTY", "DETECTOR_FIRST_LINE_SAMPLE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", "ERR_ACT", "", NULL);
            status = zladd(unit_idm_out, "PROPERTY", "DETECTOR_FIRST_LINE_SAMPLE",
                           &start_samp, "FORMAT", "INT", "MODE", "REPLACE",
                           "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
        }

        // Only initialize the image if the border is large, to safeguard against
        // blank areas of the image when writing the tiles. 
        snprintf(msg, msgLen, "Initializing image %s...", out_parm_name[*out]);
        zvmessage(msg, "");
        std::vector<float> zero_line(nso[*out], 0.0);
        float *zero_line_ptr = zero_line.data();
        for (int j = 1; j <= nlo[*out]; j++) {
            for (int b = 1; b <= nbo[*out]; b++) {
	      zvwrit(unit_out.back(), zero_line.data(), "BAND", b,
                       "LINE", j, NULL);
            }
        }
    }

    PigTileParms *parms;
    PigTileParms *default_parms;
    int samp_prev = 0;
    int upsample_x, upsample_y;

    int product_num[MAX_INPUTS];	// index by ndx
    memset(product_num, 0, sizeof(product_num));

    // Loop over the tiles

    for (auto filemodel = sorted_files.begin(); filemodel != sorted_files.end();
         ++filemodel) { 
        mars_read_inputs(0, 0, &(*filemodel), &f_img, MAX_NL, MAX_NS, 
                         band, NULL, NULL);

        default_parms = file_map[(*filemodel)];

        int ndx = 0;

	// Loop over the outputs, putting the tile in each output

        for (auto out = use_out.begin(); out != use_out.end(); out++) {
            parms = new PigTileParms(*default_parms);
            // full-res (or highest-res) output
            if (*out == 0) {
	              // if the input has been downsampled -- counterintuitive
                if (parms->upsample_method.compare("NONE")) {
                    // The factor to upsample by is the original downsampling rate
                    // over the target downsampling rate, usually 1 (if full-res)
                    upsample_x = parms->zoom_samp / zoomX;
                    upsample_y = parms->zoom_line / zoomY;
                    f_upsample(f_img, full_res_img, upsample_x, upsample_y);
                } else {
                    parms->upsample_method = "NONE";
                    upsample_x = 1;
                    upsample_y = 1;
                    f_upsample(f_img, full_res_img, upsample_x, upsample_y);
                }
            } else if (int(pow(2, *out - 1)) == parms->zoom_samp) {
                parms->upsample_method = "NONE";
                upsample_x = 1;
                upsample_y = 1;
                f_upsample(f_img, full_res_img, upsample_x, upsample_y);
            } else {
                // if the output does not include this tile, move on
                ndx++;
                continue;
            }

	    // If the tile is empty, move on
	    if (parms->ns == 0 || parms->nl == 0) {
		ndx++;
		continue;
	    }

            // Convert back to 0-base, and convert to scaled coordinates
            int border_top = parms->first_line - 1 >= border * zoom_level[*out] ?
	                     border : 0;
            int border_left = parms->first_samp - 1 >= border * zoom_level[*out] ?
	                      border : 0;
            if (*out == 0) {
	        border_top *= upsample_y;
                border_left *= upsample_x;
            }
            int first_line_in = (parms->first_line - 1) / zoom_level[*out];
            int first_samp_in = (parms->first_samp - 1) / zoom_level[*out];
            int last_line_in = parms->nl / zoom_level[*out] + first_line_in;
            int last_samp_in = parms->ns / zoom_level[*out] + first_samp_in;

            int scaled_nso = nso[*out] / zoom_level[*out];
            int scaled_nlo = nlo[*out] / zoom_level[*out];
            int scaled_slo = slo[*out] / zoom_level[*out];
            int scaled_sso = sso[*out] / zoom_level[*out];

            snprintf(msg, msgLen, "%s Borders are: %d to %d (line), %d to %d (sample)",
                    out_parm_name[*out], first_line_in, last_line_in, first_samp_in,
                    last_samp_in);
            zvmessage(msg, "");

            if (*out == 0) {
                if (do_idx) {
		    // fill in the value of the product_num from the parms into the
                    // area occupied by the tile
                    std::vector<short int> line(parms->ns / zoom_level[0], product_num[ndx]+1);
                    for (int j = 0; j < parms->nl / zoom_level[0]; j++) {
                        zvwrit(unit_idx_out, line.data(), "LINE", j + first_line_in 
                               - scaled_slo + 1, "SAMP", first_samp_in - scaled_sso + 1,
                               "NSAMPS", parms->ns / zoom_level[0], NULL);
                    }
		}
                if (do_idm) {
		    std::vector<unsigned char> line_zoom(parms->ns / zoom_level[0], parms->zoom_line);
                    for (int j = 0; j < parms->nl / zoom_level[0]; j++) {
                        zvwrit(unit_idm_out, line_zoom.data(), "LINE", j + first_line_in
                               - scaled_slo + 1, "SAMP", first_samp_in - scaled_sso + 1, 
                               "NSAMPS", parms->ns / zoom_level[0], NULL);
                    }
                    
                }
	    }

            // printf("slo: %d, sso: %d, zoom_level: %d\n", slo[*out], sso[*out], zoom_level[*out]);
            for (j = 0; j < parms->nl / zoom_level[*out]; j++) {
                int nsamp = parms->ns / zoom_level[*out];
		int start_samp = 0;
                for (int b = 0; b < nbo[*out]; b++) {
		    float *full_line = full_res_img->linePtr(b % parms->num_bands, j + border_top);
                    // b == 0 only is not the cleanest solution, but no one will notice the edge cases
                    if (trim_zero && upsample_x == 1 && b == 0) {
                        for (int s = border_left; s < nsamp; s++) {
                            if (full_line[s] == 0) {
                                start_samp++;
                            } else {
                               break;
                            }
                        }
                        for (int s = nsamp - 1; s > start_samp; s--) {
                            if (full_line[s+border_left] == 0) {
                                nsamp--;
                            } else {
                                break;
                            }
                        }
                        nsamp = nsamp - start_samp;
                    }
                    if (nsamp <= 0) {
                        continue;
                    }
                    std::vector<float>  line(nsamp, 0.0);
                    for (int s = 0; s < nsamp; s++) {
                        line[s] = full_line[start_samp + s + border_left];
                    }
                    zvwrit(unit_out[ndx], line.data(), "BAND", b + 1,
                           "LINE", j + first_line_in - scaled_slo + 1, "SAMP",
                           first_samp_in - scaled_sso + start_samp + 1,
                           "NSAMPS", nsamp, NULL);
                }
            }
            marstile::writeTileParms(unit_out[ndx], parms, nids, product_num[ndx]);
	    if (*out == 0) {
		if (do_idx) {
		    marstile::writeTileParms(unit_idx_out, parms, nids, product_num[ndx]);
		}
		if (do_idm) {
		    marstile::writeTileParms(unit_idm_out, parms, nids, product_num[ndx]);
		}
	    }
            delete(parms);
	    product_num[ndx]++;
            ndx++;
        }
    }
    if (do_idx) {
        zvclose(unit_idx_out, NULL);
    }
    if (do_idm) {
        zvclose(unit_idm_out, NULL);
    }
    for (auto unit = unit_out.begin(); unit != unit_out.end(); unit++)
        zvclose((*unit), NULL);
}

int marstile::writeOneParm(int _unit, char *parm_name, void *val, char *type,
								int index) {
    int status = 0;
    char msg[150];

    status = zladd(_unit, "PROPERTY", parm_name,
                   val, "FORMAT", type,
                   "MODE", "INSERT", "ELEMENT", index+1, "PROPERTY",
                   "INSTRUMENT_STATE_PARMS", NULL);
    if (RTN_FAILURE(status)) {
        snprintf(msg, 150, "%s failed to be added to output label.", parm_name);
        zvmessage(msg, "");
    }
    return status;
}

void marstile::writeTileParms(int _unit, PigTileParms *in_parms, int nids,
								int index)
{
    int status = 0;

    LblInstrumentState_typ InstrumentState;
    memset(&InstrumentState, 0, sizeof(LblInstrumentState_typ));

    status = marstile::writeOneParm(_unit, "TILE_PRODUCT_ID",
                   &(in_parms->product_id[0]), "STRING", index);
    status = marstile::writeOneParm(_unit, "TILE_UPSAMPLE_METHOD",
                   &(in_parms->upsample_method[0]), "STRING", index);
    status = marstile::writeOneParm(_unit, "TILE_NUM_BANDS",
                   &(in_parms->num_bands), "INT", index);
    status = marstile::writeOneParm(_unit, "TILE_FIRST_LINE",
                   &(in_parms->first_line), "INT", index);
    status = marstile::writeOneParm(_unit, "TILE_NUM_LINES",
                   &(in_parms->nl), "INT", index);
    status = marstile::writeOneParm(_unit, "TILE_FIRST_LINE_SAMPLE",
                   &(in_parms->first_samp), "INT", index);
    status = marstile::writeOneParm(_unit, "TILE_NUM_LINE_SAMPLES",
                   &(in_parms->ns), "INT", index);
    status = marstile::writeOneParm(_unit, "TILE_DOWNSAMPLE_X",
                   &(in_parms->zoom_line), "INT", index);
    status = marstile::writeOneParm(_unit, "TILE_DOWNSAMPLE_Y",
                   &(in_parms->zoom_samp), "INT", index);
}

bool sort_function(PigFileModel *file1, PigFileModel *file2)
{
    return std::make_tuple(file1->getDownsampleXFactor(1),
                file1->getDownsampleYFactor(1), -file1->getNB())
                > std::make_tuple(file2->getDownsampleXFactor(1),
                file2->getDownsampleYFactor(1), -file2->getNB());
}

std::vector<PigFileModel*> marstile::sort_tiles(int nids,
                                                PigFileModel **file_models)
{
    std::vector<PigFileModel*> sorted_files;
    for (int i = 0; i < nids; i++) {
        sorted_files.push_back(file_models[i]);
    }

    std::sort(sorted_files.begin(), sorted_files.end(), sort_function);

    return sorted_files;
}

void marstile::upsample_replication(SimpleImage<float> *in,
                                    SimpleImage<float> *out,
                                    int upsample_x, int upsample_y)
{
    char msg[256];

    const int &in_lines = in->getNL();
    const int &in_samps = in->getNS();
    const int &in_bands = in->getNB();
    float *line;

    // Save time by returning immediately if upsample factor is 1.
    if (upsample_x == 1 && upsample_y == 1) {
        for (int b = 0; b < in_bands; b++) {
            for (int j = 0; j < in_lines; j++) {
                line = in->linePtr(b, j);
                for (int i = 0; i < in_samps; i++) {
                    out->set(b, j, i, line[i]);
                }
            }
        }
        return;
    }

    snprintf(msg, 256, "Replication upsample factor: %d, %d", upsample_x, upsample_y);
    zvmessage(msg, "");

    for (int b = 0; b < in_bands; b++) {
        for (int j = 0; j < in_lines; j++) {
            line = in->linePtr(b, j);
            for (int i = 0; i < in_samps; i++) {
		for (int x = 0; x < upsample_x; x++) {
		    for (int y = 0; y < upsample_y; y++) {
			out->set(b, upsample_x*j+x, upsample_y*i+y, line[i]);
		    }
		}
            }
        }
    }
}

float lerp(float c1, float c2, unsigned int v1, unsigned int v2, float x)
{
    if (v1 == v2) {
        return c1;
    }

    float inc = ((c2 - c1) / (v2 - v1)) * (x - v1);
    float val = c1 + inc;
    return val;
}

void marstile::upsample_bilinear(SimpleImage<float> *in, SimpleImage<float> *out,
                                 int upsample_x, int upsample_y)
{
    char msg[256];
    snprintf(msg, 256, "Bilinear upsample factor: %d, %d", upsample_x, upsample_y);
    zvmessage(msg, "");

    unsigned int nl(in->getNL());
    unsigned int ns(in->getNS());

    // Save time by returning immediately if upsample factor is 1.
    if (upsample_x == 1 && upsample_y == 1) {
        float *line;
        for (int b = 0; b < in->getNB(); b++) {
            for (int j = 0; j < nl; j++) {
                line = in->linePtr(b, j);
                for (int i = 0; i < ns; i++) {
                    out->set(b, j, i, line[i]);
                }
            }
        }
        return;
    }


    for (int b = 0; b < in->getNB(); b++) {
        for (int j = 0; j < nl * upsample_x; j++) {
            double start_line = (j + 0.5) / upsample_y - 0.5;
            start_line = start_line < 0 ? 0 : start_line;

            unsigned int top_line = std::floor(start_line);
            unsigned int bot_line = std::floor(start_line + 1);
            bot_line = bot_line >= nl ? nl - 1 : bot_line;

            float *top_data = in->linePtr(b, top_line);
            float *bot_data = in->linePtr(b, bot_line);

            for (int i = 0; i < ns * upsample_y; i++) {
                double start_samp = (i + 0.5) / upsample_x - 0.5;
                start_samp = start_samp < 0 ? 0 : start_samp;

                unsigned int left_samp = std::floor(start_samp);
                unsigned int right_samp = std::floor(start_samp + 1);
                right_samp = right_samp >= ns ? ns - 1 : right_samp;

                float top_left = top_data[left_samp];
                float top_right = top_data[right_samp];
                float bot_left = bot_data[left_samp];
                float bot_right = bot_data[right_samp];

                double top_interp = lerp(top_left, top_right, left_samp,
                                         right_samp, start_samp);
                double bot_interp = lerp(bot_left, bot_right, left_samp,
                                         right_samp, start_samp);
                double m = lerp(top_interp, bot_interp, top_line,
                                bot_line, start_line);
                float val = std::floor(m + 0.5);

                out->set(b, j, i, val);
            }
        }
    }
}

// Remove unneeded property groups.  They contain info from the input
// tiles that are not particularly relevant to the reconstucted image.

void marstile::delete_prop_groups(int unit)
{
    del_prop_grp(unit, "COMPRESSION_PARMS", 1);

    del_prop_grp(unit, "SUBFRAME_REQUEST_PARMS", 1);
    del_prop_grp(unit, "HISTOGRAM_REQUEST_PARMS", 1);
    del_prop_grp(unit, "COLUMN_SUM_REQUEST_PARMS", 1);
    del_prop_grp(unit, "ROW_SUM_REQUEST_PARMS", 1);
    del_prop_grp(unit, "IMAGE_REQUEST_PARMS", 1);
    del_prop_grp(unit, "OBSERVATION_REQUEST_PARMS", 1);
    del_prop_grp(unit, "THUMBNAIL_REQUEST_PARMS", 1);
}

