/* nsytwksp */

#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include "SimpleImage.h"
#include "nsyt_instruments.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <cmath>

typedef struct {
    int n_vert;		// Number of vertices
    int value;		// Value to use for mask
    SimpleImage<double> poly;	// "image" of (n_vert,2)
} PolyDescriptor;

typedef struct {
    InsightInstrument inst;
    InstrumentParams iparams;
    int outside_dn;     // value outside everything
} WkspParams;

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS
#define POLY_FILE_DELIMITER "," 

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
        int band[3], char *type);
bool point_in_polygon(PigPoint xyz, PolyDescriptor *pp);
int count_size(char *polygon_file);
void read_polygon_file(char *polygon_file, double *arr[], int rows);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, band, line, samp;
    int count, def;
    const size_t msgLen = 256;
    char msg[msgLen];

    int nids, uvw_nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs;
    int xyz_unit[3];
    int xyz_band[3];

    // Outputs
    int out_unit;
    int nlo, nso;

    // Parameters
    WkspParams params;

    double *xxx, *yyy, *zzz;		// input lines
    unsigned char *mask;			// output line

    zvmessage("NSYTWKSP version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
              mission, instrument, homogeneous_inputs,
              MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Get coord system for input XYZ file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
                  xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Interpreting polygons using the %s coordinate frame.",
                  cs->getFrameName());
    zvmessage(msg, "");

    zvp("OUTSIDE_DN", &params.outside_dn, &count);

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, xyz_unit, xyz_band, "XYZ");

    // Open output file.
    // OUT is a single 1-band byte file

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
           "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
           "u_nb", 1,
           "open_act", "sa", "u_org", "bsq",
           "u_format", "byte", "o_format", "byte", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input and output lines

    xxx = new double[nso];
    yyy = new double[nso];
    zzz = new double[nso];
    mask = new unsigned char[nso];

    if (xxx == NULL || yyy == NULL || zzz == NULL) {
        zvmessage("Memory allocation error!", "");
        zabend();
    }

//!!!! TEMP!!!! SET UP DESCRIPTORS

    PolyDescriptor polys[6];
    int n_polys = 0;
    char polygon_file[PIG_MAX_FILENAME_SIZE];
    const char *host_id = m->getHostID();
    
    if (host_id == NULL) {
        zvmessage("Cannot retrieve host ID!", "");
        zabend();
    }
 
    params.inst = HP3;
    if (zvptst("SEIS")) {
        params.inst = SEIS;
    }

    if (params.inst == SEIS) {
        n_polys = 6;
        
        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s_%s.txt", host_id, 
                "seis-wts", "grapple", "maneuvering");
        polys[0].n_vert = count_size(polygon_file);
        double *seis_wts_grapple_maneuvering_polygon[polys[0].n_vert];
        read_polygon_file(polygon_file, seis_wts_grapple_maneuvering_polygon,
                          polys[0].n_vert);
        polys[0].value = 7;
        polys[0].poly.alloc(polys[0].n_vert, 2);
        for (int x = 0; x < polys[0].n_vert; x++) {
            polys[0].poly.set(x, 0, seis_wts_grapple_maneuvering_polygon[x][0]);
            polys[0].poly.set(x, 1, seis_wts_grapple_maneuvering_polygon[x][1]);
            free(seis_wts_grapple_maneuvering_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s_%s.txt", host_id, 
                "seis-wts", "grapple", "ICC");
        polys[1].n_vert = count_size(polygon_file);
        double *seis_wts_grapple_ICC_polygon[polys[1].n_vert];
        read_polygon_file(polygon_file, seis_wts_grapple_ICC_polygon,
                          polys[1].n_vert);
        polys[1].value = 6;
        polys[1].poly.alloc(polys[1].n_vert, 2);
        for (int x = 0; x < polys[1].n_vert; x++) {
            polys[1].poly.set(x, 0, seis_wts_grapple_ICC_polygon[x][0]);
            polys[1].poly.set(x, 1, seis_wts_grapple_ICC_polygon[x][1]);
            free(seis_wts_grapple_ICC_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s_%s.txt", host_id, 
                "seis-wts", "grapple", "nominal");
        polys[2].n_vert = count_size(polygon_file);
        double *seis_wts_grapple_nominal_polygon[polys[2].n_vert];
        read_polygon_file(polygon_file, seis_wts_grapple_nominal_polygon,
                          polys[2].n_vert);
        polys[2].value = 5;
        polys[2].poly.alloc(polys[2].n_vert, 2);
        for (int x = 0; x < polys[2].n_vert; x++) {
            polys[2].poly.set(x, 0, seis_wts_grapple_nominal_polygon[x][0]);
            polys[2].poly.set(x, 1, seis_wts_grapple_nominal_polygon[x][1]);
            free(seis_wts_grapple_nominal_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s_%s.txt", host_id, 
                "seis-wts", "grapple", "kinematic");
        polys[3].n_vert = count_size(polygon_file);
        double *seis_wts_grapple_kinematic_polygon[polys[3].n_vert];
        read_polygon_file(polygon_file, seis_wts_grapple_kinematic_polygon,
                          polys[3].n_vert);
        polys[3].value = 4;
        polys[3].poly.alloc(polys[3].n_vert, 2);
        for (int x = 0; x < polys[3].n_vert; x++) {
            polys[3].poly.set(x, 0, seis_wts_grapple_kinematic_polygon[x][0]);
            polys[3].poly.set(x, 1, seis_wts_grapple_kinematic_polygon[x][1]);
            free(seis_wts_grapple_kinematic_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s.txt", host_id, "seis",
                "boundary");
        polys[4].n_vert = count_size(polygon_file);
        double *seis_boundary_polygon[polys[4].n_vert];
        read_polygon_file(polygon_file, seis_boundary_polygon,
                          polys[4].n_vert);
        polys[4].value = 3;
        polys[4].poly.alloc(polys[4].n_vert, 2);
        for (int x = 0; x < polys[4].n_vert; x++) {
            polys[4].poly.set(x, 0, seis_boundary_polygon[x][0]);
            polys[4].poly.set(x, 1, seis_boundary_polygon[x][1]);
            free(seis_boundary_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s.txt", host_id, "wts",
                "boundary");
        polys[5].n_vert = count_size(polygon_file);
        double *wts_boundary_polygon[polys[5].n_vert];
        read_polygon_file(polygon_file, wts_boundary_polygon,
                          polys[5].n_vert);
        polys[5].value = 2;
        polys[5].poly.alloc(polys[5].n_vert, 2);
        for (int x = 0; x < polys[5].n_vert; x++) {
            polys[5].poly.set(x, 0, wts_boundary_polygon[x][0]);
            polys[5].poly.set(x, 1, wts_boundary_polygon[x][1]);
            free(wts_boundary_polygon[x]);
        }
        
        labelModel->setInstPlacement(file_models, nids, "INST_WORKSPACE_MAP", 
                                     "SEIS", cs);
    } else {		// HP3
        n_polys = 5;

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s_%s.txt", host_id, "hp3",
                "grapple", "maneuvering");
        polys[0].n_vert = count_size(polygon_file);
        double *hp3_grapple_maneuvering_polygon[polys[0].n_vert];
        read_polygon_file(polygon_file, hp3_grapple_maneuvering_polygon,
                          polys[0].n_vert);
        polys[0].value = 7;
        polys[0].poly.alloc(polys[0].n_vert, 2);
        for (int x = 0; x < polys[0].n_vert; x++) {
            polys[0].poly.set(x,0,hp3_grapple_maneuvering_polygon[x][0]);
            polys[0].poly.set(x,1,hp3_grapple_maneuvering_polygon[x][1]);
            free(hp3_grapple_maneuvering_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s_%s.txt", host_id, "hp3",
                "grapple", "ICC");
        polys[1].n_vert = count_size(polygon_file);
        double *hp3_grapple_ICC_polygon[polys[1].n_vert];
        read_polygon_file(polygon_file, hp3_grapple_ICC_polygon,
                          polys[1].n_vert);
        polys[1].value = 6;
        polys[1].poly.alloc(polys[1].n_vert, 2);
        for (int x = 0; x < polys[1].n_vert; x++) {
            polys[1].poly.set(x, 0, hp3_grapple_ICC_polygon[x][0]);
            polys[1].poly.set(x, 1, hp3_grapple_ICC_polygon[x][1]);
            free(hp3_grapple_ICC_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s_%s.txt", host_id, "hp3",
                "grapple", "nominal");
        polys[2].n_vert = count_size(polygon_file);
        double *hp3_grapple_nominal_polygon[polys[2].n_vert];
        read_polygon_file(polygon_file, hp3_grapple_nominal_polygon,
                          polys[2].n_vert);
        polys[2].value = 5;
        polys[2].poly.alloc(polys[2].n_vert, 2);
        for (int x = 0; x < polys[2].n_vert; x++) {
            polys[2].poly.set(x, 0, hp3_grapple_nominal_polygon[x][0]);
            polys[2].poly.set(x, 1, hp3_grapple_nominal_polygon[x][1]);
            free(hp3_grapple_nominal_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s_%s.txt", host_id, "hp3", 
                "grapple", "kinematic");
        polys[3].n_vert = count_size(polygon_file);
        double *hp3_grapple_kinematic_polygon[polys[3].n_vert];
        read_polygon_file(polygon_file, hp3_grapple_kinematic_polygon, 
                          polys[3].n_vert);
        polys[3].value = 4;
        polys[3].poly.alloc(polys[3].n_vert, 2);
        for (int x = 0; x < polys[3].n_vert; x++) {
            polys[3].poly.set(x, 0, hp3_grapple_kinematic_polygon[x][0]);
            polys[3].poly.set(x, 1, hp3_grapple_kinematic_polygon[x][1]);
            free(hp3_grapple_kinematic_polygon[x]);
        }

        snprintf(polygon_file, PIG_MAX_FILENAME_SIZE, "param_files/%s_wksp_%s_%s.txt", host_id, "hp3", 
                "boundary");
        polys[4].n_vert = count_size(polygon_file);
        double *hp3_boundary_polygon[polys[4].n_vert];
        read_polygon_file(polygon_file, hp3_boundary_polygon, 
                          polys[4].n_vert);
        polys[4].value = 2;
        polys[4].poly.alloc(polys[4].n_vert, 2);
        for (int x = 0; x < polys[4].n_vert; x++) {
            polys[4].poly.set(x, 0, hp3_boundary_polygon[x][0]);
            polys[4].poly.set(x, 1, hp3_boundary_polygon[x][1]);
            free(hp3_boundary_polygon[x]);
        }

        labelModel->setInstPlacement(file_models, nids, "INST_WORKSPACE_MAP", 
                                     "HP3", cs); 
    }

//!!!! END TEMP

    params.iparams.readInstrumentParams();

    // Read in the XYZ file one line at a time, and process it...
    
    double offset_x, offset_y;
    double raw_x, raw_y;
    for (line = 0; line < nlo; line++) {

        zvread(xyz_unit[0], xxx, "BAND", xyz_band[0], "LINE", line+1, NULL);
        zvread(xyz_unit[1], yyy, "BAND", xyz_band[1], "LINE", line+1, NULL);
        zvread(xyz_unit[2], zzz, "BAND", xyz_band[2], "LINE", line+1, NULL);

        for (samp = 0; samp < nso; samp++) {

	    unsigned char out_dn = 0;
	    mask[samp] = out_dn;

            // Convert coord systems if necessary...

	    PigPoint raw_xyz(xxx[samp], yyy[samp], zzz[samp]);

            if (raw_xyz.getX() == 0.0 &&
                raw_xyz.getY() == 0.0 &&
                raw_xyz.getZ() == 0.0) {

		continue;			// nothing to do here
	    }
	    out_dn = params.outside_dn;	// in case it's outside everything

            if (cs != xyz_cs) {
                PigPoint old_xyz = raw_xyz;

                // 0 check already done...

                raw_xyz = cs->convertPoint(old_xyz, xyz_cs);
	    }

	    // Now loop through polygons to look for a match

	    for (int p=0; p < n_polys; p++) {
                // apply radial offsets to wts only
                PigPoint new_xyz;
                if (params.inst == SEIS && p == 5) {
                    raw_x = raw_xyz.getX();
                    raw_y = raw_xyz.getY();
                    offset_x = params.iparams.getOffsetX(raw_x, raw_y, WTS);
                    offset_y = params.iparams.getOffsetY(raw_x, raw_y, WTS);
                    new_xyz = params.iparams.applyWTSOffsets(raw_xyz, offset_x, 
                                                             offset_y, WTS); 
                } else {
                    new_xyz = raw_xyz;
                }

		if (point_in_polygon(new_xyz, &polys[p])) {
		    out_dn = polys[p].value;
		    break;
		}
	    }

	    mask[samp] = out_dn;
        }

	// Write output line

        zvwrit(out_unit, mask, "BAND", 1, "LINE", line+1, NULL);
    }

    zvclose(out_unit, NULL);

}

////////////////////////////////////////////////////////////////////////
// Open one set of inputs, either XYZ or UVW...
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
        int band[3], char *type)
{
    const size_t msgLen = 256;
    char msg[msgLen];

    if (nids == 1) {

        // Make sure file is open with U_FORMAT of DOUB to match our buffer.

        // get Unit id
        unit[0] = file_models[0]->getUnit();
    
        if (file_models[0]->isFileOpen())
            file_models[0]->closeFile();
        zvopen(unit[0], "op", "read", "open_act", "sa",
              "io_act", "sa", "u_format", "doub", NULL);
        file_models[0]->setFileOpen(TRUE);

        if (file_models[0]->getNB() != 3) {
            snprintf(msg, msgLen, "A single %s file must have three bands", type);
            zvmessage(msg, "");
            zabend();
        }

        // Initialize xyz_unit array
        unit[2] = unit[1] = unit[0];

        // Initialize band array
        band[0] = 1;
        band[1] = 2;
        band[2] = 3;
    }
    else if (nids == 3) {
        for (int i = 0; i < 3; i++) {

            // make sure that file is open
            if (file_models[i]->isFileOpen())
                file_models[i]->closeFile();

            // get Unit id
            unit[i] = file_models[i]->getUnit();

            zvopen(unit[i], "op", "read", "open_act", "sa",
                  "io_act", "sa", "u_format", "doub", NULL);
            file_models[i]->setFileOpen(TRUE);

            if (file_models[i]->getNB() != 1) {
                snprintf(msg, msgLen, "A three-file %s must have one band each", type);
                zvmessage(msg, "");
                zabend();
            }

            // check that all files are the same size
            if ((file_models[i]->getNL() != file_models[0]->getNL()) ||
                (file_models[i]->getNS() != file_models[0]->getNS())) {

                zvmessage("Input is of different size than Input #1", "");
                zabend();
            }
            band[i] = 1;
        }

    }
    else {
        snprintf(msg, msgLen, "NSYTWKSP requires either 1 3-band file or 3 single band files as input for %s", type);
        zvmessage(msg, "");
        zabend();
    }
}


////////////////////////////////////////////////////////////////////////
// Test to see if the point is inside the polygon
////////////////////////////////////////////////////////////////////////

bool point_in_polygon(PigPoint xyz, PolyDescriptor *pp)
{
    int j = pp->n_vert - 1;
    bool odd_nodes = false;

    double x = xyz.getX();
    double y = xyz.getY();
    double z = xyz.getZ();

    for (int i = 0; i < pp->n_vert; i++) {
        if ((pp->poly.get(i,1) < y && pp->poly.get(j,1) >= y) ||
            (pp->poly.get(j,1) < y && pp->poly.get(i,1) >= y)) {

            if (pp->poly.get(i,0) +
                (y - pp->poly.get(i,1)) /
			(pp->poly.get(j,1) - pp->poly.get(i,1)) *
			 (pp->poly.get(j,0) - pp->poly.get(i,0))
		 < x) {

                odd_nodes = ! odd_nodes;
            }
        }
        j = i;
    }

    return odd_nodes;
}

////////////////////////////////////////////////////////////////////////
// Read polygon file, and return the total number of lines.
////////////////////////////////////////////////////////////////////////

int count_size(char *polygon_file) 
{
    int size = 0;
    char ch;
    char msg[256]; 
    char polygon_path[PIG_MAX_FILENAME_SIZE];

    FILE *file = PigModelBase::openConfigFile(polygon_file, polygon_path);
    if (file == NULL) {
        snprintf(msg, 256, "Error loading polygon file %s", polygon_file);
        zvmessage(msg, "");
        zabend();
    }

    for (ch = getc(file); ch != EOF; ch = getc(file))
        if (ch == '\n')
            size++;

    fclose(file);

    return size;
}

////////////////////////////////////////////////////////////////////////
// Read polygon file, and then store it to 2D array.
//
// polygon_file: partial path appended to CONFIG_PATH to find polygon 
//               files. For example, the partial path might be 
//               param_files/NSYT_hp3_boundary_workspace_reduced_ida0.txt.
// arr: N x 2 array that stores polygon's vertices. 
// rows: total number of lines for the polygon file. This value can be 
//       obtained by calling count_size() function.   
////////////////////////////////////////////////////////////////////////

void read_polygon_file(char *polygon_file, double *arr[], int rows)
{
    char msg[256];
    char line[1024];
    char *token;
    int cols = 2; // fixed length for storing x and y.
    char polygon_path[PIG_MAX_FILENAME_SIZE];

    FILE *file = PigModelBase::openConfigFile(polygon_file, polygon_path);
    if (file != NULL) {
        snprintf(msg, 256, "loading polygon file %s", polygon_path);
        zvmessage(msg, "");
    } else {
        snprintf(msg, 256, "Error loading polygon file %s", polygon_file);
        zvmessage(msg, "");
        zabend();
    }

    for (int i = 0; i < rows; i++) {
        arr[i] = (double *)malloc(cols * sizeof(double));
        if (arr[i] == NULL) {
            snprintf(msg, 256, "Unable to allocate memory to read polygon file %s.", 
                    polygon_file);
            zvmessage(msg, "");
            zabend();
        }

        fgets(line, sizeof(line), file);
        token = strtok(line, POLY_FILE_DELIMITER);
        arr[i][0] = atof(token);
        token = strtok(NULL, POLY_FILE_DELIMITER);
        arr[i][1] = atof(token);
    }
  
    fclose(file);
}

