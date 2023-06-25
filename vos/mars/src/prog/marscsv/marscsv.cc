/* marscsv */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include <cstdlib>
#include <cmath>
#include <stdio.h>

// Allowable buffer sizes in main program
#define MAX_INPUTS 1
#define MAX_OPEN 1

// A value of 0 indicates there is no upper bound for the input image size.
#define MAX_NS 0
#define MAX_NL 0

#define EPSILON 0.0000001

////////////////////////////////////////////////////////////////////////////////

namespace marscsv {
    void do_img_mode(PigFileModel *file_model, double **data, FILE *file_csv,
                     int fileNL, int fileNS, int fileNB);
    void do_xyz_mode(char *xyz_name, PigFileModel *file_model, double **xyz,
                     short int **rgb, FILE *file_csv, PigCoordSystem *cs,
                     PigCoordSystem *xyz_cs, int fileNL, int fileNS, int fileNB);
}


void main44()
{
    int i, j, band, line;
    int status, count, def;
    const size_t msgLen = 256;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];

    // Inputs
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    RadiometryModel *radiometric[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs = NULL;
    PigCoordSystem *xyz_cs = NULL;
    int unit_xyz;                                // Only needed if using XYZ
    int unit;                                    // Used for the RGB file
    int fileNL, fileNS, fileNB;
    bool use_img;
    char format[10];                
    double *xyz[MARS_MAX_NB];                       // Input image in XYZ mode
    short int *rgb[MARS_MAX_NB];
    double *data[MARS_MAX_NB];
    char filename[PIG_MAX_FILENAME_SIZE+1];
    char filename_csv[PIG_MAX_FILENAME_SIZE+1];

    // Outputs
    int nlo, nso;
    FILE *file_csv;

    // User parameters


    zvmessage("MARSCSV version 2", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Much of the things that mars_setup does are not
    // necessary for generating the csv file.
    mars_setup(nids, file_models, camera_in, pointing_in, radiometric, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);


    file_models[0]->openFile();
    status = zvget(file_models[0]->getUnit(), "FORMAT", format, NULL);

    if (status == 1) {
        if ((strcmp(format, "HALF") == 0) || (strcmp(format, "BYTE") == 0)) {
            strcpy(format, "HALF");
        } else {
            strcpy(format, "REAL");
        }
    } else {
        zvmessage("Unable to determine the data format of the input.", "");
        zabend();
    }

    use_img = zvptst("NO_XYZ");
    if (!use_img) {
        zvpcnt("IN_XYZ", &count);
        if (count == 0) {
            zvmessage("IN_XYZ is a required parameter unless NO_XYZ is requested. Exiting.", "");
            zabend();
        }

        PigMission *m = PigMission::getMissionObject(mission);

        zvpone("IN_XYZ", filename, 1, PIG_MAX_FILENAME_SIZE);
	PigFileModel *xyz_file = PigFileModel::create(filename);

        // Get coord system for input XYZ file
        PigCSReference *ref;
        xyz_file->getDerivedImageCS(ref);
        xyz_cs = m->getCoordSystem(ref);
	if (xyz_cs != NULL) {

            snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
                xyz_cs->getFrameName(), ref->getFullName());
	} else {
	    snprintf(msg, msgLen, "No XYZ coord system found");
	}
        zvmessage(msg, "");        
    }

    // Get parameters
    fileNL = file_models[0]->getNL();
    fileNS = file_models[0]->getNS();
    fileNB = file_models[0]->getNB();

    if (nids == 1) {
        // Make sure file is open with a format that matches our buffer.

        // get unit id
        unit = file_models[0]->getUnit();

        if (file_models[0]->isFileOpen()) {
            file_models[0]->closeFile();
        }

        if (strcmp(format, "HALF") == 0) {
            if (use_img) {
                zvmessage("Input is HALF, but will be treated as REAL\n", "");
                status = zvopen(unit, "op", "read", "open_act", "sa",
                            "io_act", "sa", "u_format", "doub", NULL);
            } else {
                zvmessage("Open input as HALF\n", "");
                status = zvopen(unit, "op", "read", "open_act", "sa",
                                "io_act", "sa", "u_format", "half", NULL);
            }
        } else {
            zvmessage("Open input as DOUBLE\n", "");
            status = zvopen(unit, "op", "read", "open_act", "sa",
                            "io_act", "sa", "u_format", "doub", NULL);
        }
        file_models[0]->setFileOpen(TRUE);
    } else {
        zvmessage("marcsv requires 1 file as input!", "");
        zabend();
    }

    snprintf(msg, msgLen, "Image size: %d, %d", fileNL, fileNS);
    zvmessage(msg, "");

    if (use_img) {
        for (band = 0; band < fileNB; band++) {
            data[band] = (double *)malloc(fileNL * fileNS * sizeof(double));
        }   
    } else {
        for (band = 0; band < 3; band++) {
            // xyz[band] = (double *)malloc(fileNL * fileNS * sizeof(double));
            xyz[band] = new double[fileNL * fileNS];
        }
        for (band = 0; band < fileNB; band++) {
            // rgb[band] = (short int *)malloc(fileNL * fileNS * sizeof(short int));
            rgb[band] = new short int[fileNL * fileNS];
        }
    }
    if (rgb[0] == NULL || (!use_img && xyz[0] == NULL)) {
        snprintf(msg, msgLen, "Unable to allocate memory for XYZ or RGB input");
        zvmessage(msg, "");
        zabend();
    }

    zvpone("OUT", filename_csv, 1, PIG_MAX_FILENAME_SIZE);
    file_csv = fopen(filename_csv, "w");

    if (use_img) {
        marscsv::do_img_mode(file_models[0], data, file_csv, fileNL, fileNS, fileNB);
        for (band = 0; band < fileNB; band++) {
            free(data[band]);
	}
    } else {
        // Open input xyz
        zvpone("IN_XYZ", filename, 1, PIG_MAX_FILENAME_SIZE);
        marscsv::do_xyz_mode(filename, file_models[0], xyz, rgb, file_csv,
                             cs, xyz_cs, fileNL, fileNS, fileNB);

        for (band = 0; band < 3; band++) {
            delete xyz[band];
        }
        for (band = 0; band < fileNB; band++) {
            delete rgb[band];
	}

    }

    fclose(file_csv);
}

void marscsv::do_xyz_mode(char *xyz_name, PigFileModel *file_model,
                          double **xyz, short int **rgb, FILE *file_csv,
                          PigCoordSystem *cs, PigCoordSystem *xyz_cs,
                          int fileNL, int fileNS, int fileNB)
{
    int line, samp, band, count, def, status;
    const size_t msgLen = 255;
    char msg[msgLen];

    int unit_xyz;
    zvunit(&unit_xyz, "IN_XYZ", 1, "u_name", xyz_name, NULL);
    status = zvopen(unit_xyz, "OP", "READ", "U_FORMAT", "DOUB",
                    "OPEN_ACT", "AS", NULL);

    for (band = 0; band < fileNB; band++) {
        for (line = 0; line < fileNL; line++) {
            zvread(file_model->getUnit(), &rgb[band][line * fileNS],
                   "BAND", band + 1, "LINE", line + 1, NULL);
        }
    }

    for (band = 0; band < 3; band++) {
        for (line = 0; line < fileNL; line++) {
            zvread(unit_xyz, &xyz[band][line * fileNS],
                   "BAND", band + 1, "LINE", line + 1, NULL);
        }
    }


    if (cs != xyz_cs && xyz_cs != NULL) {
        for (line = 0; line < fileNL; line++) {
            for (samp = 0; samp < fileNS; samp++) {
                int index = line * fileNS + samp;
                PigPoint old_xyz(*(xyz[0] + index), *(xyz[1] + index),
                            *(xyz[2] + index));

                // If the point is 0,0,0 (meaning not valid), leave it alone

                if (old_xyz.getX() != 0.0 ||
                    old_xyz.getY() != 0.0 ||
                    old_xyz.getZ() != 0.0) {

                    PigPoint new_xyz = cs->convertPoint(old_xyz, xyz_cs);
                    *(xyz[0] + index) = new_xyz.getX();
                    *(xyz[1] + index) = new_xyz.getY();
                    *(xyz[2] + index) = new_xyz.getZ();
                }
            }
        }
    }

    int dn_max;
    zvparmd("DN_MAX", &dn_max, &count, &def, 1, 0);

    snprintf(msg, msgLen, "DN_MAX=%d", dn_max);
    zvmessage(msg, "");

    bool singleLine;
    singleLine = zvptst("SINGLE_LINE");

    bool useSpaces;
    useSpaces =  zvptst("USE_SPACES");

    float x, y, z;
    short int  r, g, b;
    int idd = 0;
    float str_scale = 255.0 / dn_max;

    // choose the separator in the output file
    char* sep = NULL;
    if(useSpaces == true) {
        sep = " ";
    }
    else {
        sep = ",";
    }
    
    for (int line = 0; line < fileNL; line++) {
        for(int samp = 0; samp < fileNS; samp++) {
           
            x = y = z = 0.0;
            x = xyz[0][line * fileNS + samp];
            y = xyz[1][line * fileNS + samp];
            z = xyz[2][line * fileNS + samp];

            r = g = b = 0;
            r = (short int)(rgb[0][line * fileNS + samp] * str_scale);
            if (fileNB == 3) {
                g = (short int)(rgb[1][line * fileNS + samp] * str_scale);
                b = (short int)(rgb[2][line * fileNS + samp] * str_scale);
            }

            // check first for invalid or missing pixel values
            // continue only if valid pixel
            if (std::fabs(x) < EPSILON && std::fabs(y) < EPSILON &&
                std::fabs(z) < EPSILON) {
                continue;
            }
            if (r == 0 && g == 0 && b == 0)
                continue;

            switch (fileNB) {
                case 1:
                // x,y,z and intensity
                fprintf(file_csv, "%f%s%f%s%f%s %d\n", x, sep, y, sep, z, sep, r); break;
                case 2:
                // 2-banded (uncommon) case
                fprintf(file_csv, "%f%s%f%s%f%s %d%s%d\n", x, sep, y, sep, z, sep, r, sep, g); break;
                case 3:
                // xyz and rgb
                if(singleLine) {
                        fprintf(file_csv, "%f%s%f%s%f%s%d%s%d%s%d\n", x, sep, y, sep, z, sep, r, sep, g, sep, b);
                }
                else {
                        fprintf(file_csv, "%f%s%f%s%f\n%d %d %d\n", x,  sep, y, sep, z, r, g, b);
                }
                break;
                default:
                fprintf(file_csv, "%f%s%f%s%f\n", x,  sep, y, sep, z);
            }
        }
    }
}

void marscsv::do_img_mode(PigFileModel* file_model, double **data, FILE *file_csv,
                          int fileNL, int fileNS, int fileNB)
{
    int band, line, samp, count, def;
    char msg[255];

    for (band = 0; band < fileNB; band++) {
        for (line = 0; line < fileNL; line++) {
	     zvread(file_model->getUnit(), data[band] + (line * fileNS),
                   "BAND", band + 1, "LINE", line + 1, NULL);
        }
    }
    
    int dn_max;
    zvparmd("DN_MAX", &dn_max, &count, &def, 1, 0);

    snprintf(msg, 255, "DN_MAX=%d", dn_max);
    zvmessage(msg, "");
        
    float a, b, c;
    bool lineSamp;
    lineSamp = zvptst("LINE_SAMP");

    bool useSpaces;
    useSpaces =  zvptst("USE_SPACES");

     // choose the separator in the output file
    char* sep = NULL;
    if(useSpaces == true) {
        sep = " ";
    }
    else {
        sep = ",";
    }

    for (int line = 0; line < fileNL; line++) {
        for(int samp = 0; samp < fileNS; samp++) {
            a = b = c = 0.0;
            a = data[0][line * fileNS + samp];
            if (fileNB > 1) {
                b = data[1][line * fileNS + samp];
                if (fileNB > 2) {
		    c = data[2][line * fileNS + samp];
                }
            }
            if ((std::fabs(a) < EPSILON) && (std::fabs(b) < EPSILON)
                && (std::fabs(c) < EPSILON)) {  // don't include non-data
                    continue;
            }
            if (lineSamp) {
                switch (fileNB) {
                    case 1:
                    fprintf(file_csv, "%d%s%d%s%f\n", line, sep, samp, sep, a); break;
                    case 2:
                    fprintf(file_csv, "%d%s%d%s%f%s%f\n", line, sep, samp, sep, a, sep, b); break;
                    case 3:
                    fprintf(file_csv, "%d%s%d%s%f%s%f%s%f\n", line, sep, samp, sep, a, sep, b, sep, c);
                    break;
                    default:
                    fprintf(file_csv, "%d%s%d%s%f\n", line, samp, a);
                }
            } else {
                switch (fileNB) {
                    case 1:
                    fprintf(file_csv, "%f\n", a); break;
                    case 2:
                    fprintf(file_csv, "%f%s%f\n", a, sep, b); break;
                    case 3:
                    fprintf(file_csv, "%f%s%f%s%f\n", a, sep, b, sep, c); break;
                    default:
                    fprintf(file_csv, "%f\n", a);
                }
            }
        }
    }
}
