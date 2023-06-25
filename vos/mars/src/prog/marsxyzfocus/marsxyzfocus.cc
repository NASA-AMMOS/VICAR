/* marsxyzfocus */

#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include "PigCAHV.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "mat3.h"

#include "return_status.h"

#include <math.h>
#include <iostream>

#include <cmath>
#include <vector>
#include <Eigen/Dense>   
#include <Eigen/SVD>
#include <Eigen/QR>
#include <limits>

using namespace std;
using namespace Eigen;

/* buffer sizes in main program */
#define MAX_INPUTS 1
#define MAX_PARENTS 31
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define MAX_OBUF 30000


// Function declarations:

void polyfit(const vector<double> &x, const std::vector<double> &y,
		vector<double> &coeff, int order);

double polyeval(const vector<double> &coeff, double testval);

int search_closest(const vector<double>& sorted, double x);

int closest_index(const vector<double>& dnvals, double x);

bool intersectPlane(const PigVector &planenormal, const PigPoint &offset, 
		const PigPoint &rayorigin, const PigVector &raydirection, double &t);


////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j;
    int count, def, status;
#define MSG_SIZE 150
    char msg[MSG_SIZE];

    int nids;
    char mission[64], instrument[64];
    int nl, ns, nb;
    int nids_parents;

    // Inputs:
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *depth_cs;
    int depth_unit;
    float point[3];

    // If parent images available:
    char mission_parents[64], instrument_parents[64];
    PigFileModel *file_models_parents[MAX_PARENTS];
    PigCameraModel *camera_in_parents[MAX_PARENTS];
    PigPointingModel *pointing_in_parents[MAX_PARENTS];
    PigCoordSystem *cs_parents; 

    // Outputs:
    int out_unit_range, out_unit_xyz[3]; 
    int out_band_range, out_band_xyz[3];
    int nlo, nso;
    unsigned char depth[MAX_NS];
    float x[MAX_NS], y[MAX_NS], z[MAX_NS]; 
    double edm_to_range;
    char outfilename[150];
    char outdm[150];

    // User parameters:
    PigPoint depthOrigin;
    string mission_type;
    string camera_type;
    int order = 2;
    int relabel;
  

    zvmessage("MARSXYZFOCUS version 2022-05-27", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one input only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
	     mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Get the coordinate system to use:

    snprintf(msg, MSG_SIZE, "Generating XYZ using the %s coordinate frame: %s",
	   cs->getFrameName(), cs->getFullName());
    zvmessage(msg, "");

    // Read the mission type (MSL and M20 supported for now) and camera type 
    // (WATSON, ACI, and MAHLI supported for now):

    mission_type = "M20";
    camera_type = "WATSON";
    if (zvptst("ACI"))
	camera_type = "ACI";
    if (zvptst("MSL")) {
        mission_type = "MSL";
	camera_type = "MAHLI";
    }

    // Obtain a camera model for the parent images. Initialize from the depth 
    // map, but change to the "best" parent image (based on the depth map 
    // information) if available later on when these are read in.
    // NOTE: The depth map may not have a camera model, so this is necessary

    PigCameraModel* cmod = camera_in[0];

    // Check for an input focus array:

    int focus_array[MAX_PARENTS];
    status = zvparm("FOCUSARRAY", focus_array, &count, &def, MAX_PARENTS, 0);
    if (status == 1 && count > 0) {
	nids_parents = count;
        for (i=0; i < count; i++) {

            if (focus_array[i] == -1){
                continue;  // set nothing in the array
            }
	    // !!!!NOTE: These limits should be set PER INSTRUMENT! 
	    // Placeholders:
	    // if(camera_type == "WATSON") { focusmax = 20000; focusmin = 0; };
	    // if(camera_type == "ACI") { focusmax = 20000; focusmin = 0; };
	    // if(camera_type == "MAHLI") { focusmax = 20000; focusmin = 0; };

	    // Using these absolute values for now:

            if (focus_array[i] > 20000 || focus_array[i] < 0) {
                zvmessage("FOCUSARRAY value outside valid limits!", "");
                zabend();
            }
        }  
    }
    else {

        // If no file is supplied, read the parent images.
        // Get the list of all parent files:

        char **filenames_parents = new char *[MAX_PARENTS];
        if (filenames_parents == NULL) {
	    zvmessage("Memory error in mars_setup, filename array", "");
	    zabend();
        }
        mars_get_filelist("PARENTS", nids_parents, filenames_parents, 
	    MAX_PARENTS, FALSE);

        // Read the files:

        char *sid = NULL;
        PigPointingCorrections *pointing_corrections;
        mars_read_filelist(nids_parents, filenames_parents, file_models_parents, 
	    camera_in_parents, pointing_in_parents, NULL, NULL, cs_parents, 
	    mission_parents, instrument_parents, homogeneous_inputs, MAX_NL, 
	    MAX_NS, &pointing_corrections, &sid);
 
        // Make sure we have a valid file:

        int a_good_file_idx = 0;
        for (a_good_file_idx = 0; a_good_file_idx < nids_parents; a_good_file_idx++) {
	    if (file_models_parents[a_good_file_idx] != NULL)
	        break;
        }
        if (a_good_file_idx >= nids_parents) {
	    zvmessage("Could not find a good input file model.", "");
	    zabend();
        }

        // Finally, loop through the parent images and get their focus values:

        for (int i=0; i < nids_parents; i++) {
            focus_array[i] = file_models_parents[i]->getInstrumentFocusPosition(0);
        }

        // Search for the "best" parent image to fill in cmod.
	// NOTE: this is a future enhancement, and if we want to search for the 
	// best parent focus value, this is where we would do it. This is only 
	// relevant if the camera models are different per focus. For now, 
	// setting to the camera model for the first parent:

	cmod = camera_in_parents[0];

    }

    // Get the coordinate system for the camera model:
    PigCoordSystem* cm_cs = cmod->getCoordSystem();

    // With these focus values, compute the range (working distance) from 
    // motor count, whih varies for WATSON and ACI.
    // !!!!NOTE: These should eventually go into a config file! Also, need the 
    // values for MAHLI.

    vector<double> range(nids_parents);
    if(camera_type == "ACI") {

        zvmessage("Using ACI camera", "");

	// For ACI, and assuming the "dust cover open" case:
        //
	// dw = a*mopen - b
	// 
	// a = 0.005
	// b = 20.34

        double a = 0.005;
        double b = 20.34;


        for (int i=0; i < nids_parents; i++) {
	    double mopen=focus_array[i];
            range[i] = a*mopen - b;
	    // NOTE: The result is in centimeters, but we need it in meters:
	    range[i] /= 100.0;
	    snprintf(msg, MSG_SIZE, "Parent image %d, focus motor count = %d, range (working distance) = %f m", i, focus_array[i], range[i]);
	    zvmessage(msg, "");
        }

    }
    else if(camera_type == "WATSON" || camera_type == "MAHLI") { 

	if(camera_type == "WATSON")
            zvmessage("Using WATSON camera", "");
	else
	    zvmessage("Using MAHLI camera", "");
    
        // For WATSON/MAHLI, and assuming the "dust cover open" case:
        //
        // dw = (a*mopen^(–1) + b + c*mopen + d*mopen^2 + e*mopen^3)^(–1)
        //
        // a = 1.09106×106
        // b = –332.921
        // c = 3.82592×10–2
        // d = –1.96922×10–6 
        // e = 3.84562×10–11

        double a = 1.09106e6;
        double b = -332.921;
        double c = 3.82592e-2;
        double d = -1.96922e-6;
        double e = 3.84562e-11;

        for (int i=0; i < nids_parents; i++) {
	    double mopen=focus_array[i];
            range[i] = pow(a*pow(mopen,-1) + b + c*mopen + d*pow(mopen, 2) + 
		e*pow(mopen, 3), -1);
	    // NOTE: The result is in centimeters, but we need it in meters:
	    range[i] /= 100.0;
	    snprintf(msg, MSG_SIZE, "Parent image %d, focus motor count = %d, range (working distance) = %f m", i, focus_array[i], range[i]);
	    zvmessage(msg, "");
        }
    }
    else {
	zvmessage("Must specify a valid camera type.", "");
	zabend();
    }

    // Next, figure out the relation between the parent images and the depth map
    // grayscale data value (DN). 
    // To derive the values from 2-31 images, divide 256 by the number of images 
    // merged (e.g., 9), subtract that result (e.g., 28.44) from DN 255 and 
    // round to nearest whole number (e.g., 227), then subtract that result (e.g.
    // 28.44) from that resulting DN (e.g., 227) and round (e.g., 199) and so 
    // forth.

    vector<double> dn;
    for(int i=0; i<nids_parents; i++) {
        dn.insert(dn.end(), floor(255.0-i*255.0/(float)nids_parents));
    }

    zvmessage("DN vector:", "");
    for(int i=0; i<nids_parents; i++) {
        snprintf(msg, MSG_SIZE, "%f ", dn.at(i)); 
        zvmessage(msg, "");
    }
    zvmessage(msg, "");
 
    // Next, compute a best-fit polynomial for DN to range, using 
    // focus_image_relation (DN) as the x-axis and working distance (range) as 
    // the y-axis:

    vector<double> coeff;
    zvp("ORDER", &order, &count);
    polyfit(dn, range, coeff, order); //Can also use '3' for a cubic
    zvmessage("Computed coefficients for DN to range fit:", ""); 
    for(int i=0; i<coeff.size(); i++) {
        snprintf(msg, MSG_SIZE, "%f ", coeff.at(i)); 
        zvmessage(msg, "");
    }
 

    // *************************************************************************
    // For each pixel in the EDM, get the DN, and use the curve to convert DN to 
    // range:

    // Make sure the file is not open:

    if (file_models[0]->isFileOpen())
        file_models[0]->closeFile();
      
    // Open the file:

    file_models[0]->openFile();
    file_models[0]->setFileOpen(TRUE);
      
    // Get the Unit ID:

    depth_unit = file_models[0]->getUnit(); 

    // Check for proper number of bands:

    zvget(depth_unit, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
    snprintf(msg, MSG_SIZE, "Depth unit = %d, nl = %d, ns = %d, nb = %d", 
	depth_unit, nl, ns, nb);
    zvmessage(msg, "");
    if (nb != 1) {
        zvmessage("An EDM file must have one band", "");
        zabend();
    }

    // Get input EDM image dimensions:

    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();
    snprintf(msg, MSG_SIZE, "Output # lines & samples=%10d %10d", nlo, nso);
    zvmessage(msg, "");

    // Check for limits:

    if (nlo < 1 || (nso > MAX_NS) || (nso < 1)) {
        zvmessage("Exceeded buffer limits", "");
        zabend();
    }

    // Get the camera orientation ('A' vector in CAHVORE system), which will be
    // needed later, as well as the position 'C':

    PigVector Avector = cmod->getCameraOrientation(); //in camera model cs
    PigPoint Cposition = cmod->getCameraPosition();
    snprintf(msg, MSG_SIZE, "Camera position ('C'): (%f %f %f)", 
        Cposition.getX(), Cposition.getY(), Cposition.getZ());
    snprintf(msg, MSG_SIZE, "Camera orientation ('A') vector: (%f %f %f)", 
        Avector.getX(), Avector.getY(), Avector.getZ());
    zvmessage(msg, "");

    // Determine user-defined projection origin and coordinate system.
    // This is how it's done in 'marsmap':

    PigPoint proj_origin;
    proj_origin.setXYZ(0.0, 0.0, 0.0);
    PigCoordSystem *po_cs = cm_cs; 
    zvpcnt("PROJ_ORIGIN", &count);
    if (count == 3) {
	char po_coord[256];
	zvp("PO_COORD", po_coord, &count);
	if (count != 0) {
            po_cs = m->getCoordSystem(file_models[0], po_coord);
	    if (po_cs == NULL) {
	        snprintf(msg, MSG_SIZE, "Invalid PO_COORD: %s", po_coord); 
	        zvmessage(msg, "");
	        zabend();
	    }
	}
	snprintf(msg, MSG_SIZE, "Interpreting PROJ_ORIGIN in the %s frame: %s", 
	    po_cs->getFrameName()); 
	zvmessage(msg, "");
    }

    double array[3];
    zvparmd("PROJ_ORIGIN", array, &count, &def, 3, 0);
    if (count == 3) {
        proj_origin.setXYZ(array);
        proj_origin = cm_cs->convertPoint(proj_origin, po_cs);
        snprintf(msg, MSG_SIZE, "User provided projection origin = (%f, %f, %f)", 
  	proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
        zvmessage(msg, "");
    }
    else {
        // We can override the projection origin here for the MSL or M20 case.
        // However, the origin in the MSL or M20 case is with respect to the 
        // camera model, but now we need to express it with respect to the rover 
        // nav frame, such that the origin and vector are in the camera frame.
	// !!!!We need to consider both the C-to-sapphire distance PLUS the 
	// sapphire-to-poker distance in order to do this properly, since range
	// is relative to that distance!
	// !!!! The C-to-sapphire + sapphire-to-poker for WATSON is 0 + 0.269m, 
	// the same will be assumed for ACI for now (this needs to be figured 
	// out!), and 0.019m + 0.04264m for MAHLI:

	double ctosapphire = 0.0;
	double sapphiretopoker = 0.0; 
        if(camera_type == "WATSON" || camera_type == "ACI") 
	    sapphiretopoker = 0.0269; 
        if(camera_type == "MAHLI") { 
	    ctosapphire = 0.019; 
	    sapphiretopoker = 0.04264;
	}

	double camoffset = ctosapphire + sapphiretopoker;

	// User override:
	double temp;
	zvparmd("CAMOFFSET", &temp, &count, &def, 1, 0);
	if (count != 0) {
	   if (temp != camoffset) {
	      sprintf(msg, "Camera offset overriden from %f to %f", camoffset, temp);
	      zvmessage(msg, "");
	      camoffset = temp;
	    }
	}

	proj_origin.setX(Cposition.getX() + camoffset*Avector.getX());
        proj_origin.setY(Cposition.getY() + camoffset*Avector.getY());
	    proj_origin.setZ(Cposition.getZ() + camoffset*Avector.getZ());
        proj_origin = cm_cs->convertPoint(proj_origin, po_cs);
        snprintf(msg, MSG_SIZE, "Projection origin = (%f, %f, %f)", 
	proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
        zvmessage(msg, "");
    }

    // OUT can be 1 or 3 files, for a single, 3-banded file, or 3 single-
    // band files.
    // NOTE: the RTL transfers labels automatically from the primary input, which
    // by default is the first file in INP, in this case the EDM. It was found 
    // that the metadata for the EDM is wrong, specifying time of downlink and 
    // not time of acquisition). To fix this, the RTL routine "zvselpi" allows
    // to select the primary input, so we call this prior to opening the output 
    // file:

    zvselpiu(file_models[0]->getUnit()); // transfer labels

    zvpcnt("OUT", &count);
    if (count == 1) {
        zvunit(&out_unit_xyz[0], "OUT", 1, NULL);
        zvopen(out_unit_xyz[0], "op", "write",
	    "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	    "u_nb", 3,
	    "open_act", "sa", "u_org", "bsq",
	    "u_format", "real", "o_format", "real", NULL);
        zvplabel(out_unit_xyz[0], 0, 1);
        out_unit_xyz[1] = out_unit_xyz[0];
        out_unit_xyz[2] = out_unit_xyz[0];
        out_band_xyz[0] = 1;
        out_band_xyz[1] = 2;
        out_band_xyz[2] = 3;

        // Write output label:

        PigMission *m = PigMission::getMissionObject(mission);
        PigLabelModel *labelModel = m->createLabelModel(out_unit_xyz[0]);

        // Pick the coordinate system to use:

        labelModel->setXYZ(file_models, nids, cs, "XYZ_MAP", 0.0,
			file_models[0]->getStereoProductId());
    	labelModel->writeCM(cmod, cm_cs);
    }
    else if (count == 3) {
        char *image_type[3] = {"X_MAP", "Y_MAP", "Z_MAP"};
        for (i=0; i<3; i++) {
            zvunit(&out_unit_xyz[i], "OUT", i+1, NULL);
            zvopen(out_unit_xyz[i], "op", "write",
	        "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	        "u_nb", 1,
	        "open_act", "sa", "u_org", "bsq",
	        "u_format", "real", "o_format", "real", NULL);
            zvplabel(out_unit_xyz[i], 0, 1);
            out_band_xyz[i] = 1;

            // Write output label:

            PigMission *m = PigMission::getMissionObject(mission);
            PigLabelModel *labelModel = m->createLabelModel(out_unit_xyz[i]);

            // Pick the coordinate system to use:

            labelModel->setXYZ(file_models, nids, cs, image_type[i], 0.0,
			file_models[0]->getStereoProductId());
	    labelModel->writeCM(cmod, cm_cs);
        }
    }
    else {
        zvmessage("OUT must have 1 or 3 filenames", "");
        zabend();
    }

    // Range of the ouptut XYZ:

    double xmax = -std::numeric_limits<float>::max();
    double xmin = std::numeric_limits<float>::max();
    double ymax = -std::numeric_limits<float>::max();
    double ymin = std::numeric_limits<float>::max();
    double zmax = -std::numeric_limits<float>::max();
    double zmin = std::numeric_limits<float>::max();

    // Process the image data:

    int no_intersection = 0;
    int invalid_points = 0;

    for (j=0; j < nlo; j++) {		// line

        // Read the line from the input file:

        zvread(depth_unit, depth, "LINE", j+1, "BAND", 1, NULL);

        for (i=0; i < nso; i++) {		// sample

	    // Compute EDM to meters given the polynomial fit:

	    edm_to_range = polyeval(coeff, (double)depth[i]);

            // Finally, convert range (in m) to XYZ, which involves these steps:

            // 1) Given the projection origin and the 'A' camera orientation 
	    // vector, also computed previously, we can create a plane that is 
	    // coincident with the sapphire window and is perpendicular to the 
	    // optical axis. We have all of this information, and will use it in 
	    // Step 5 below.
	 

	    // 2) For each pixel, find the closest input image, using the DN  
	    // vector. Extract the camera model from that image.  Currently, we  
	    // have only one camera model across all focuses, but in the future 
	    // we may get focus-dependent camera models, so we'll pull from the  
	    // correct image.Also, camera model interpolation between focus 
	    // values likely won't makea difference, so the closest value should 
	    // suffice.

	    // "search_closest" is much faster for vectors sorted lowest to 
	    // highest, so leaving this in place if needed in the future. For 
	    // now, all of our dn vectors begin with 255 and go down from there,
	    // so "closest_index" works well for small vectors:
	    // int idx_closest = search_closest(dn, depth[i]);
	    int idx_closest = closest_index(dn, depth[i]);

	    // 3) Use the chosen camera model to project a ray into space for  
	    // the current pixel:

	    //Convert points to camera coordinates:

	    double img_l = j + file_models_parents[idx_closest]->getYOffset();
            double img_s = i + file_models_parents[idx_closest]->getXOffset();

	    // Obtain ray origin and direction:

	    PigPoint ray_origin;
	    PigVector ray_direction;
	    camera_in_parents[idx_closest]->LStoLookVector(img_l, img_s,
		     ray_origin, ray_direction, cm_cs);
	    ray_direction.normalize();  // make sure it's a unit vector


	    // 4) Use the range computed in "edm_to_range" to offset the plane  
	    // above along the plane normal. Given a non-zero scalar k, k*n is 
	    // always a point on a plane. So multiplying A*edm_to_range directly 
	    // provides a point on the offset plane that we seek:

	    PigPoint offset(Avector.getX(), Avector.getY(), Avector.getZ());
	    offset = proj_origin + offset*edm_to_range; 
	  

	    // 5) Intersect the projected ray with the offset plane, which  
	    // defines the XYZ coordinate, and save the XYZ coordinate in an XYZ 
	    // image:

	    PigPoint final_point(0.0, 0.0, 0.0);
	    double t = 0;
            if (edm_to_range == 0.0) {
		// Invalid point! Keep final_point at (0, 0, 0)	
		invalid_points++;
            }
	    else {
	  
	        // Given the ray direction, we travel along it until we   
		// intercept the plane. The intersecting point is provided by: 
		//     ray_origin + t*ray_direction
	    
	        bool inter = intersectPlane(Avector, offset, ray_origin, ray_direction, t);
	        if(inter) {
	            final_point.setX(ray_origin.getX() + t*ray_direction.getX());
		    final_point.setY(ray_origin.getY() + t*ray_direction.getY());
		    final_point.setZ(ray_origin.getZ() + t*ray_direction.getZ());
	        }
	        else {
		    // This should not happen!
		    no_intersection++;
	        }
            }
	 
  
  	    // 6) XYZ is expressed in the camera coordinate system, but we can 
	    // convert to whatever frame we want or the user specified.
	    // NOTE: not working for now, always returns (0, 0, 0)
	    if (cs != cm_cs) final_point = cs->convertPoint(final_point, cm_cs);  
	    x[i] = final_point.getX();
	    y[i] = final_point.getY();
	    z[i] = final_point.getZ();

            // Compute ranges of the XYZ data:
            if(x[i] < xmin) xmin = x[i];
            if(x[i] > xmax) xmax = x[i];
            if(y[i] < ymin) ymin = y[i];
            if(y[i] > ymax) ymax = y[i];
            if(z[i] < zmin) zmin = z[i];
            if(z[i] > zmax) zmax = z[i];

	    // *****DEBUG PRINT, step by step:
#if 0
	    //if (i==0 && j==0) {
	    if (i<5 && j<5) {
	        cout << "Depth = " << static_cast<unsigned>(depth[i]) << endl;
	        cout << "1) Range value at this depth: " << edm_to_range << endl;
	        cout << "2) Index of the closest element: " << idx_closest << endl;
        	cout << "2) The closest element itself: " << dn[idx_closest] << endl;
		cout << "3) Ray origin at this pixel: " << ray_origin.getX()
		  << ", " << ray_origin.getY() << ", " << ray_origin.getZ() << endl; 
	        cout << "3) Ray projected in this direction: " << ray_direction.getX()
		  << ", " << ray_direction.getY() << ", " << ray_direction.getZ() << endl; 
	        cout << "4) Plane offset to this new origin: " << offset.getX()
		  << ", " << offset.getY() << ", " << offset.getZ() << endl;
		cout << "5) Computed t value: " << t << endl;
	        cout << "5) Computed XYZ: " << x[i] << ", " << y[i] << ", " << z[i] << endl;
	    }
#endif

        }
  
    // Write out to files:

    zvwrit(out_unit_xyz[0], x, "LINE", j+1, "BAND", out_band_xyz[0], NULL);
    zvwrit(out_unit_xyz[1], y, "LINE", j+1, "BAND", out_band_xyz[1], NULL);
    zvwrit(out_unit_xyz[2], z, "LINE", j+1, "BAND", out_band_xyz[2], NULL);

    }

    snprintf(msg, MSG_SIZE, "Ray-plane intersection failed %d times; set XYZ to 0",    no_intersection);
    zvmessage(msg, "");
snprintf(msg, MSG_SIZE, "Number of invalid points (range = 0): %d", invalid_points);
    zvmessage(msg, "");

    // Print out XYZ ranges:
    snprintf(msg, MSG_SIZE, "X range: [%f, %f]", xmin, xmax);
    zvmessage(msg, "");
    snprintf(msg, MSG_SIZE, "Y range: [%f, %f]", ymin, ymax);
    zvmessage(msg, "");
    snprintf(msg, MSG_SIZE, "Z range: [%f, %f]", zmin, zmax);
    zvmessage(msg, "");

    zvclose(out_unit_xyz[0], NULL);
    if (out_unit_xyz[1] != out_unit_xyz[0])
        zvclose(out_unit_xyz[1],NULL);
    if (out_unit_xyz[2] != out_unit_xyz[0])
        zvclose(out_unit_xyz[2],NULL);

}


// *****************************************************************************
// *****Function definitions:*****
// *****************************************************************************

// Least-squares polynomial fitting using Eigen. Based on this example:
// https://towardsdatascience.com/least-square-polynomial-fitting-using-c-eigen-package-c0673728bd01

void polyfit(const vector<double> &x, const vector<double> &y,
		vector<double> &coeff, int order)
{

    // Create matrix placeholder of size n x k, n = number of datapoints, k = 
    // order of polynomial, for example k = 3 for cubic polynomial:

    Eigen::MatrixXd X(x.size(), order + 1);
    Eigen::VectorXd Y = Eigen::VectorXd::Map(&y.front(), y.size());
    Eigen::VectorXd result;

    // Check to make sure inputs are correct:

    assert(x.size() == y.size());
    assert(x.size() >= order + 1);
	
    // Populate the matrix:

    for(size_t i = 0 ; i < x.size(); ++i)  {
	for(size_t j = 0; j < order + 1; ++j)  {
	    X(i, j) = pow(x.at(i), j);
	}
    }
	
    // Solve for the linear least-squares fit:

    result  = X.householderQr().solve(Y);
    coeff.resize(order+1);
    for (int k = 0; k < order+1; k++)  {
	coeff[k] = result[k];
    }

}


// Evaluate a polynomial of order k

double polyeval(const vector<double> &coeff, double testval)
{

    double add = 0.0;
    add += coeff[0];

    for(int i = 1; i < coeff.size(); i++) {
		add += coeff[i]*pow(testval, i);
    }

    return add;

}


// Search for the closest element in a sorted vector. Reference:
// https://alexsm.com/cpp-closest-lower-bound/

int search_closest(const vector<double>& sorted, double x) {

    auto iter = lower_bound(sorted.begin(), sorted.end(), x); 

    if (iter == sorted.begin()) {
        return 0;
    }

    double a = *(iter - 1);
    double b = *(iter);

    if (fabs(x - a) < fabs(x - b)) {
        return iter - sorted.begin() - 1;
    }

    return iter - sorted.begin();

}

// Search for the closest element in an unsorted vector
// Works fine for the focus motor count array sizes we have

int closest_index(const vector<double>& dnvals, double x) 
{
    int current_index = 0;

    // Stores the closest value:
    double current_closest = dnvals[0];
 
    for (int i = 1; i < dnvals.size(); i++) {
        if (abs(x - current_closest) > abs(x - dnvals[i])) {
            current_closest = dnvals[i];
	    current_index = i;
        }
    }
 
    // Return the index for the closest array element:
    return current_index;

}


// Compute a value for 't' to compute the intersection point using the ray
// parametric equation. Explanation here:
// https://stackoverflow.com/questions/23975555/how-to-do-ray-plane-intersection
// https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-plane-and-ray-disk-intersection
//NOTE: The intersecting point is provided by rayorigin + t*raydirection

bool intersectPlane(const PigVector &planenormal, const PigPoint &offset, const PigPoint &rayorigin, const PigVector &raydirection, double &t) 
{ 

    // Compute dot product <planenormal, raydirection>: 
    double denom = planenormal % raydirection;

    if (abs(denom) > 1e-6) { 
        PigVector diff = offset - rayorigin; 

        // Compute dot product <diff, planenormal>/denom:
	t = (diff % planenormal)/denom;

	return true;

    } 
 
    return false; 

} 

