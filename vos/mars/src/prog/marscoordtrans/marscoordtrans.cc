/* marscoordtrans */

#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigCAHVORE.h"		// also gets PigCAHVOR.h and PigCAHV.h
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"

#include "return_status.h"

#include <math.h>

#ifdef DEBUG
#define DPR(x) { char msg[256]; x; zvmessage(msg,""); }
#else
#define DPR(x)
#endif

#define EPSILON 0.0000001

////////////////////////////////////////////////////////////////////////

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 1000
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define MAX_OBUF 100000

#define MAX_OUT_COORDS 1000	// for parameters only
#define MAX_IN_COORDS 1000	// for parameters only

typedef enum { Cylindrical, Polar, Vertical } ProjectionType;
typedef enum { Input, Projection } CoordinateType;
typedef enum { Flat, TwoLayer } FileType;

struct MarsMapProjArgs {
  PigCameraModel **camera_in;
  PigSurfaceModel *surface_model;
  ProjectionType proj_mode;           // projection mode
  double scale;                       // radians/pixel, cyl/polar only
  PigPoint proj_origin;               // cylindrical/polar only
  double line_zero_el;                // cylindrical only
  double az_first_sample;             // cylindrical only
  double az_last_sample;              // cylindrical only (info only)
  double up_azimuth;                  // polar only
  int nadir_line, nadir_samp;         // polar only
  int nlo, nso;                       // vertical only (valid for all)
  double vert_scale;                  // pixels/meter, vertical only
  double maxx, maxy;                  // meters, vertical only
  double minx, miny;                  // meters, vertical only
  double min_elev, max_elev;          // cylindrical only (info only)
  PigCoordSystem *cs;                 // projection coord system (all)
  int azdir;                          // azimuth direction from cs (all)
  // THE ITEMS BELOW ARE NOT IN MARSMAP'S STRUCTURE!!
  int nids;				// # of inputs.
  PigFileModel **file_models;
  double FOV[MAX_INPUTS];             // cache for efficiency
  PigVector camera_orientation[MAX_INPUTS];  // cache for efficiency
  PigPoint camera_position[MAX_INPUTS];      // cache for efficiency
};

////////////////////////////////////////////////////////////////////////

void do_coordinate(double ic[3], MarsMapProjArgs *proj,
		   int print, int use_output_file, FILE *out_file,
		   int use_param, int &n_output_coords, double output_coords[],
		   CoordinateType coord_type, int from_mosaic, int multi_match);

void xlate_mosaic_input(double ic[3], double oc[3], MarsMapProjArgs *proj,
			int i, int &found);
void xlate_input_mosaic(double ic[3], double oc[3], MarsMapProjArgs *proj);
void xlate_mosaic_proj(double ic[3], double oc[3], MarsMapProjArgs *proj);
void xlate_proj_mosaic(double ic[3], double oc[3], MarsMapProjArgs *proj);

////////////////////////////////////////////////////////////////////////

void main44()
{
  int i, j /*, k */;
  int status = 0;
  int count, def;
  const int BUF_LEN = 150;
  char msg[BUF_LEN];

  PigSurfaceModel *surface_model = NULL;
  char mission[64], instrument[64];

  // Inputs

  // int band;
  int nids;

  PigFileModel *file_models[MAX_INPUTS];
  PigFileModel *mosaic_file;
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  int homogeneous_inputs = TRUE;

  // double FOV[MAX_INPUTS];                             // cache for efficiency
  // PigVector camera_orientation[MAX_INPUTS];           // cache for efficiency
  // PigPoint camera_position[MAX_INPUTS];               // cache for efficiency

  PigCoordSystem *proj_cs = NULL;
  PigCoordSystem *param_cs = NULL;
  int azdir = 1;				// +1 == CW, -1 == CCW
  // PigMission *m;

  // Coordinate management

  double input_coords[MAX_IN_COORDS];
  int n_input_coords;
  double output_coords[MAX_OUT_COORDS];
  int n_output_coords;
  char in_filename[PIG_MAX_FILENAME_SIZE];
  char out_filename[PIG_MAX_FILENAME_SIZE];
  int use_input_file;
  int use_output_file = 0;
  FILE *in_file = NULL;
  FILE *out_file = NULL;

  // User Parameters

  CoordinateType coord_type;
  int from_mosaic;			// true = from, false = to
  int multi_match;
  int use_param = 0;
  int print = 0;
  FileType file_type;

  // Projection

  ProjectionType proj_mode = Cylindrical;
  double scale_y=0, scale_x=0, scale=0;	// radians/pixel, cyl/polar
  PigPoint proj_origin;			// cyl/polar
  double line_zero_el=0;			// cyl
  double start_az=0, stop_az=0;		// cyl
  double min_elev=0, max_elev=0;		// cyl, cyl/polar
  double up_azimuth=0;			// polar
  int nadir_line=0, nadir_samp=0;		// polar
  double maxx=0, maxy=0;			// vert
  double minx=0, miny=0;                      // vert
  double vert_scale=0;			// pixels/meter, vert
  double line_offset=0, samp_offset=0;        // vertical projection only
  int nlo = 0, nso = 0;

  MarsMapProjArgs proj;

  char mosaic_filename[PIG_MAX_FILENAME_SIZE];

  double dbl_param;
  int int_param;

  ////////////////////////////////////////////////////////////////////////

  zvmessage("MARSCOORDTRANS version 2020-05-05", "");
 
  if (zvptst("INPUT"))
    coord_type = Input;
  else
    coord_type = Projection;

  from_mosaic = zvptst("FROM_MOSAIC");
  multi_match = zvptst("MULTI");

  use_param = zvptst("USE_PARM");
  print = zvptst("PRINT");
  if (zvptst("FLAT"))
    file_type = Flat;
  else
    file_type = TwoLayer;

  ////////////////////////////////////////////////////////////////////////
  // If input files are given, set them up
  ////////////////////////////////////////////////////////////////////////

  zvpcnt("INP", &nids);
  if (nids != 0) {

    mars_setup(nids, file_models, camera_in, pointing_in, NULL,
	       param_cs, mission, instrument, homogeneous_inputs,
	       MAX_NL, MAX_NS, MAX_INPUTS);

    status = mars_apply_navtable(nids, pointing_in, file_models);
    if (status >= 2)		// 1 means user didn't ask for file
      zabend();			// msg already printed

  } else {		// No input files

    if (coord_type == Input) {
      zvmessage("INPUT coordinates require input files be specified!","");
      zabend();
    }

  }

  ////////////////////////////////////////////////////////////////////////
  // Load in the mosaic and its parameters
  ////////////////////////////////////////////////////////////////////////

  zvp("MOSAIC", mosaic_filename, &count);
  if (count != 1) {		// should never happen
    zvmessage("Internal ERROR: MOSAIC count is not 1!", "");
    zabend();
  }

  mosaic_file = PigFileModel::create(mosaic_filename);
  if (mosaic_file == NULL) {
    zvmessage("Unable to create file model for mosaic INP file", "");
    zabend();
  }

  if (nids == 0) {			// haven't set up PIG yet
    strcpy(mission, mosaic_file->getMissionName());
    mars_read_rsf(mission);
    param_cs = mars_setup_coords(mission, 1, &mosaic_file);
  }

  // Get projection type from mosaic
  //!!!! THIS SHOULD PROBABLY MOVE TO PIG eventually

  const LblSurfaceProjection_typ *sp = mosaic_file->getLblSurfaceProjection();
  if (sp == NULL) {
    zvmessage("Unable to find mosaic projection label", "");
    zabend();
  }
  if (!sp->MapProjectionType.Valid) {
    zvmessage("Unable to find mosaic projection type!", "");
    zabend();
  }

  ////////////////////////////////////////////////////////////////////////
  // CYLINDRICAL PROJECTION
  ////////////////////////////////////////////////////////////////////////

  if (strcasecmp(sp->MapProjectionType.Value, "CYLINDRICAL") == 0) {

    proj_mode = Cylindrical;

    status = mosaic_file->getCylindricalProjectionParams(
							 &surface_model, scale_y, scale_x, proj_origin, line_zero_el,
							 start_az, stop_az, min_elev, max_elev, &proj_cs);
    surface_model->setCoordSystem(proj_cs);	// just in case...
    azdir = proj_cs->getAzimuthDirection();
    nlo = mosaic_file->getNL();
    nso = mosaic_file->getNS();

    if (fabs(scale_x - scale_y) > EPSILON) {
      zvmessage("X and Y scales differ!!  This is not supported.", "");
      zvmessage("X scale used.", "");
    }
    scale = scale_x;

    snprintf(msg, BUF_LEN, "Mosaic is projected in the %s coordinate frame",
	     proj_cs->getFrameName());
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Elevation minimum %f, Elevation maximum %f",
	     PigRad2Deg(min_elev), PigRad2Deg(max_elev));
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Azimuth minimum %f, Azimuth maximum %f",
	     PigRad2Deg(start_az), PigRad2Deg(stop_az));
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Projection origin = (%f, %f, %f)",
	     proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Pixel scale: %f radians/pixel or %f pixels/degree",
	     scale, 1.0/PigRad2Deg(scale));
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Mosaic lines = %d, samples = %d", nlo, nso);
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "line of zero elevation = %f", line_zero_el);
    zvmessage(msg, "");

    // Override projection parameters

    zvparmd("SCALE", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      scale = PigDeg2Rad(1.0 / dbl_param);
      snprintf(msg, BUF_LEN,
	       "Override: Pixel scale: %f radians/pixel or %f pixels/degree",
	       scale, 1.0 / PigRad2Deg(scale));
      zvmessage(msg, "");
      line_zero_el = max_elev / scale;
      snprintf(msg, BUF_LEN, "Line of zero elevation changed to %f", line_zero_el);
      zvmessage(msg, "");
    }
    zvparmd("LEFTAZ", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      start_az = PigDeg2Rad(dbl_param);
      snprintf(msg, BUF_LEN, "Override: Starting Azimuth (left) = %f",
	       PigRad2Deg(start_az));
      zvmessage(msg, "");
    }
    zvparmd("RIGHTAZ", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      stop_az = PigDeg2Rad(dbl_param);
      snprintf(msg, BUF_LEN, "Override: Ending Azimuth (right) = %f",
	       PigRad2Deg(stop_az));
      zvmessage(msg, "");
    }
    zvparmd("BOTTOMEL", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      min_elev = PigDeg2Rad(dbl_param);
      snprintf(msg, BUF_LEN, "Override: Elevation Minimum = %f",
	       PigRad2Deg(min_elev));
      zvmessage(msg, "");
    }
    zvparmd("TOPEL", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      max_elev = PigDeg2Rad(dbl_param);
      snprintf(msg, BUF_LEN, "Override: Elevation Maximum = %f",
	       PigRad2Deg(max_elev));
      zvmessage(msg, "");
      line_zero_el = max_elev / scale;
      snprintf(msg, BUF_LEN, "Line of zero elevation changed to %f", line_zero_el);
      zvmessage(msg, "");
    }
    zvpcnt("PROJ_ORIGIN", &count);
    if (count == 3) {
      double array[3];
      zvparmd("PROJ_ORIGIN", array, &count, &def, 3, 0);
      proj_origin.setXYZ(array);
      snprintf(msg, BUF_LEN, "Override: Projection origin = (%f, %f, %f)",
	       proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
      zvmessage(msg, "");
    }
  }

  ////////////////////////////////////////////////////////////////////////
  // POLAR PROJECTION
  ////////////////////////////////////////////////////////////////////////

  else if (strcasecmp(sp->MapProjectionType.Value, "POLAR") == 0) {
       
    proj_mode = Polar;
    // scale alone should be use, but we verify that the scales are not
    // different, similar to cylindrical
    status = mosaic_file->getPolarProjectionParams(
						   &surface_model, scale_y, scale_x, proj_origin, up_azimuth, 
						   nadir_line, nadir_samp, max_elev, &proj_cs);
    surface_model->setCoordSystem(proj_cs);	// just in case...
    azdir = proj_cs->getAzimuthDirection();
    nlo = mosaic_file->getNL();
    nso = mosaic_file->getNS();
    if (fabs(scale_x - scale_y) > EPSILON) {
      zvmessage("X and Y scales differ!!  This is not supported.", "");
      zvmessage("X scale used.", "");
    }
    scale = scale_x;

    snprintf(msg, BUF_LEN, "Mosaic is projected in the %s coordinate frame",
	     proj_cs->getFrameName());
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Elevation maximum %f", PigRad2Deg(max_elev));
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Projection origin = (%f, %f, %f)",
	     proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Pixel scale: %f radians/pixel or %f pixels/degree",
	     scale, 1.0/PigRad2Deg(scale));
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Mosaic lines = %d, samples = %d", nlo, nso);
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Nadir line = %d, Nadir sample = %d",
	     nadir_line, nadir_samp);
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Reference (Up) Azimuth: %f", PigRad2Deg(up_azimuth));
    zvmessage(msg, "");

    // Override projection parameters

    zvparmd("SCALE", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      scale = PigDeg2Rad(1.0 / dbl_param);
      snprintf(msg, BUF_LEN,
	       "Override: Pixel scale: %f radians/pixel or %f pixels/degree",
	       scale, 1.0 / PigRad2Deg(scale));
      zvmessage(msg, "");
      line_zero_el = max_elev / scale;
      snprintf(msg, BUF_LEN, "Line of zero elevation changed to %f", line_zero_el);
      zvmessage(msg, "");
    }
    zvpcnt("PROJ_ORIGIN", &count);
    if (count == 3) {
      double array[3];
      zvparmd("PROJ_ORIGIN", array, &count, &def, 3, 0);
      proj_origin.setXYZ(array);
      snprintf(msg, BUF_LEN, "Override: Projection origin = (%f, %f, %f)",
	       proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
      zvmessage(msg, "");
    }
    zvparmd("UP_AZ", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      up_azimuth = PigDeg2Rad(dbl_param); 
      snprintf(msg, BUF_LEN,
	       "Override: Azimuth at top of mosaic = %f", PigRad2Deg(up_azimuth));
      zvmessage(msg, "");
    }
    zvparmd("NADIR_LINE", &int_param, &count, &def, 1, 0);
    if (count > 0) {
      nadir_line = int_param;
      snprintf(msg, BUF_LEN,
	       "Override: Nadir line pixel = %d", nadir_line);
      zvmessage(msg, "");
    }
    zvparmd("NADIR_SAMP", &int_param, &count, &def, 1, 0);
    if (count > 0) {
      nadir_samp = int_param;
      snprintf(msg, BUF_LEN,
	       "Override: Nadir sample pixel = %d", nadir_samp);
      zvmessage(msg, "");
    }
  }

  ////////////////////////////////////////////////////////////////////////
  // VERTICAL PROJECTION
  ////////////////////////////////////////////////////////////////////////

  else if (strcasecmp(sp->MapProjectionType.Value, "VERTICAL") == 0) {
    	
    proj_mode = Vertical;

    // scale alone should be use, but we verify that the scales are not
    // different, similar to cylindrical
    int proj_type = 0;
    status = mosaic_file->getVertOrthoProjectionParams(
						       &surface_model, scale_y, scale_x, line_offset,
						       samp_offset, minx, maxx, miny, maxy, proj_type, &proj_cs);
    surface_model->setCoordSystem(proj_cs);	// just in case...
    azdir = proj_cs->getAzimuthDirection();
    nlo = mosaic_file->getNL();
    nso = mosaic_file->getNS();

    if (fabs(scale_y - scale_x) > EPSILON) {
      zvmessage("Line and Sample scales differ!!  This is not supported.", "");
      zvmessage("Line scale used.", "");
    }
    vert_scale = scale_y;

    snprintf(msg, BUF_LEN, "Mosaic is projected in the %s coordinate frame",
	     proj_cs->getFrameName());
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Pixel scale = %.2f meters/pixel or %.1f pixels/meter",
	     vert_scale, 1.0 / vert_scale);
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Mosaic lines = %d, samples = %d", nlo, nso);
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Min X = %f, Min Y = %f", minx, miny);
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Max X = %f, Max Y = %f", maxx, maxy);
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Line Offset = %f, Samp Offset = %f", 
	     line_offset, samp_offset);
    zvmessage(msg, "");

    // Override projection parameters

    zvparmd("VERT_SCALE", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      vert_scale = 1.0 / dbl_param;
      snprintf(msg, BUF_LEN,
	       "Override: Pixel scale: %f meters/pixel or %f pixels/meter",
	       vert_scale, 1.0 / vert_scale);
      zvmessage(msg, "");
    }
    zvpcnt("PROJ_ORIGIN", &count);
    if (count == 3) {
      double array[3];
      zvparmd("PROJ_ORIGIN", array, &count, &def, 3, 0);
      proj_origin.setXYZ(array);
      snprintf(msg, BUF_LEN, "Override: Projection origin = (%f, %f, %f)",
	       proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
      zvmessage(msg, "");
    }
    zvparmd("MAXX", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      maxx = dbl_param;
      snprintf(msg, BUF_LEN,
	       "Override: Maximum X: %f meters", maxx);
      zvmessage(msg, "");
    }
    zvparmd("MAXY", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      maxy = dbl_param;
      snprintf(msg, BUF_LEN,
	       "Override: Maximum Y: %f meters", maxy);
      zvmessage(msg, "");
    }
    zvparmd("MINX", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      minx = dbl_param;
      snprintf(msg, BUF_LEN,
	       "Override: Minimum X: %f meters", minx);
      zvmessage(msg, "");
    }
    zvparmd("MINY", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
      miny = dbl_param;
      snprintf(msg, BUF_LEN,
	       "Override: Minimum Y: %f meters", miny);
      zvmessage(msg, "");
    }
  } 

  ////////////////////////////////////////////////////////////////////////
  // !!!! OTHER PROJECTIONS !!!!
  ////////////////////////////////////////////////////////////////////////

  else {
    snprintf(msg, BUF_LEN,
	     "Coordinate transformations for the %s projection has not yet been implemented.",
	     sp->MapProjectionType.Value);
    zvmessage(msg, "");
    zabend();
  }

  ////////////////////////////////////////////////////////////////////////
  // Set up projection structure
  ////////////////////////////////////////////////////////////////////////

  proj.camera_in = camera_in;
  proj.surface_model = surface_model;
  proj.proj_mode = proj_mode;
  proj.scale = scale;
  proj.proj_origin = proj_origin;
  proj.line_zero_el = line_zero_el;
  proj.az_first_sample = start_az;
  proj.az_last_sample = stop_az;
  proj.up_azimuth = up_azimuth;
  proj.nadir_line = nadir_line;
  proj.nadir_samp = nadir_samp;
  proj.nlo = nlo;
  proj.nso = nso;
  proj.vert_scale = vert_scale;
  proj.maxx = maxx;
  proj.maxy = maxy;
  proj.minx = minx;
  proj.miny = miny;
  proj.min_elev = min_elev;
  proj.max_elev = max_elev;
  proj.cs = proj_cs;
  proj.azdir = azdir;
  proj.nids = nids;
  proj.file_models = file_models;

  for (i=0; i < nids; i++) {
    proj.FOV[i] = cos((file_models[i]->getFOV(camera_in[i], 0) +
		       file_models[i]->getFOV(camera_in[i], 1))/2);
    if (proj.FOV[i] < 0.0)		// Limit to 90 degrees
      proj.FOV[i] = 0.0;
    if (proj.FOV[i] > .866)		// but at least 30 degrees
      proj.FOV[i] = .866;

    proj.camera_position[i] = pointing_in[i]->getCameraPosition(proj_cs);
    proj.camera_orientation[i] =
      pointing_in[i]->getCameraOrientation(proj_cs);
  }

  ////////////////////////////////////////////////////////////////////////
  // Set up outputs...
  ////////////////////////////////////////////////////////////////////////

  n_output_coords = 0;
  zvpcnt("IN_COORDS", &n_input_coords);

  zvp("IN_FILE", in_filename, &count);
  use_input_file = (count != 0);

  zvp("OUT_FILE", out_filename, &count);
  use_output_file = (count != 0);

  // use_param, print already set up

  if (use_output_file && file_type == TwoLayer && n_input_coords != 0) {
    zvmessage(
	      "TwoLayer output file cannot be used with IN_COORDS parameter", "");
    zabend();
  }

  if (use_output_file) {
    out_file = fopen(out_filename, "w");
    if (out_file == NULL) {
      snprintf(msg, BUF_LEN, "Error opening output file '%s'", out_filename);
      zvmessage(msg, "");
      zabend();
    }
  }

  ////////////////////////////////////////////////////////////////////////
  // Read coordinate inputs
  ////////////////////////////////////////////////////////////////////////

  double ic[3];

  if (n_input_coords != 0) {
    zvparmd("IN_COORDS", &input_coords, &n_input_coords, &def,
	    MAX_IN_COORDS, 0);

    int i = 0;

    while (i < n_input_coords) {
      ic[0] = input_coords[i++];
      ic[1] = input_coords[i++];
      if (coord_type == Input && !from_mosaic) {	// Input has 3 values
	ic[2] = input_coords[i++];
        if (int(ic[0]) > nids) {
          zvmessage("The image index of the input coordinate is invalid.", "");
          snprintf(msg, BUF_LEN, "Requested index: %.0f, Total images: %d", ic[0], nids);
          zvmessage(msg, "");
          zabend();
        }
      }
      if (i > n_input_coords) {
	zvmessage("Not enough input coords to make a set!", "");
	zvmessage("Results may be suspect", "");
      }
      

      do_coordinate(ic, &proj, print, use_output_file, out_file,
		    use_param, n_output_coords, output_coords,
		    coord_type, from_mosaic, multi_match);
    }
  }

  ////////////////////////////////////////////////////////////////////////
  // Read file inputs
  ////////////////////////////////////////////////////////////////////////

  if (use_input_file) {

    in_file = fopen(in_filename, "r");
    if (in_file == NULL) {
      snprintf(msg, BUF_LEN, "Error opening input file '%s'", in_filename);
      zvmessage(msg, "");
      zabend();
    }

    char line[256];
    char *eof;

    if (file_type == TwoLayer) {		// Two-layer file

      int nsets = 0;
      int npoints = 0;

      eof = fgets(line, 255, in_file);
      if (eof == NULL) {
	zvmessage("Premature end of input file reading nsets", "");
	zabend();
      }
      sscanf(line, "%d", &nsets);

      if (use_output_file)
	fprintf(out_file, "%d\n", nsets);

      for (i=0; i < nsets; i++) {

	eof = fgets(line, 255, in_file);
	if (eof == NULL) {
	  snprintf(msg, BUF_LEN,
		   "Premature end of input file reading npoints for set %d",i);
	  zvmessage(msg, "");
	  zabend();
	}
	sscanf(line, "%d", &npoints);

	if (use_output_file)
	  fprintf(out_file, "%d\n", npoints);

	for (j=0; j < npoints; j++) {

	  eof = fgets(line, 255, in_file);
	  if (eof == NULL) {
	    snprintf(msg, BUF_LEN,
		     "Premature end of input file in set %d, point %d",
		     i, j);
	    zvmessage(msg, "");
	    zabend();
	  }
	  if (coord_type == Input && !from_mosaic) {	// Input has 3
	    sscanf(line, "%lf %lf %lf", &ic[0], &ic[1], &ic[2]);
	  }
	  else {
	    sscanf(line, "%lf %lf", &ic[0], &ic[1]);
	  }
	  do_coordinate(ic, &proj, print, use_output_file, out_file,
			use_param, n_output_coords, output_coords,
			coord_type, from_mosaic, multi_match);
	}
      }
    }

    else {		// FLAT file
      while (TRUE) {
	eof = fgets(line, 255, in_file);
	if (eof == NULL)
	  break;			// done!
	if (coord_type == Input && !from_mosaic) {	// Input has 3
	  sscanf(line, "%lf %lf %lf", &ic[0], &ic[1], &ic[2]);
	}
	else {
	  sscanf(line, "%lf %lf", &ic[0], &ic[1]);
	}
	do_coordinate(ic, &proj, print, use_output_file, out_file,
		      use_param, n_output_coords, output_coords,
		      coord_type, from_mosaic, multi_match);
      }
    }

    fclose(in_file);

  }

  if (use_output_file) {
    fclose(out_file);
  }

  //!!!! OUTPUT PARAMETER TO TAE!!!!
  if (use_param != 0)
    zvmessage("Parameter USE_PARM has not yet been implemented!", "");
   

}

////////////////////////////////////////////////////////////////////////
// Process and output one coordinate
////////////////////////////////////////////////////////////////////////

void do_coordinate(double ic[3], MarsMapProjArgs *proj,
		   int print, int use_output_file, FILE *out_file,
		   int use_param, int &n_output_coords, double output_coords[],
		   CoordinateType coord_type, int from_mosaic, int multi_match)
{
  double oc[3];
  const int BUF_LEN = 256;
  char msg[BUF_LEN];

  // This case is more challenging because we must search

  if (coord_type == Input && from_mosaic) {

    const int BIG_BUF_LEN = 2048;
    char print_line[BIG_BUF_LEN];

    if (print)
      snprintf(print_line, BIG_BUF_LEN, "(%lf, %lf) ->", ic[0], ic[1]);
    else
      print_line[0] = '\0';

    for (int i=0; i < proj->nids; i++) {

      int found;
      xlate_mosaic_input(ic, oc, proj, i, found);

      if (found) {

	if (print) {
	  snprintf(msg, BUF_LEN, " (%lf, %lf, %lf)", oc[0], oc[1], oc[2]);
	  strcat(print_line, msg);
	}
	if (use_output_file) {
	  fprintf(out_file, "%lf %lf %lf ", oc[0], oc[1], oc[2]);
	}
	if (use_param) {
	  if (n_output_coords + 3 >= MAX_OUT_COORDS) {
	    zvmessage("Exceeding maximum output coords; truncating",
		      "");
	  }
	  else {
	    output_coords[n_output_coords++] = oc[0];
	    output_coords[n_output_coords++] = oc[1];
	    output_coords[n_output_coords++] = oc[2];
	  }
	}

	if (!multi_match)
	  break;			// stop looping if single
      }
    }
    if (print)
      zvmessage(print_line, "");
    if (use_output_file)
      fprintf(out_file, "\n");

  }

  else {		// The three other modes are easier...

    if (coord_type == Input && !from_mosaic) {
      xlate_input_mosaic(ic, oc, proj);
    } else if (coord_type == Projection && from_mosaic) {
      xlate_mosaic_proj(ic, oc, proj);
    } else if (coord_type == Projection && !from_mosaic) {
      xlate_proj_mosaic(ic, oc, proj);
    } else {
      zvmessage("Internal logic error in do_coordinate!!", "");
      zabend();
    }

    if (print) {
      if (coord_type == Input) {
	snprintf(msg, BUF_LEN, "(%lf, %lf, %lf) -> (%lf, %lf)",
		 ic[0], ic[1], ic[2], oc[0], oc[1]);
      } else {
	snprintf(msg, BUF_LEN, "(%lf, %lf) -> (%lf, %lf)",
		 ic[0], ic[1], oc[0], oc[1]);
      }
      zvmessage(msg, "");
    }

    if (use_output_file) {
      fprintf(out_file, "%lf %lf\n", oc[0], oc[1]);
    }
    if (use_param) {
      if (n_output_coords + 2 >= MAX_OUT_COORDS) {
	zvmessage("Exceeding maximum output coords; truncating", "");
      }
      else {
	output_coords[n_output_coords++] = oc[0];
	output_coords[n_output_coords++] = oc[1];
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////
// Translate mosaic to input coordinates for the given input.
// Note:  Both coordinates, and the image number, are 1-based!!
// However, i (the image number to look at) is 0-based.  Sigh.
////////////////////////////////////////////////////////////////////////

void xlate_mosaic_input(double ic[3], double oc[3], MarsMapProjArgs *proj,
			int i, int &found)
{
  // We add 0.5 just to avoid any roundoff issues (e.g. image 5.9999)
  double mos_line = ic[0] - 1.0;		// make 0-based
  double mos_samp = ic[1] - 1.0;

  PigVector look;
  PigPoint surf_pt, proj_pt;
  double out_az, out_el;
  double x_ctr, y_ctr;
  PigVector polar;
  int infinity = 0, hits;
  double in_line, in_samp;
  double image_line, image_samp;

  found = FALSE;

  switch (proj->proj_mode) {

  case Cylindrical:
    out_az = (proj->azdir * mos_samp * proj->scale) +
      proj->az_first_sample;
    out_el = (proj->line_zero_el - mos_line) * proj->scale;
    look = proj->cs->constructVector(out_az, out_el);
    hits = proj->surface_model->intersectRay(proj->proj_origin,
					     look, surf_pt);
    infinity = (hits <= 0);

    break;

  case Polar:
    x_ctr = mos_samp - (proj->nadir_samp - 1);
    y_ctr = (proj->nadir_line - 1) - mos_line;
    polar.setXYZ(x_ctr, y_ctr, 0.0);
    out_el = polar.getRange() * proj->scale - PigDeg2Rad(90.0);
    // PigVector az is CCW from +X, we want CW/CCW from +Y
    out_az = proj->up_azimuth +
      (PigDeg2Rad(90.0) - polar.getAz()) * proj->azdir;
    look = proj->cs->constructVector(out_az, out_el);
    hits = proj->surface_model->intersectRay(proj->proj_origin,
					     look, surf_pt);
    infinity = (hits <= 0);

    break;

  case Vertical:
    x_ctr = (proj->nlo/2 - mos_line) * proj->vert_scale;  // +X is up
    y_ctr = proj->azdir * (mos_samp - proj->nso/2) * proj->vert_scale;
    if (x_ctr > proj->maxx || y_ctr > proj->maxy || x_ctr < proj->minx || y_ctr < proj->miny) {
      return;
    }
    // +Y is right for azdir=1, left for -1
    proj_pt.setXYZ(x_ctr, y_ctr, 0.0);

    {
      PigVector down(0.0, 0.0, -1.0), up(0.0, 0.0, 1.0);
      down = down * proj->cs->getElevationDirection();
      up = up * proj->cs->getElevationDirection();

      hits = proj->surface_model->intersectRay(proj_pt, down, 1,
					       surf_pt);
      if (hits <= 0)
	hits = proj->surface_model->intersectRay(proj_pt, up, 1,
						 surf_pt);
    }

    infinity = (hits <= 0);

    // Infinity case is undefined for vertical (there's no "look")

    if (infinity)
      return;

    break;

  default:
    zvmessage("Internal error: bad proj_mode!!", "");
    zabend();
  }


  // see marsmap for comments

  if (infinity) {
    if ((look % proj->camera_orientation[i]) < proj->FOV[i])
      return;		// no hit
  }
  else {
    PigVector new_look = surf_pt - proj->camera_position[i];
    new_look.normalize();
    if ((new_look % proj->camera_orientation[i]) < proj->FOV[i])
      return;		// no hit
  }

  proj->camera_in[i]->XYZtoLS(surf_pt, infinity, &in_line, &in_samp,
			      proj->cs);

  if (proj->file_models[i]->testPixelLocation(in_line,in_samp) != 0)
    return;			// no hit

  image_line = in_line - proj->file_models[i]->getYOffset();
  image_samp = in_samp - proj->file_models[i]->getXOffset();

  oc[0] = i + 1;			// make 1-based
  oc[1] = image_line + 1.0;		// make 1-based
  oc[2] = image_samp + 1.0;
  found = TRUE;

}

////////////////////////////////////////////////////////////////////////
// Translate input to mosaic coordinates.
// Note:  Both coordinates, and the image number, are 1-based!!
////////////////////////////////////////////////////////////////////////

void xlate_input_mosaic(double ic[3], double oc[3], MarsMapProjArgs *proj)
{
  // We add 0.5 just to avoid any roundoff issues (e.g. image 5.9999)
  int input = (int)(ic[0] + 0.5) - 1;		// also make 0-based
  double input_line = ic[1] - 1.0;		// make 0-based
  double input_samp = ic[2] - 1.0;

  PigPoint origin;
  PigVector look;
  PigPoint surf_pt;
  int hits;
  double out_az, out_el, out_range, new_az;
  double mos_line = 0.0, mos_samp = 0.0;
  PigPoint polar;

  proj->camera_in[input]->LStoLookVector(input_line, input_samp,
					 origin, look, proj->cs);
  hits = proj->surface_model->intersectRay(origin, look, surf_pt);

  switch (proj->proj_mode) {

  case Cylindrical:
    // getRay returns "look"
    proj->surface_model->getRay(proj->proj_origin, surf_pt, (hits <= 0),
				look);
    out_az = proj->cs->getAz(look);
    out_el = proj->cs->getEl(look);

    new_az = (out_az - proj->az_first_sample) * proj->azdir;
    while (new_az >= PigDeg2Rad(360.0)) new_az -= PigDeg2Rad(360.0);
    while (new_az < 0) new_az += PigDeg2Rad(360.0);
    mos_samp = new_az / proj->scale;
    mos_line = proj->line_zero_el - (out_el / proj->scale);

    break;

  case Polar:
    // getRay returns "look"
    proj->surface_model->getRay(proj->proj_origin, surf_pt, (hits <= 0),
				look);
    out_az = PigDeg2Rad(90.0) -
      (proj->cs->getAz(look) - proj->up_azimuth) * proj->azdir;
    out_range= (proj->cs->getEl(look) + PigDeg2Rad(90.0)) / proj->scale;
    polar.setSpherical(out_az, 0.0, out_range);

    mos_samp = polar.getX() + (proj->nadir_samp - 1);
    mos_line = (proj->nadir_line - 1) - polar.getY();

    break;

  case Vertical:
    mos_line = proj->nlo/2 - (surf_pt.getX() / proj->vert_scale);
    mos_samp = (surf_pt.getY() / proj->vert_scale) * proj->azdir +
      proj->nso/2;

    if (hits <= 0) {
      zvmessage("WARNING!! Point does not hit plane in vert proj","");
      //!!!! what to do??!?!!!!
    }
    break;
  }

  oc[0] = mos_line + 1.0;		// convert to 1-based
  oc[1] = mos_samp + 1.0;

}

////////////////////////////////////////////////////////////////////////
// Translate mosaic to projection coordinates.
// Note:  The mosaic coordinates are 1-based, and proj is in degrees!!
////////////////////////////////////////////////////////////////////////

void xlate_mosaic_proj(double ic[3], double oc[3], MarsMapProjArgs *proj)
{
  double mos_line = ic[0] - 1.0;		// make 0-based
  double mos_samp = ic[1] - 1.0;

  double out_az, out_el;
  double x_ctr, y_ctr;
  PigVector polar;

  switch (proj->proj_mode) {
  case Cylindrical:
    out_az = (proj->azdir * mos_samp * proj->scale) +
      proj->az_first_sample;
    out_el = (proj->line_zero_el - mos_line) * proj->scale;

    oc[0] = PigRad2Deg(out_az);
    oc[1] = PigRad2Deg(out_el);
    break;

  case Polar:
    x_ctr = mos_samp - (proj->nadir_samp - 1);
    y_ctr = (proj->nadir_line - 1) - mos_line;
    polar.setXYZ(x_ctr, y_ctr, 0.0);
    out_el = polar.getRange() * proj->scale - PigDeg2Rad(90.0);
    // PigVector az is CCW from +X, we want CW/CCW from +Y
    out_az = proj->up_azimuth +
      (PigDeg2Rad(90.0) - polar.getAz()) * proj->azdir;

    oc[0] = PigRad2Deg(out_az);
    oc[1] = PigRad2Deg(out_el);
    break;

  case Vertical:
    x_ctr = (proj->nlo/2 - mos_line) * proj->vert_scale;   // +X is up
    y_ctr = proj->azdir * (mos_samp - proj->nso/2) * proj->vert_scale;
    // +y is right for azdir=1, left for -1

    oc[0] = x_ctr;
    oc[1] = y_ctr;
    break;

  default:
    zvmessage("Internal error: bad proj_mode!!", "");
    zabend();
  }
}

////////////////////////////////////////////////////////////////////////
// Translate mosaic to projection coordinates.
// Note:  The mosaic coordinates are 1-based, and proj is in degrees!!
////////////////////////////////////////////////////////////////////////

void xlate_proj_mosaic(double ic[3], double oc[3], MarsMapProjArgs *proj)
{
  double mos_line;
  double mos_samp;

  double in_az, in_el;
  double new_az, new_range;
  // double x_ctr, y_ctr;
  PigVector polar;

  switch (proj->proj_mode) {
  case Cylindrical:
    in_az = PigDeg2Rad(ic[0]);
    in_el = PigDeg2Rad(ic[1]);

    new_az = (in_az - proj->az_first_sample) * proj->azdir;
    while (new_az >= PigDeg2Rad(360.0)) new_az -= PigDeg2Rad(360.0);
    while (new_az < 0) new_az += PigDeg2Rad(360.0);
    mos_samp = new_az / proj->scale;
    mos_line = proj->line_zero_el - (in_el / proj->scale);

    oc[0] = mos_line + 1.0;		// make it 1-based
    oc[1] = mos_samp + 1.0;
    break;

  case Polar:
    in_az = PigDeg2Rad(ic[0]);
    in_el = PigDeg2Rad(ic[1]);

    new_az = PigDeg2Rad(90.0)- (in_az - proj->up_azimuth) * proj->azdir;
    new_range = (in_el + PigDeg2Rad(90.0)) / proj->scale;
    polar.setSpherical(new_az, 0.0, new_range);

    mos_samp = polar.getX() + (proj->nadir_samp - 1);
    mos_line = (proj->nadir_line - 1) - polar.getY();

    oc[0] = mos_line + 1.0;		// make it 1-based
    oc[1] = mos_samp + 1.0;
    break;

  case Vertical:

    mos_line = proj->nlo/2 - (oc[0] / proj->vert_scale);  // +X is up
    mos_samp = (oc[1] / proj->vert_scale) * proj->azdir + proj->nso/2;
    // +Y is right for azdir=1, left for -1

    oc[0] = mos_line + 1.0;		// make it 1-based
    oc[1] = mos_samp + 1.0;
    break;

  default:
    zvmessage("Internal error: bad proj_mode!!", "");
    zabend();
  }
}
