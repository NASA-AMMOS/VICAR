/* error_analysis */
#include <math.h>
#include <iostream>
using namespace std;

#include "vicmain_c"
#include "zvprintf.h"
#include "zmabend.h"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "mat3.h"

#include "return_status.h"

#include "SimpleImage.h"

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NS 0		// no limits
#define MAX_NL 0
#define BAD_DELTA -999999

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif

static int xvector(const PigPoint cam1, const PigPoint cam2,
		   const PigVector uvw1, const PigVector uvw2,
		   PigPoint &xyz, double *error);
static void dsimq(double A[10], double B[10], int N, int *KS);

void  lcross(double b1[3],
	     double v1[3],
	     double b2[3],
	     double v2[3],
	     double *k1,
	     double p1[3],
	     double *k2,
	     double p2[3],
	     int *colinear);

void compute_box_filter(SimpleImage<double> &input, SimpleImage<double> &output,
			int box_height, int box_width, double ignore);

void findErrors( PigCameraModel **cm, PigCoordSystem *cs,
		 const double orig_l, const double orig_s, 
		 const double disp_l, const double disp_s, 
		 const double delta_ref_line,  const double delta_ref_samp,
		 const double delta_disp_line, const double delta_disp_samp,
		 const PigPoint xyz,
		 double& range_err,
		 double& xe, double& ye, double& ze, double& vxyz, 
		 double& dr, double& c1, double& c2, double& vm, 
		 const PigPoint orig_origin );

void test_findErrors( PigCameraModel **cm, PigCoordSystem *cs,
		      const double orig_l, const double orig_s, 
		      const double disp_l, const double disp_s, const PigPoint xyz, 
		      double& dr, double& c1, double& c2, double& vm );

void open_inputs(  int nids, PigFileModel *file_models[], int unit[3], int band[3], char *type );
void open_inputs2( int nids, PigFileModel *file_models[], int unit[2], int band[2], char *type );


////////////////////////////////////////////////////////////////////////


void main44()
{
  int i, j;
  int /* status, */ count, def;
  int disp_err_count;

  int nids;
  //int inp_unit[2];
  //int inp_band[2];
  int xyz_nids;
  int xyz_unit[3];
  int xyz_band[3];
  // int nfms;
  char mission[64], instrument[64];
  char filename[PIG_MAX_FILENAME_SIZE+1];
  int nl=0, ns=0, nb;
  double /* error_value, */ range;

  // Inputs
	
  // For file models, in addition to creating
  // file model for each input, we also create
  // file models for Disparity Map(s).
  PigFileModel *file_models[MAX_INPUTS+2];
  PigFileModel *xyz_file_models[MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  int homogeneous_inputs = TRUE;
  int disp_unit_line=0, disp_unit_samp=0, disp_band_line=0, disp_band_samp=0;
  int disp_err_line_unit=0, disp_err_samp_unit=0, disp_err_line_band=0, disp_err_samp_band=0;
  // char frame_name[256];
  PigCoordSystem *cs;
		
  // Outputs

  int out_unit[3];	// unit for dr-c1-c2 range error image
  int out_band[3];
  int xyze_unit[3];	// unit for  x- y- z range error image
  int xyz_vol_unit;	// unit for  x- y- z range error ellipsoid volume image
  int rng_vol_unit;	// unit for dr-c1-c2 range error ellipsoid volume image

  SimpleImage<double> xi, yi, zi;		// XYZ image

  int range_unit;
  int range_err_unit;
  int out_erband[3];

  int mask_unit;

  SimpleImage<double> dri;   // down-range error
  SimpleImage<double> c1i;   // cross-range 1 error
  SimpleImage<double> c2i;   // cross-range 2 error
  SimpleImage<double> vmi;   // dr-cr1-cr2 range error volume

  SimpleImage<double> xei;   // x-range error
  SimpleImage<double> yei;   // y-range error
  SimpleImage<double> zei;   // z-range error
  SimpleImage<double> vxyzi; // x-y-z range error volume
	
  SimpleImage<double> disp_line;
  SimpleImage<double> disp_samp;
  SimpleImage<double> range_image;   // range
  SimpleImage<double> range_err_i;   // range error
  SimpleImage<double> mask_image;    // mask
	
  SimpleImage<double> disp_err_line;	// disparity line error
  SimpleImage<double> disp_err_samp;	// disparity sample error
	
  // SimpleImage<double> I1, I2;
	
  // User Parameters
	
  // Defaults in pdf file may be different from below:
  double delta_ref_line  = 0.1;
  double delta_ref_samp  = 0.1;
  double delta_disp_line = 0.5;
  double delta_disp_samp = 0.5;

  zvmessage("MARSERROR version 2020-03-20", "");

  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  // Get parameter overrides if any

  zvparmd("DELTA_REF_LINE" , &delta_ref_line , &count, &def, 1, 0);
  zvparmd("DELTA_REF_SAMP" , &delta_ref_samp , &count, &def, 1, 0);
  zvparmd("DELTA_DISP_LINE", &delta_disp_line, &count, &def, 1, 0);
  zvparmd("DELTA_DISP_SAMP", &delta_disp_samp, &count, &def, 1, 0);
	
  zvpcnt("DISP_ERR", &disp_err_count);
  if( disp_err_count != 0 )
    {
      delta_disp_line = 0.0;
      delta_disp_samp = 0.0;
      zvmessage("DISP_ERR file is provided - default delta_disp constants are set to 0.", "");
    }
	
  double error_params[] = {delta_ref_line, delta_ref_samp, delta_disp_line, delta_disp_samp};
  int error_params_cnt = 4;
	
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  // Get the input file list, and set up initial camera/pointing models
  // for each input.  Although we accept two and only two inputs, mars_setup
  // does lots of other nice things for us.

  mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
	     mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  /**** Read input images ****/

  // For XYZ we must do part of what mars_setup does for the INP parameter...
  // ________________________________________________________________________
  char **xyz_filenames = new char *[MAX_INPUTS];
  if (xyz_filenames == NULL)
    {
      zvmessage("Memory error in setup, xyz filename array", "");
      zabend();
    }
  mars_get_filelist("XYZ", xyz_nids, xyz_filenames, MAX_INPUTS, FALSE);

  // cout << "xyz_nids = " << xyz_nids << endl;

  for (i = 0; i < xyz_nids; i++) 
    {
      xyz_file_models[i] = PigFileModel::create(xyz_filenames[i]);
      if (xyz_file_models[i] == NULL) 
	zvnabend(150, "Unable to create file model for XYZ input %d", i);
    }
  // ________________________________________________________________________

  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

  zvnprintf(150, "Finding errors using the %s coordinate frame",
	    cs->getFrameName());

  if (nids != 2)
    zmabend("MARSERROR requires 2 and only 2 image inputs");
	
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
  // Open input file(s). Input left and right images should be
  // 2 single band files
	
  // Commented, since no reason to read left and right image values in this program.
  // open_inputs2(nids, file_models, inp_unit, inp_band, "INP");
	
  // Open input file(s).  An XYZ data could be either 1 3-band file
  // or 3 single band files
	
  //open_inputs(xyz_nids, xyz_file_models, xyz_unit, xyz_band, "XYZ", "real");
  open_inputs(xyz_nids, xyz_file_models, xyz_unit, xyz_band, "XYZ");
	
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  // Open disparity file(s).
  // DISPAR can be 1 or 2 files, for a single, 2-banded file, or 2
  // single-band files.

  PigMission *m = PigMission::getMissionObject(mission);
  zvpcnt("DISPAR", &count);
  if (count == 1)
    {
      zvp("DISPAR", filename, &count);
      zvunit(&disp_unit_line, "DISPAR", 1, "u_name", filename, NULL);
      disp_unit_samp = disp_unit_line;
      zvopen(disp_unit_line, "op", "read", "u_format", "doub",
	     "open_act", "sa", NULL);
      // nfms=nids+1;
      file_models[nids]=m->createFileModel(filename, disp_unit_line);
      zvget(disp_unit_line, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
      if (nb != 2)
	{
	  zvmessage("A single DISPAR file must have two bands", "");
	  zabend();
        }

      if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS())
        {
	  zvmessage("Size of disparity file must match first input", "");
	  zabend();
        }

      disp_band_line = 1;
      disp_band_samp = 2;
    }
  else if (count == 2)
    {
      zvpone("DISPAR", filename, 1, sizeof(filename)-1);
      zvunit(&disp_unit_line, "DISPAR", 1, "u_name", filename, NULL);
      zvopen(disp_unit_line, "op", "read", "u_format", "doub",
	     "open_act", "sa", NULL);
      // nfms=nids+2;
      file_models[nids]=m->createFileModel(filename, disp_unit_line);
      zvget(disp_unit_line, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
      if (nb != 1)
        {
	  zvmessage("A two-file DISPAR must have one band each (#1)", "");
	  zabend();
        }
      if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS())
        {
	  zvmessage("Size of disparity file #1 must match first input", "");
	  zabend();
        }
      disp_band_line = 1;

      zvpone("DISPAR", filename, 2, sizeof(filename)-1);
      zvunit(&disp_unit_samp, "DISPAR", 2, "u_name", filename, NULL);
      zvopen(disp_unit_samp, "op", "read", "u_format", "doub",
	     "open_act", "sa", NULL);
      file_models[nids+1]=m->createFileModel(filename, disp_unit_samp);
      zvget(disp_unit_samp, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
      if (nb != 1)
        {
	  zvmessage("A two-file DISPAR must have one band each (#2)", "");
	  zabend();
        }
      if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS())
        {
	  zvmessage("Size of disparity file #2 must match first input", "");
	  zabend();
        }
      disp_band_samp = 1;
    }
  else
    {
      zvmessage("DISPAR must have 1 or 2 files", "");
      zabend();
    }

  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  // Open Disparity Line/Sample Error file(s).
  // DISPAR can be 1 or 2 files, for a single, 2-banded file, or 2
  // single-band files.
	
  //zvpcnt("DISP_ERR", &count);
  if (disp_err_count == 1)
    {
      zvp("DISP_ERR", filename, &count);
      zvunit(&disp_err_line_unit, "DISP_ERR", 1, "u_name", filename, NULL);
      disp_err_samp_unit = disp_err_line_unit;
      zvopen(disp_err_line_unit, "op", "read", "u_format", "doub",
	     "open_act", "sa", NULL);
      // nfms=nids+1;
      file_models[nids]=m->createFileModel(filename, disp_err_line_unit);
      zvget(disp_err_line_unit, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
      if (nb != 2)
        {
	  zvmessage("A single DISP_ERR file must have two bands", "");
	  zabend();
        }
		
      if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS())
        {
	  zvmessage("Size of Disparity Error file must match first input", "");
	  zabend();
        }

      disp_err_line_band = 1;
      disp_err_samp_band = 2;
    }
  else if (disp_err_count == 2)
    {
      zvpone("DISP_ERR", filename, 1, sizeof(filename)-1);
      zvunit(&disp_err_line_unit, "DISP_ERR", 1, "u_name", filename, NULL);
      zvopen(disp_err_line_unit, "op", "read", "u_format", "doub",
	     "open_act", "sa", NULL);
      // nfms=nids+2;
      file_models[nids]=m->createFileModel(filename, disp_err_line_unit);
      zvget(disp_err_line_unit, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
      if (nb != 1)
        {
	  zvmessage("A two-file DISP_ERR must have one band each (#1)", "");
	  zabend();
        }
      if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS())
        {
	  zvmessage("Size of disparity error file #1 must match first input", "");
	  zabend();
        }
      disp_err_line_band = 1;
		
      zvpone("DISP_ERR", filename, 2, sizeof(filename)-1);
      zvunit(&disp_err_samp_unit, "DISP_ERR", 2, "u_name", filename, NULL);
      zvopen(disp_err_samp_unit, "op", "read", "u_format", "doub",
	     "open_act", "sa", NULL);
      file_models[nids+1]=m->createFileModel(filename, disp_err_samp_unit);
      zvget(disp_err_samp_unit, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
      if (nb != 1)
        {
	  zvmessage("A two-file DISP_ERR must have one band each (#2)", "");
	  zabend();
        }
      if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS())
        {
	  zvmessage("Size of disparity error file #2 must match first input", "");
	  zabend();
        }
      disp_err_samp_band = 1;
    }
  else if (disp_err_count == 0)
    {
      zvmessage("No DISP_ERR data was provided. Using default constants (delta_ref_line, delta_ref_samp, delta_disp_line, delta_disp_samp)", "");
    }
  else
    {
      zvmessage("DISP_ERR must have 1 or 2 files, if DISP_ERR data is provided.", "");
      zabend();
    }


  // Open output files.
  // OUT can be 1 or 3 files, for a single, 3-banded file, or 3 single-band
  // files.  ERR_FILE is an error output file.

  zvpcnt("OUT", &count);
  if (count == 1)
    {
      zvunit(&out_unit[0], "OUT", 1, NULL);
      zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
      zvopen(out_unit[0], "op", "write",
	     "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	     "u_nb", 3,
	     "open_act", "sa", "u_org", "bsq",
	     "u_format", "doub", "o_format", "real", NULL);
      zvplabel(out_unit[0], 0, 1);
      out_unit[1] = out_unit[0];
      out_unit[2] = out_unit[0];
      out_band[0] = 1;
      out_band[1] = 2;
      out_band[2] = 3;
	
      // write output label
      PigLabelModel *labelModel = NULL;
      if( m != NULL )
        {  labelModel = m->createLabelModel(out_unit[0]);  }
      // pick the coordinate system to use.
      if ( labelModel != NULL )
	labelModel->setError(xyz_file_models, xyz_nids, cs, error_params, error_params_cnt, "RANGE_ERROR_MAP");
    }
  else if (count == 3)
    {
      char* image_type[3] = {"DOWNRANGE_ERROR_MAP", "CROSSRANGE1_ERROR_MAP", "CROSSRANGE2_ERROR_MAP"};
      for (i=0; i<3; i++)
        {
	  zvunit(&out_unit[i], "OUT", i+1, NULL);
	  zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
	  zvopen(out_unit[i], "op", "write",
		 "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
		 "u_nb", 1,
		 "open_act", "sa", "u_org", "bsq",
		 "u_format", "doub", "o_format", "real", NULL);
	  zvplabel(out_unit[0], 0, 1);
	  out_band[i] = 1;

	  // write output label
	  PigLabelModel *labelModel = NULL;
	  if( m != NULL )
            {  labelModel = m->createLabelModel(out_unit[i]);  }
	  // pick the coordinate system to use.
	  if ( labelModel != NULL )
	    labelModel->setError(xyz_file_models, xyz_nids, cs, error_params, error_params_cnt, image_type[i]);
        }
    }
  else
    {
      zvmessage("OUT must have 1 or 3 filenames", "");
      zabend();
    }

  // Allocate working images

  disp_line.alloc(nl, ns);
  disp_samp.alloc(nl, ns);

  disp_err_line.alloc(nl, ns);
  disp_err_samp.alloc(nl, ns);

  range_image.alloc(nl, ns);

  // Allocate output images

  //I1.alloc(nl, ns);
  //I2.alloc(nl, ns);

  xi.alloc(nl, ns);
  yi.alloc(nl, ns);
  zi.alloc(nl, ns);

  // Allocate output error measures
  range_err_i.alloc(nl,ns);

  dri.alloc(nl,ns);
  c1i.alloc(nl,ns);
  c2i.alloc(nl,ns);
  vmi.alloc(nl,ns);
	
  xei.alloc(nl,ns);
  yei.alloc(nl,ns);
  zei.alloc(nl,ns);
  vxyzi.alloc(nl,ns);
	
  mask_image.alloc(nl,ns);
	
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
  range_err_unit = -1;
  zvp("RNG_ERR_MAGNT", filename, &count);
  if (count == 1)
    {
      zvunit(&range_err_unit, "RNG_ERR_MAGNT", 1, "u_name", filename, NULL);

      zvmessage("Before RNG_ERR_MAGNT open", "");
      zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
      zvopen(range_err_unit, "op", "write",
	     "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	     "u_nb", 1,
	     "open_act", "sa", "u_org", "bsq",
	     "u_format", "doub", "o_format", "real", NULL);
      zvmessage("After RNG_ERR_MAGNT open", "");
      zvplabel(range_err_unit, 0, 1);
		
      // write output label
      PigLabelModel *labelModel = NULL;
      if( m != NULL )
        {  labelModel = m->createLabelModel(range_err_unit);  }
      // pick the coordinate system to use.
      if ( labelModel != NULL )
	labelModel->setError(xyz_file_models, xyz_nids, cs, error_params, error_params_cnt, "RANGE_ERROR_MAGNITUDE");
    }

  xyze_unit[0] = -1;
  // zvp("XYZ_ERR", filename, &count);
  zvpcnt("XYZ_ERR", &count);
  if (count == 1)
    {
      zvp("XYZ_ERR", filename, &count);
      zvunit(&xyze_unit[0], "XYZ_ERR", 1, "u_name", filename, NULL);

      zvmessage("Before XYZ_ERR open", "");
      zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
      zvopen(xyze_unit[0], "op", "write",
	     "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	     "u_nb", 3,
	     "open_act", "sa", "u_org", "bsq",
	     "u_format", "doub", "o_format", "real", NULL);
      zvmessage("After XYZ_ERR open", "");
      zvplabel(xyze_unit[0], 0, 1);
		
      xyze_unit[1] = xyze_unit[0];
      xyze_unit[2] = xyze_unit[0];	
      out_erband[0] = 1;
      out_erband[1] = 2;
      out_erband[2] = 3;

      // write output label
      PigLabelModel *labelModel = NULL;
      if( m != NULL )
        {  labelModel = m->createLabelModel(xyze_unit[0]);  }
      // pick the coordinate system to use.
      if ( labelModel != NULL )
	labelModel->setError(xyz_file_models, xyz_nids, cs, error_params, error_params_cnt, "XYZ_ERROR_MAP");
    }
  else if (count == 3)
    {
      char* image_type[3] = {"X_ERROR_MAP", "Y_ERROR_MAP", "Z_ERROR_MAP"};
      for (i=0; i<3; i++)
        {
	  zvpone("XYZ_ERR", filename, i+1, sizeof(filename)-1);
	  zvunit(&xyze_unit[i], "XYZ_ERR", i+1, "u_name", filename, NULL);
	  zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
	  zvopen(xyze_unit[i], "op", "write",
		 "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
		 "u_nb", 1,
		 "open_act", "sa", "u_org", "bsq",
		 "u_format", "doub", "o_format", "real", NULL);
	  zvplabel(xyze_unit[0], 0, 1);
	  out_erband[i] = 1;

	  // write output label
	  PigLabelModel *labelModel = NULL;
	  if( m != NULL )
            {  labelModel = m->createLabelModel(xyze_unit[i]);  }
	  // pick the coordinate system to use.
	  if ( labelModel != NULL )
	    labelModel->setError(xyz_file_models, xyz_nids, cs, error_params, error_params_cnt, image_type[i]);
        }
    }
  else
    {
      zvmessage("XYZ_ERR must have 0, 1 or 3 filenames", "");
      zabend();
    }

  xyz_vol_unit = -1;
  zvp("XYZ_ERR_VOL", filename, &count);
  if (count == 1)
    {
      zvunit(&xyz_vol_unit, "XYZ_ERR_VOL", 1, "u_name", filename, NULL);

      zvmessage("Before XYZ_ERR_VOL open", "");
      zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
      zvopen(xyz_vol_unit, "op", "write",
	     "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	     "u_nb", 1,
	     "open_act", "sa", "u_org", "bsq",
	     "u_format", "doub", "o_format", "real", NULL);
      zvmessage("After XYZ_ERR_VOL open", "");
      zvplabel(xyz_vol_unit, 0, 1);

      // write output label
      PigLabelModel *labelModel = NULL;
      if( m != NULL )
        {  labelModel = m->createLabelModel(xyz_vol_unit);  }
      // pick the coordinate system to use.
      if ( labelModel != NULL )
	labelModel->setError(xyz_file_models, xyz_nids, cs, error_params, error_params_cnt, "XYZ_ERROR_VOLUME");		
    }

  rng_vol_unit = -1;
  zvp("RNG_ERR_VOL", filename, &count);
  if (count == 1)
    {
      zvunit(&rng_vol_unit, "RNG_ERR_VOL", 1, "u_name", filename, NULL);

      zvmessage("Before RNG_ERR_VOL open", "");
      zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
      zvopen(rng_vol_unit, "op", "write",
	     "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	     "u_nb", 1,
	     "open_act", "sa", "u_org", "bsq",
	     "u_format", "doub", "o_format", "real", NULL);
      zvmessage("After RNG_ERR_VOL open", "");
      zvplabel(rng_vol_unit, 0, 1);

      // write output label
      PigLabelModel *labelModel = NULL;
      if( m != NULL )
        {  labelModel = m->createLabelModel(rng_vol_unit);  }
      // pick the coordinate system to use.
      if ( labelModel != NULL )
	labelModel->setError(xyz_file_models, xyz_nids, cs, error_params, error_params_cnt, "RANGE_ERROR_VOLUME");
    }

  range_unit = -1;
  zvp("RANGE", filename, &count);
  if (count == 1)
    {
      zvunit(&range_unit, "RANGE", 1, "u_name", filename, NULL);

      zvmessage("Before RANGE open", "");
      zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
      zvopen(range_unit, "op", "write",
	     "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	     "u_nb", 1,
	     "open_act", "sa", "u_org", "bsq",
	     "u_format", "doub", "o_format", "real", NULL);
      zvmessage("After RANGE open", "");
      zvplabel(range_unit, 0, 1);
    }

  mask_unit = -1;
  zvp("MASK", filename, &count);
  if (count == 1)
    {
      zvunit(&mask_unit, "MASK", 1, "u_name", filename, NULL);

      zvmessage("Before MASK open", "");
      zvselpiu(xyz_unit[0]); // inherit labels from XYZ file
      zvopen(mask_unit, "op", "write",
	     "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	     "u_nb", 1,
	     "open_act", "sa", "u_org", "bsq",
	     "u_format", "doub", "o_format", "real", NULL);
      zvmessage("After MASK open", "");
      zvplabel(mask_unit, 0, 1);
    }

  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  // Read INP data

  /*    for (j=0; j < file_models[0]->getNL(); j++)		// line
	{		
        //         ***** READ a line from INP file *****
        // read the line from the two single band files

        zvread(inp_unit[0], I1.linePtr(j), "LINE", j+1, "BAND", inp_band[0], NULL);
        zvread(inp_unit[1], I2.linePtr(j), "LINE", j+1, "BAND", inp_band[1], NULL);
		
        for( i=0; i < file_models[0]->getNS(); i++ )		// samp
        {			

	// Print out some values
	//			if (count++ < 10) {
	//				snprintf(msg, msgLen "%6.2f %6.2f %6.2f %6.2f %f %f %f %f",
	//						orig_l, orig_s, disp_l, disp_s,
	//						xyz.getX(), xyz.getY(), xyz.getZ(), error_value);
	//				zvmessage(msg," ");
	//			}
			
	// #if 0	// for debug
	if( i == 300 && j == 200 )	{	cout << I1.get(j,i) << " " << I2.get(j,i) << endl;	}
	if( i == 200 && j == 300 )	{	cout << I1.get(j,i) << " " << I2.get(j,i) << endl;	}
	if( i == 512 && j == 512 )	{	cout << I1.get(j,i) << " " << I2.get(j,i) << endl;	}
	//#endif
        }
	}
  */

  // Read XYZ data and create the mask_image

  for (j=0; j < xyz_file_models[0]->getNL(); j++)		// line
    {		
      //         ***** READ a line from XYZ file *****
      //read the line from the three bands of the single input file
      //or read the line from the three single band files
        
      zvread(xyz_unit[0], xi.linePtr(j), "LINE", j+1, "BAND", xyz_band[0], NULL);
      zvread(xyz_unit[1], yi.linePtr(j), "LINE", j+1, "BAND", xyz_band[1], NULL);
      zvread(xyz_unit[2], zi.linePtr(j), "LINE", j+1, "BAND", xyz_band[2], NULL);

      for( i=0; i < xyz_file_models[0]->getNS(); i++ )		// samp
        {			
	  // 0.0 is being used as a specific flag value and is representable exactly
	  if ( xi.get(j,i) == 0.0 && yi.get(j,i) == 0.0 && zi.get(j,i) == 0.0 )          // invalid point
            {
	      mask_image.set(j,i,0.0);
            }
	  else
            {
	      mask_image.set(j,i,100.0);
            }
        }
      if (mask_unit >= 0)
	zvwrit(mask_unit, mask_image.linePtr(j), "LINE", j+1, NULL);
    }

  if (mask_unit >= 0)
    zvclose(mask_unit, NULL);

  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  // Compute line and sample centers:

  double line_ctr, sample_ctr;
  camera_in[0]->getCameraCenter(line_ctr, sample_ctr);
  zvnprintf(150, "Line   center: %lf", line_ctr);
  zvnprintf(150, "Sample center: %lf", sample_ctr);
	
	
  // Compute and print camera baseline.  Must convert to common CS
  // (to accomodate long-baseline stereo)

  PigPoint point0 = camera_in[0]->getCameraPosition();
  PigPoint point1 = camera_in[1]->getCameraPosition();
  PigCoordSystem *camera_cs0 = camera_in[0]->getCoordSystem();
  PigCoordSystem *camera_cs1 = camera_in[1]->getCoordSystem();

  PigPoint point1prime = camera_cs0->convertPoint(point1, camera_cs1);

  double baseline = (point0 - point1prime).magnitude();
  zvnprintf(150, "Camera baseline is %lf", baseline);

  // Read the correlation data.  Save it, and calculate the delta line
  // disparity.  Check to make sure it's reasonable, and save it too.

  zvmessage("Reading disparity file(s)...", "");
  for (j=0; j < nl; j++)
    {		// line

      zvread(disp_unit_line, disp_line.linePtr(j),
	     "LINE", j+1, "BAND", disp_band_line, NULL);
      zvread(disp_unit_samp, disp_samp.linePtr(j),
	     "LINE", j+1, "BAND", disp_band_samp, NULL);

      // orig_xxx = original (usually left image)
      // disp_xxx = from disparity image (usually right image)

    }
	
  zvnprintf(150, "Perturb ref line  (in pixels): %lf", delta_ref_line );
  zvnprintf(150, "Perturb ref samp  (in pixels): %lf", delta_ref_samp );
  zvnprintf(150, "Perturb disp line (in pixels): %lf", delta_disp_line);
  zvnprintf(150, "Perturb disp samp (in pixels): %lf", delta_disp_samp);
	
  // If disp_err_count > 2, the code would not reach this place.
  if (disp_err_count != 0)	// If disp_err_count == 1 or 2 (DISP_ERR image was provided).
    {
      zvmessage("Reading disparity error file(s)...", "");
      for (j=0; j < nl; j++)
        {    // line
	  zvread(disp_err_line_unit, disp_err_line.linePtr(j),
		 "LINE", j+1, "BAND", disp_err_line_band, NULL);
	  zvread(disp_err_samp_unit, disp_err_samp.linePtr(j),
		 "LINE", j+1, "BAND", disp_err_samp_band, NULL);
        }
    }
  else   // if disp_err_count = 0 (DISP_ERR image was not provided).
    {    // Use default constants delta_disp_line and delta_disp_samp.
      zvmessage("Setting default constants to disparity errors...", "");
      for (j=0; j < nl; j++)
        {
	  for (i=0; i < ns; i++)
            {
	      disp_err_line.set(j,i, delta_disp_line);
	      disp_err_samp.set(j,i, delta_disp_samp);
            }
        }
    }
		
  zvmessage("Finding Errors ...", "");

  for (j=0; j < nl; j++)
    {

      for (i=0; i < ns; i++)
        {
	  range_image.set(j,i,0.0);
		  
	  range_err_i.set(j,i,0.0);
	  dri.set(j,i,0.0);			// in case the point is 
	  c1i.set(j,i,0.0);			// outside of masked region
	  c2i.set(j,i,0.0);
	  vmi.set(j,i,0.0);
		
	  xei.set(j,i,0.0);			// in case the point is 
	  yei.set(j,i,0.0);				// outside of masked region
	  zei.set(j,i,0.0);
	  vxyzi.set(j,i,0.0);

	  if( mask_image.get(j,i) > 50 )
            {

	      // Convert points to camera coordinates

	      double orig_l = j + file_models[0]->getYOffset();
	      double orig_s = i + file_models[0]->getXOffset();

	      // disparity images are 1-based, subtract 1 to convert
	      // to 0-base for camera model
	      double disp_l = disp_line.get(j,i)-1 + file_models[1]->getYOffset();
	      double disp_s = disp_samp.get(j,i)-1 + file_models[1]->getXOffset();


	      // *** Calculate Range ***
	      // Compute a unit vector for origin point - only for 
	      // purposes of calculating range

	      PigPoint orig_origin;
	      PigVector orig_vector;

	      camera_in[0]->LStoLookVector(orig_l, orig_s,
					   orig_origin, orig_vector, cs);

	      PigPoint xyz(xi.get(j,i),yi.get(j,i),zi.get(j,i));

	      range = (xyz - orig_origin).magnitude();

	      range_image.set(j,i,range);

	      //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	      //
	      // Compute range error (range_err),
	      // down-range (dr), cross-range 1 (c1), cross-range 2 (c2) errors, 
	      // as well as volume of error ellipsoid. 
	      //
	      //if( i == 159 && j == 159 )
	      //cout << endl << "xyz(" << j << "," << i << ") = " << xi.get(j,i) << "," << yi.get(j,i) << "," << zi.get(j,i) << endl;
	      //if( i == 160 && j == 160 )
	      //cout << endl << "xyz(" << j << "," << i << ") = " << xi.get(j,i) << "," << yi.get(j,i) << "," << zi.get(j,i) << endl;
		  	
	      double range_err;
	      double dr, c1, c2, vm;
	      double xe, ye, ze, vxyz;
	      //findErrors( camera_in, cs, orig_l, orig_s, disp_l, disp_s, 
	      //			  delta_ref_line, delta_ref_samp, delta_disp_line, delta_disp_samp,
	      //			  xyz, range_err, xe, ye, ze, vxyz, dr, c1, c2, vm, orig_origin );
	      findErrors( camera_in, cs, orig_l, orig_s, disp_l, disp_s, 
			  delta_ref_line, delta_ref_samp, disp_err_line.get(j,i), disp_err_samp.get(j,i),
			  xyz, range_err, xe, ye, ze, vxyz, dr, c1, c2, vm, orig_origin );
			
			
	      if( i == 300 && j == 200 )
                {
		  cout << "disp_err_line(" << j << "," << i << ") = " << disp_err_line.get(j,i) << ", disp_err_samp(" << j << "," << i << ") = " << disp_err_samp.get(j,i) << endl;
                }
	      if( i == 200 && j == 300 )
                {
		  cout << "disp_err_line(" << j << "," << i << ") = " << disp_err_line.get(j,i) << ", disp_err_samp(" << j << "," << i << ") = " << disp_err_samp.get(j,i) << endl;
                }
	      if( i == 512 && j == 512 )
                {
		  cout << "disp_err_line(" << j << "," << i << ") = " << disp_err_line.get(j,i) << ", disp_err_samp(" << j << "," << i << ") = " << disp_err_samp.get(j,i) << endl;
                }
			
	      //snprintf(msg, msgLen, "Error volume: %lf", vm);
	      //zvmessage(msg, "");

	      //if( i == 159 && j == 159 )
	      //cout << "range_err = " << range_err << ", dr = " << dr << ", c1 = " << c1 << ", c2 = " << c2 << ", vm = " << vm << endl;
	      //if( i == 160 && j == 160 )
	      //cout << "range_err = " << range_err << ", dr = " << dr << ", c1 = " << c1 << ", c2 = " << c2 << ", vm = " << vm << endl;
		
	      // Save errors:
	      range_err_i.set(j,i, range_err);
	      dri.set(j,i, dr);
	      c1i.set(j,i, c1);
	      c2i.set(j,i, c2);
	      vmi.set(j,i, vm);
					
	      xei.set(j,i, xe);
	      yei.set(j,i, ye);
	      zei.set(j,i, ze);
	      vxyzi.set(j,i, vxyz);
			
	      if( j == int(line_ctr) && i == int(sample_ctr) )
		cout << j << " " << i << " " << range_err << endl << endl;
			
	      //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            } // end if mask_image
        }  // end for i
    }  // end for j
  disp_line.free();
  disp_samp.free();

  disp_err_line.free();
  disp_err_samp.free();



  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  // Write out range image:
  //cout << "range_unit = " << range_unit << endl;
  if (range_unit >= 0)
    {
      for (j=0; j < nl; j++)
        {
	  zvwrit(range_unit, range_image.linePtr(j), "LINE", j+1, NULL);
        }
    }
	
  // Write out range error magnitude image:
  //cout << "range_err_unit = " << range_err_unit << endl;
  if (range_err_unit >= 0)
    {
      for (j=0; j < nl; j++)
        {
	  zvwrit(range_err_unit, range_err_i.linePtr(j), "LINE", j+1, NULL);
        }
    }
		
  // Write out the x-y-z range error image:
  if (xyze_unit[0] >=0)
    {
      for (j=0; j < nl; j++)
        {
	  zvwrit(xyze_unit[0], xei.linePtr(j), "LINE",j+1, "BAND",out_erband[0],NULL);
	  zvwrit(xyze_unit[1], yei.linePtr(j), "LINE",j+1, "BAND",out_erband[1],NULL);
	  zvwrit(xyze_unit[2], zei.linePtr(j), "LINE",j+1, "BAND",out_erband[2],NULL);
        }
    }
	
  // Write out x- y- z range error ellipsoid volume image:
  //cout << "xyz_vol_unit = " << xyz_vol_unit << endl;
  if (xyz_vol_unit >= 0)
    {
      for (j=0; j < nl; j++)
        {
	  zvwrit(xyz_vol_unit, vxyzi.linePtr(j), "LINE", j+1, NULL);
        }
    }
	
  // Write out dr-c1-c2 range error ellipsoid volume image:
  //cout << "rng_vol_unit = " << rng_vol_unit << endl;
  if (rng_vol_unit >= 0)
    {
      for (j=0; j < nl; j++)
        {
	  zvwrit(rng_vol_unit, vmi.linePtr(j), "LINE", j+1, NULL);
        }
    }

  //cout << "vmi(" << 159 << "," << 159 << ") = " << vmi.get(159,159) << endl;
  //cout << "vmi(" << 160 << "," << 160 << ") = " << vmi.get(160,160) << endl;
	
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if (range_unit >= 0)
    zvclose(range_unit, NULL);
	
  if (range_err_unit >= 0)
    zvclose(range_err_unit, NULL);
			
  if (xyze_unit[0] >= 0)
    {
      zvclose(xyze_unit[0], NULL);
      if (xyze_unit[1] != xyze_unit[0])
	zvclose(xyze_unit[1], NULL);
      if (xyze_unit[2] != xyze_unit[0])
	zvclose(xyze_unit[2], NULL);
    }

  if (xyz_vol_unit >= 0)
    zvclose(xyz_vol_unit, NULL);

  if (rng_vol_unit >= 0)
    zvclose(rng_vol_unit, NULL);
	
  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
  xi.free();
  yi.free();
  zi.free();

  vmi.free();

  xei.free();
  yei.free();
  zei.free();
  vxyzi.free();

  range_image.free();
  range_err_i.free();
  mask_image.free();

  //I1.free();
  //I2.free();

  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  // Write out the down-cross1-cross2 range error image:
  for (j=0; j < nl; j++)
    {
      zvwrit(out_unit[0], dri.linePtr(j), "LINE",j+1, "BAND",out_band[0],NULL);
      zvwrit(out_unit[1], c1i.linePtr(j), "LINE",j+1, "BAND",out_band[1],NULL);
      zvwrit(out_unit[2], c2i.linePtr(j), "LINE",j+1, "BAND",out_band[2],NULL);
    }

  zvclose(out_unit[0], NULL);
  if (out_unit[1] != out_unit[0])
    zvclose(out_unit[1], NULL);
  if (out_unit[2] != out_unit[0])
    zvclose(out_unit[2], NULL);
	
  dri.free();
  c1i.free();
  c2i.free();
}

/*********************************************************************
c Convert from image coordinates to xyz coordinates given two
c images forming a stereo pair.
c cam1=x,y,z object space position of camera 1 (PigPoint)
c cam2=x,y,z object space position of camera 2 (PigPoint)
c uvw1=direction cosines for left pixel (PigVector)
c uvw2=direction cosines for right pixel (PigVector)
c xyz= xyz object space coord of object (returned PigPoint)
c return value: 0=OK, 1=no solution
***********************************************************************/

static int xvector(const PigPoint cam1, const PigPoint cam2,
		   const PigVector uvw1, const PigVector uvw2,
		   PigPoint &xyz, double *error)
{
  double a[10],b[4],c[10];
  int i, status;
  // double d[3];

  /* compute direction cosines u,v,w for ray1 and ray2 */
    
  // They're already unit vectors...
  //    uvw1.normalize();
  //    uvw2.normalize();
 
  /* solve for x,y,z point on ray1 nearest to ray2 */
  PigVector s = uvw1 * uvw2;
  PigVector s1 = s * uvw1;
  PigVector s2 = s * uvw2;

  a[1]=s.getX();
  a[2]=s1.getX();
  a[3]=s2.getX();
  a[4]=s.getY();
  a[5]=s1.getY();
  a[6]=s2.getY();
  a[7]=s.getZ();
  a[8]=s1.getZ();
  a[9]=s2.getZ();
  for (i=1; i < 10; i++)
    c[i]=a[i];
  b[1] = s % cam1;
  b[2] = s1 % cam1;
  b[3] = s2 % cam2;

  dsimq(a, b, 3, &status);
  if (status > 0)
    return status;
  PigPoint xyz1(b[1], b[2], b[3]);
 
  /* solve for xx,yy,zz point on ray2 nearest to ray1 */
  b[1] = s % cam2;
  b[2] = s1 % cam1;
  b[3] = s2 % cam2;

  dsimq(c, b, 3, &status);
  if (status > 0)
    return status;
  PigPoint xyz2(b[1], b[2], b[3]);


#if 0
  ///////////////////////////////////////////////////////////////////
  //
  // use lcross routine for comparison, added by jnm 15 April 1999

  double b1[3],b2[3],v1[3],v2[3],p1[3],p2[3];
  double k1,k2;
  int colinear;
  k1 =0;
  k2 =0;
  colinear =0;

  // pull the points out into simple arrays
  cam1.getXYZ(b1);  // base point #1
  cam2.getXYZ(b2);  // base point #2
  uvw1.getXYZ(v1);  // pointing vector #1
  uvw2.getXYZ(v2);  // pointing vector #2
 
  // cout << "Base point 1 is: " << b1[0] << "," <<b1[1] << "," << b1[2] << endl;
  // cout << "Base point 2 is: " << b2[0] << "," <<b2[1] << "," << b2[2] << endl;
  // cout << "Vector 1 is: " << v1[0] << "," <<v1[1] << "," << v1[2] << endl;
  // cout << "Vector 2 is: " << v2[0] << "," <<v2[1] << "," << v2[2] << endl;

  lcross(b1, v1, b2, v2, &k1, p1, &k2, p2, &colinear);

  // now put the points p1 and p2 into the xyz1 and xyz1 Point()s.
  
  PigPoint xyz1(p1[0], p1[1], p1[2]);
  PigPoint xyz2(p2[0], p2[1], p2[2]);

  // now back to our regularly scheduled program.
  //
  ///////////////////////////////////////////////////////////////////
#endif

  /* point inbetween is the closest approach point to both vectors */
  *error = (xyz1-xyz2).magnitude();
 
  xyz = (xyz1 + xyz2) / 2.0;

  return 0;
}

/**************************************************************************
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
*************************************************************************/

static void dsimq(double A[10], double B[10], int N, int *KS)
{
  double BIGA,SAVE,TOL;   
  int JJ,I,IT=0,J,IJ,I1,I2,IMAX=0,IQS,JY,IXJ,IX,JX,IXJX,JJX;
  int K,NY,IA,IB,IC;
  TOL=0.0;
  *KS=0;
  JJ=-N;
  for(J=1; J < N+1; J++)                 /*  DO 65 J=1,N */
    {
      JY=J+1;
      JJ=JJ+N+1;
      BIGA=0.0;
      IT=JJ-J;
      for(I=J; I < N+1; I++)                /* DO 30 I=J,N */
        {
          IJ=IT+I;
	  /* IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30 */
          if(fabs(BIGA)-fabs(A[IJ]) < 0.0){
            BIGA=A[IJ];               /* 20 */
            IMAX=I;
          }
        }                            /* 30 CONTINUE */
      if(fabs(BIGA)-TOL <= 0.0){     /* IF(dabs(BIGA)-TOL) 35,35,40 */
	*KS=1;                          /* 35 */
	return;
      }
      I1=J+N*(J-2);                   /* 40 */
      IT=IMAX-J;
      for(K=J; K < N+1; K++){         /* DO 50 K=J,N */
	I1=I1+N;
	I2=I1+IT;
	SAVE=A[I1];
	A[I1]=A[I2];
	A[I2]=SAVE;
	A[I1]=A[I1]/BIGA;                /* 50 */
      }
      SAVE=B[IMAX];
      B[IMAX]=B[J];
      B[J]=SAVE/BIGA;
      if(J-N == 0) goto seventy;           /* IF(J-N) 55,70,55 */
      IQS=N*(J-1);                      /* 55 */
      for(IX=JY; IX < N+1; IX++){     /* DO 65 IX=JY,N */
	IXJ=IQS+IX;
	IT=J-IX;
	for(JX=JY; JX < N+1; JX++){   /* DO 60 JX=JY,N */
	  IXJX=N*(JX-1)+IX;
	  JJX=IXJX+IT;
	  A[IXJX]=A[IXJX]-(A[IXJ]*A[JJX]); /* 60 */
	}
	B[IX]=B[IX]-(B[J]*A[IXJ]);          /* end of 2nd 65 */
      }
    }                                        /* end of 1st 65 */
 seventy: NY=N-1;
  IT=N*N;
  for(J=1; J < NY+1; J++){              /* DO 80 J=1,NY */
    IA=IT-J;
    IB=N-J;
    IC=N;
    for(K=1; K < J+1; K++){             /* DO 80 K=1,J */
      B[IB]=B[IB]-A[IA]*B[IC];
      IA=IA-N;
      IC=IC-1;                             /* 80 */
    }
  }
} 


/******************************************************************************
 ********************************   LCROSS   ***********************************
 *******************************************************************************

 // code by Todd Litwin, added 15 April 1999 by Justin Maki

    This function computes the nearest points, p1 and p2, on lines 1 and
    2, where each line is represented by a base point (b1 or b2) and a
    vector (v1 or v2). If input lines are colinear, then no computation
    will be made. The lines are expected to come close to intersecting
    each other. The equations of the lines are:

        L1 = B1 + k1 V1
        L2 = B2 + k2 V2

    where k1 and k2 are the parameters of the lines. */

#define EPSILON 1.0e-20

void  lcross( double b1[3], /* input base point for line 1 */
	      double v1[3], /* input pointing vector for line 1 */
	      double b2[3], /* input base point for line 2 */
	      double v2[3], /* input pointing vector for line 2 */
	      double *k1,   /* output parameter value for crossover on line 1 */
	      double p1[3], /* output crossover point on line 1 */
	      double *k2,   /* output parameter value for crossover on line 2 */
	      double p2[3], /* output crossover point on line 2 */
	      int *colinear) { /* output flag if lines are colinear */
	      
  double b[3], v1b, v2b, v1v1, v2v2, v1v2, denom;
  static double zero[3] = {0, 0, 0};

  /* Compute the intermediate terms */
  sub3(b1, b2, b);
  v1b  = dot3(v1, b);
  v2b  = dot3(v2, b);
  v1v1 = dot3(v1, v1);
  v2v2 = dot3(v2, v2);
  v1v2 = dot3(v1, v2);

  /* Test for colinearity */
  denom = v1v2 * v1v2 - v1v1 * v2v2;
  if ((denom < EPSILON) && (denom > -EPSILON)) {
    *colinear = TRUE;
    *k1 = 0;
    *k2 = 0;
    copy3(zero, p1);
    copy3(zero, p2);
    return;
  }
  else
    *colinear = FALSE;

  /* Compute the parameter values */
  *k2 = (v1b * v1v2 - v2b * v1v1) / denom;
  *k1 = (*k2 * v1v2 - v1b) / v1v1;

  /* Compute the crossover points */
  scale3(*k1, v1, p1);
  scale3(*k2, v2, p2);
  add3(b1, p1, p1);
  add3(b2, p2, p2);
}



////////////////////////////////////////////////////////////////////////
// Compute a box filter (simple average of kernel) using sliding sums.
// The output image is allocated here.  "ignore" is a value to ignore
// in the computation of the average.
////////////////////////////////////////////////////////////////////////

void compute_box_filter(SimpleImage<double> &input, SimpleImage<double> &output,
			int box_height, int box_width, double ignore)
{
  int i, j;
  // int ii, jj;

  int bh2 = box_height / 2;
  int bw2 = box_width / 2;

  int nl = input.getNL();
  int ns = input.getNS();

  output.alloc(nl, ns);

  // Compute initial box in corner

  int init_box_size = 0;
  double init_box_sum = 0.0;

  for (j=0; j < MIN(box_height, nl); j++) {
    for (i=0; i < MIN(box_width, ns); i++) {
      if (input.get(j,i) != ignore) {
	init_box_sum += input.get(j,i);
	init_box_size++;
      }
    }
  }

  for (j=0; j < nl; j++) {			// line

    // First and last half-kernels don't move; they're pinned
    // against the edge

    if (j > bh2 && j <= (nl - bh2 - 1)) {

      // Subtract the old first row and add new last row

      for (int ii = 0; ii < MIN(box_width, ns); ii++) {
	if (input.get(j-bh2-1,ii) != ignore) {
	  init_box_sum -= input.get(j-bh2-1,ii);
	  init_box_size--;
	}
	if (input.get(j+bh2,ii) != ignore) {
	  init_box_sum += input.get(j+bh2,ii);
	  init_box_size++;
	}
      }
    }

    // Sample loop.  Start with the initial box and slide it over.

    double box_sum = init_box_sum;
    int box_size = init_box_size;

    for (i=0; i < ns; i++) { 		// samp

      // First and last half-kernels don't move; they're pinned
      // against the edge

      if (i > bw2 && i <= (ns - bw2 - 1)) {

	// Subtract the old first column and add new last column

	int start_j = MAX(j-bh2, 0);
	int end_j = start_j + box_height;
	for (int jj = start_j; jj < MIN(end_j, nl); jj++) {
	  if (input.get(jj,i-bw2-1) != ignore) {
	    box_sum -= input.get(jj,i-bw2-1);
	    box_size--;
	  }
	  if (input.get(jj,i+bw2) != ignore) {
	    box_sum += input.get(jj,i+bw2);
	    box_size++;
	  }
	}
      }

      // Compute the average and save it

      if (box_size == 0)
	output.set(j,i, 0.0);
      else
	output.set(j,i, box_sum / box_size);

    }		// end samp loop
  }		// end line loop

}			// end box filter computation


////////////////////////////////////////////////////////////////////////
// Compute down-range (DR), cross-range 1 (C1), 
// and cross-range 2 (C2) errors.
////////////////////////////////////////////////////////////////////////

void findErrors( PigCameraModel **cm, PigCoordSystem *cs,
                 const double orig_l, const double orig_s, 
                 const double disp_l, const double disp_s, 
                 const double delta_ref_line,  const double delta_ref_samp,
                 const double delta_disp_line, const double delta_disp_samp,
                 const PigPoint xyz,
                 double& Em_max,
                 double&  x_out, double&  y_out, double&  z_out, double& vxyz_out,
                 double& dr_out, double& c1_out, double& c2_out, double& vm_out, 
                 const PigPoint orig_origin )
{
  int i, j;
  // char msg[150];
  int status;
	
  PigPoint point0   = cm[0]->getCameraPosition();
  /* PigVector vector0 = */ cm[0]->getCameraOrientation();
  PigPoint point1   = cm[1]->getCameraPosition();
  PigCoordSystem *camera_cs0 = cm[0]->getCoordSystem();
  PigCoordSystem *camera_cs1 = cm[1]->getCoordSystem();

  PigPoint point1prime = camera_cs0->convertPoint(point1, camera_cs1);

  PigVector B = point0 - point1prime;		// Baseline vector
  /*snprintf(msg, msgLen, "Baseline vector.x: %lf", B.getX());
    zvmessage(msg, "");
    snprintf(msg, msgLen, "Baseline vector.y: %lf", B.getY());
    zvmessage(msg, "");
    snprintf(msg, msgLen, "Baseline vector.z: %lf", B.getZ());
    zvmessage(msg, "");*/

  // double Bm = B.magnitude();
  //snprintf(msg, msgLen, "Baseline: %lf", Bm);
  //zvmessage(msg, "");

  //
  // Perturb origin
  //
  double orig_l_pert[4];
  double orig_s_pert[4];
	
  // Ealier experiments had pert_orig = 0.1, pert_disp = 0.5.
  orig_l_pert[0] = orig_l - delta_ref_line;
  orig_l_pert[1] = orig_l - delta_ref_line;
  orig_l_pert[2] = orig_l + delta_ref_line;
  orig_l_pert[3] = orig_l + delta_ref_line;
	
  orig_s_pert[0] = orig_s - delta_ref_samp;
  orig_s_pert[1] = orig_s + delta_ref_samp;
  orig_s_pert[2] = orig_s - delta_ref_samp;
  orig_s_pert[3] = orig_s + delta_ref_samp;

  PigPoint orig_origin_pert;
  PigVector orig_vector_pert;

  //
  // Perturb disparity
  //
  double disp_l_pert[4];
  double disp_s_pert[4];
	
  disp_l_pert[0] = disp_l - delta_disp_line;
  disp_l_pert[1] = disp_l - delta_disp_line;
  disp_l_pert[2] = disp_l + delta_disp_line;
  disp_l_pert[3] = disp_l + delta_disp_line;
	
  disp_s_pert[0] = disp_s - delta_disp_samp;
  disp_s_pert[1] = disp_s + delta_disp_samp;
  disp_s_pert[2] = disp_s - delta_disp_samp;
  disp_s_pert[3] = disp_s + delta_disp_samp;
	
  PigPoint  disp_origin_pert;
  PigVector disp_vector_pert;
  double error_value_pert;
	
  //
  // Find perturbed xyz coordinate and calculate
  // down-range and cross-range errors
  //
  PigPoint xyz_pert;
		
  double dr, c1, c2;
  double   xmax = -1e10,   ymax = -1e10,   zmax = -1e10;
  double   xmin =  1e10,   ymin =  1e10,   zmin =  1e10;
  double dr_max = -1e10, c1_max = -1e10, c2_max = -1e10;
  double dr_min =  1e10, c1_min =  1e10, c2_min =  1e10;
	
  Em_max = -1e10;     // range error scalar
	
  for( j=0; j<4; j++)
    {
      //cout << "****************************************************************************" << endl;
      for( i=0; i<4; i++ )
        {

	  //cout << "(j,i) = (" << j << ", " << i << ")" << endl; 

	  // Given orig coordinates (orig_l, orig_s), get orig_origin and look direction.
	  // Given disp coordinates (disp_l, disp_s), get disp_origin and look direction.

	  // Given orig_origin & look direction, disp_origin & look direction, get xyz.


	  // Compute unit vector for perturbed origin
	  cm[0]->LStoLookVector(orig_l_pert[j], orig_s_pert[j], orig_origin_pert, orig_vector_pert, cs);

	  // Compute unit vector for perturbed disparity
	  cm[1]->LStoLookVector(disp_l_pert[i], disp_s_pert[i], disp_origin_pert, disp_vector_pert, cs);

	  // Compute x,y,z for perturbed disparity vector.

	  status = xvector(orig_origin_pert, disp_origin_pert, orig_vector_pert, disp_vector_pert, xyz_pert, &error_value_pert);
	  if (status != 0) {
	    // reject_nosolution++;   What should we do about this?
	    continue;		// error
	  }

	  PigVector R = xyz - orig_origin;	// Range vector

	  // Compute errors in XYZ due to disparity perturbation:
	  PigVector E = xyz_pert - xyz;		// (range) Error vector

	  double Rm = R.magnitude();
	  double Em = E.magnitude();				// range error scalar

	  Em_max = MAX(Em_max, Em);

	  //
	  // Error dimensions of error box in XYZ coordinate frame:
	  //
	  xmax = MAX(xmax, E.getX());
	  ymax = MAX(ymax, E.getY());
	  zmax = MAX(zmax, E.getZ());
			
	  xmin = MIN(xmin, E.getX());
	  ymin = MIN(ymin, E.getY());
	  zmin = MIN(zmin, E.getZ());

	  //
	  // Finding error in DR-C1-C2 axes:
	  //
	  double theta_R_E = acos( R%E / (Rm*Em) );		// angle between R and E

	  PigVector C2 = R*B;					// perpendicular to R and B
	  PigVector C1 = R*C2;				// perpendicular to R and C2
			
	  double C1m = C1.magnitude();
	  double C2m = C2.magnitude();
			
	  double theta_C1_E = acos(C1%E/(C1m*Em));
	  double theta_C2_E = acos(C2%E/(C2m*Em));

	  dr = Em*cos(theta_R_E);				// down-range error (proj of E on R)
	  c1 = Em*cos(theta_C1_E);			// cross-range 1 error (proj of e on C1)
	  c2 = Em*cos(theta_C2_E);			// cross-range 2 error (proj of e on C2)
			
	  //
	  // OUTPUT TEST:
	  //
	  /*cout << endl << "R  = " <<  R.getX() << ", " <<  R.getY() << ", " <<  R.getZ() << ",  ||R|| = " << Rm;
            cout << endl << "E  = " <<  E.getX() << ", " <<  E.getY() << ", " <<  E.getZ() << ",  ||E|| = " << Em << endl;

            cout << endl << "C1 = " << C1.getX() << ", " << C1.getY() << ", " << C1.getZ() << ",  ||C1|| = " << C1.magnitude();
            cout << endl << "C2 = " << C2.getX() << ", " << C2.getY() << ", " << C2.getZ() << ",  ||C2|| = " << C2.magnitude() << endl << endl;

            cout << "Should be 0.  C1_dot_R  = " << C1%R  << endl;
            cout << "Should be 0.  R_dot_C2  = " << R%C2  << endl;
            cout << "Should be 0.  C1_dot_C2 = " << C1%C2 << endl;


            cout << "theta_R_E = "  << theta_R_E  << "  In degrees: " << PigRad2Deg(theta_R_E)  << endl;
            cout << "theta_C1_E = " << theta_C1_E << "  In degrees: " << PigRad2Deg(theta_C1_E) << endl;
            cout << "theta_C2_E = " << theta_C2_E << "  In degrees: " << PigRad2Deg(theta_C2_E) << endl;

            cout << endl << "dr = " << dr << ", c1 = " << c1 << ", c2 = " << c2 << endl; 
            cout << "Magnitude [dr c1 c2] vector = " << sqrt(dr*dr + c1*c1 + c2*c2) << endl;
	  */
			
	  //
	  // Find the dimensions of the error box in DR-C1-C2 coordinate frame:
	  //
	  dr_max = (dr_max > dr) ? dr_max : dr;
	  c1_max = (c1_max > c1) ? c1_max : c1;
	  c2_max = (c2_max > c2) ? c2_max : c2;

	  dr_min = (dr_min < dr) ? dr_min : dr;
	  c1_min = (c1_min < c1) ? c1_min : c1;
	  c2_min = (c2_min < c2) ? c2_min : c2;
        }
    }
	
  // X,Y,Z errors for output:
  x_out = MAX(fabs(xmax),fabs(xmin));
  y_out = MAX(fabs(ymax),fabs(ymin));
  z_out = MAX(fabs(zmax),fabs(zmin));

  // Define the proper X,Y,Z boundaries of error cube to compute error ellipsoid volume:

  xmax = MAX(xmax,0);
  ymax = MAX(ymax,0);
  zmax = MAX(zmax,0);

  xmin = MIN(xmin,0);
  ymin = MIN(ymin,0);
  zmin = MIN(zmin,0);

  vxyz_out = (4.0/24.0)*(xmax-xmin)*(ymax-ymin)*(zmax-zmin);

  // Down-range, cross-range 1, and cross-range 2 errors for output:
  dr_out = (fabs(dr_max) > fabs(dr_min)) ? fabs(dr_max) : fabs(dr_min);
  c1_out = (fabs(c1_max) > fabs(c1_min)) ? fabs(c1_max) : fabs(c1_min);
  c2_out = (fabs(c2_max) > fabs(c2_min)) ? fabs(c2_max) : fabs(c2_min);

  // Define the proper boundaries of error cube to compute error ellipsoid volume:
  dr_max = MAX(dr_max,0);
  c1_max = MAX(c1_max,0);
  c2_max = MAX(c2_max,0);

  dr_min = MIN(dr_min,0);
  c1_min = MIN(c1_min,0);
  c2_min = MIN(c2_min,0);

  vm_out = (4.0/24.0)*(dr_max-dr_min)*(c1_max-c1_min)*(c2_max-c2_min);


  //cout << "****************************************************************************" << endl;

  //cout << endl << "dr_out = " << dr_out << ", c1_out = " << c1_out << ", c2_out = " << c2_out << endl; 

  //cout << "****************************************************************************" << endl;
  //cout << "****************************************************************************" << endl;
  //cout << "****************************************************************************" << endl;
}


////////////////////////////////////////////////////////////////////////
// Compute down-range (DR), cross-range 1 (C1), 
// and cross-range 2 (C2) errors 
// This version uses quaternions, and needs some work.
////////////////////////////////////////////////////////////////////////

void findErrors_Quaternions( PigCameraModel **cm, PigCoordSystem *cs,
			     const double orig_l, const double orig_s, 
			     const double disp_l, const double disp_s, 
			     const double pert_orig, const double pert_disp,
			     const PigPoint xyz,
			     double& range_err,
			     double& dr, double& c1, double& c2, double& vm, const PigPoint orig_origin )
{
  int i, j;
  // char msg[150];
  int status;
	
  /* PigPoint point0 = */ cm[0]->getCameraPosition();
  PigPoint point1 = cm[1]->getCameraPosition();
  PigCoordSystem *camera_cs0 = cm[0]->getCoordSystem();
  PigCoordSystem *camera_cs1 = cm[1]->getCoordSystem();
	
  /* PigPoint point1prime = */ camera_cs0->convertPoint(point1, camera_cs1);
	
  // PigVector baseline_vector = point0 - point1prime;
  /*snprintf(msg, msgLen, "Baseline vector.x: %lf", baseline_vector.getX());
    zvmessage(msg, "");
    snprintf(msg, msgLen, "Baseline vector.y: %lf", baseline_vector.getY());
    zvmessage(msg, "");
    snprintf(msg, msgLen, "Baseline vector.z: %lf", baseline_vector.getZ());
    zvmessage(msg, ""); */
	
  //double baseline = baseline_vector.magnitude();
  /*snprintf(msg, msgLen, "Baseline: %lf", baseline);
    zvmessage(msg, ""); */
	
  // Axis vectors
  PigVector  xaxis(1, 0,0);
  PigVector  yaxis(0, 1,0);
  PigVector nyaxis(0,-1,0);
  PigVector  zaxis(0, 0,1);
	
  //
  // Perturb origin
  //
  double orig_l_pert[4];
  double orig_s_pert[4];
	
  // Ealier experiments had pert_orig = 0.1, pert_disp = 0.5.
  orig_l_pert[0] = orig_l - pert_orig;
  orig_l_pert[1] = orig_l - pert_orig;
  orig_l_pert[2] = orig_l + pert_orig;
  orig_l_pert[3] = orig_l + pert_orig;
	
  orig_s_pert[0] = orig_s - pert_orig;
  orig_s_pert[1] = orig_s + pert_orig;
  orig_s_pert[2] = orig_s - pert_orig;
  orig_s_pert[3] = orig_s + pert_orig;
	
  PigPoint orig_origin_pert;
  PigVector orig_vector_pert;
	
  //
  // Perturb disparity
  //
  double disp_l_pert[4];
  double disp_s_pert[4];
	
  disp_l_pert[0] = disp_l - pert_disp;
  disp_l_pert[1] = disp_l - pert_disp;
  disp_l_pert[2] = disp_l + pert_disp;
  disp_l_pert[3] = disp_l + pert_disp;
	
  disp_s_pert[0] = disp_s - pert_disp;
  disp_s_pert[1] = disp_s + pert_disp;
  disp_s_pert[2] = disp_s - pert_disp;
  disp_s_pert[3] = disp_s + pert_disp;
	
  PigPoint  disp_origin_pert;
  PigVector disp_vector_pert;
  double error_value_pert;
	
  //
  // Find perturbed xyz coordinate and calculate
  // down-range and cross-range errors
  //
  PigPoint xyz_pert;
	
  //double C1, C2;	// cross-range axis
	
  double az, el;
	
  //double dR;
	
  PigQuaternion quat1, quat2, quat;
  PigVector error_xyz_DrCrCr;
  double range_err_max = -1e10;
  double xx,yy,zz;
  double xmax = -1e10, ymax = -1e10, zmax = -1e10;
  double xmin =  1e10, ymin =  1e10, zmin =  1e10;
	
  for( j=0; j<4; j++)
    {
      cout << "****************************************************************************" << endl;
      for( i=0; i<4; i++ )
	{
			
	  cout << "(j,i) = (" << j << ", " << i << ")" << endl; 
			
	  // Given orig coordinates (orig_l, orig_s), get orig_origin and look direction.
	  // Given disp coordinates (disp_l, disp_s), get disp_origin and look direction.
			
	  // Given orig_origin & look direction, disp_origin & look direction, get xyz.

			
	  // Compute unit vector for perturbed origin
	  cm[0]->LStoLookVector(orig_l_pert[j], orig_s_pert[j], orig_origin_pert, orig_vector_pert, cs);
			
	  // Compute unit vector for perturbed disparity
	  cm[1]->LStoLookVector(disp_l_pert[i], disp_s_pert[i], disp_origin_pert, disp_vector_pert, cs);
			
	  // Compute x,y,z for perturbed disparity vector.
			
	  status = xvector(orig_origin_pert, disp_origin_pert, orig_vector_pert, disp_vector_pert, xyz_pert, &error_value_pert);
	  if (status != 0) {
	    // reject_nosolution++;   What should we do about this?
	    continue;		// error
	  }
			
	  // Compute errors in XYZ due to disparity perturbation:
	  PigVector error_xyz = xyz_pert - xyz;	// (range) error vector
			
	  range_err = error_xyz.magnitude();		// range error scalar
	  range_err_max = MAX(range_err_max, range_err);
			
	  //		    C1 = range*baseline_vector; // cross-range 1 error axes C1 = cross product RxB
	  //		    C1.normalize();
			
	  //		    C2 = range*C1;              // cross-range 2 error axes C2 = cross product RxC1
	  //		    C2.normalize();
			
	  // Azimuth and elevation of original (unperturbed xyz)
	  az = xyz.getAz();   // angle between x-axis and projection of R onto xy-plane
	  el = xyz.getEl();   // angle between xy-plane and R
			
	  // Quaternions:
	  // (u,theta) constructs a rotation
	  // theta about the axis u (a Vector).  (s, v) constructs a quaternion with
	  // the explicit values (s, v[0], v[1], v[2]) (v is an array[3]).
			
			
	  // In order to align x-axis with range vector R perform the following.
	  // Rotate around NEGATIVE y-axis by elevation angle.
	  quat1.setRotation(nyaxis,el);
			
	  // Rotate around z-axis by azimuth angle:
	  quat2.setRotation( zaxis,az);
			
	  // Combined quaternion:
	  quat = quat2*quat1;
			
	  PigVector test = quat*(xyz - orig_origin);  // should align with x-axis
			
			
	  // error_xyz in DR-C1-C2 coordinate frame:
	  error_xyz_DrCrCr = quat*error_xyz;
	  error_xyz_DrCrCr.getXYZ(xx,yy,zz);
			
	  cout << endl << "range vec = " << (xyz - orig_origin).getX() << ", " << (xyz - orig_origin).getY() << ", " << (xyz - orig_origin).getZ();
	  cout << endl << "error_xyz = (" << error_xyz.getX() << ", " << error_xyz.getY() << ", " << error_xyz.getZ() << ")";
	  cout << endl << "||error_xyz|| = " << range_err;
	  cout << endl << "error_xyz_DrCrCr = (" << xx << ", " << yy << ", " << zz << ")";
	  cout << endl << "||error_xyz_DrCrCr|| = " << error_xyz_DrCrCr.magnitude();
	  cout << endl << "test = " << test.getX() << ", " << test.getY() << ", " << test.getZ() << endl;

			
	  // Find the dimensions of the error box in DR-C1-C2 coordinate frame:
	  xmax = (xmax > xx) ? xmax : xx;
	  ymax = (ymax > yy) ? ymax : yy;
	  zmax = (zmax > zz) ? zmax : zz;
			
	  xmin = (xmin < xx) ? xmin : xx;
	  ymin = (ymin < yy) ? ymin : yy;
	  zmin = (zmin < zz) ? zmin : zz;
	}
    }
	
  // Down-range, cross-range 1, and cross-range 2 errors:
  dr = (fabs(xmax) > fabs(xmin)) ? fabs(xmax) : fabs(xmin);
  c1 = (fabs(ymax) > fabs(ymin)) ? fabs(ymax) : fabs(ymin);
  c2 = (fabs(zmax) > fabs(zmin)) ? fabs(zmax) : fabs(zmin);
	
  vm = (4.0/24.0)*(xmax-xmin)*(ymax-ymin)*(zmax-zmin); 
	
  cout << "****************************************************************************" << endl;

  cout << endl << "dr = " << dr << ", c1 = " << c1 << ", c2 = " << c2 << endl; 
	
  cout << "****************************************************************************" << endl;
  cout << "****************************************************************************" << endl;
  cout << "****************************************************************************" << endl;

	
}

////////////////////////////////////////////////////////////////////////
// Tests findErrors(...) function to make sure elevation, azimuth,
// quaternion rotations are used correctly to align x-axis with range
// vector.
////////////////////////////////////////////////////////////////////////

void test_findErrors( PigCameraModel **cm, PigCoordSystem *cs,
		      const double orig_l, const double orig_s, 
		      const double disp_l, const double disp_s, const PigPoint xyz, 
		      double& dr, double& c1, double& c2, double& vm )
{
  // Axis vectors
  PigVector  xaxis(1, 0,0);
  PigVector  yaxis(0, 1,0);
  PigVector nyaxis(0,-1,0);
  PigVector  zaxis(0, 0,1);
	
  double az, el;
	
  PigQuaternion quat1, quat2, quat;
  double xx,yy,zz;

  cout << endl << " *** TEST *** " << endl;
  cout << "Rotations will align x-axis with R" << endl;
  cout << "For example: " << endl;
  cout << "  If R = (1,1,1), then a = (1,0,0) -> a_n = ( 0.57735, 0.57735, 0.57735) <- located on R" << endl;
  cout << "  If R = (1,1,1), then a = (0,1,0) -> a_n = (-0.70711, 0.70711, 0      )" << endl;
  cout << "  If R = (1,1,1), then a = (0,0,1) -> a_n = (-0.40825,-0.40825, 0.81650)" << endl;
  cout << "  These three are orthogonal to each other (dot products = 0) " << endl;
			
  cout << "  If R = (1,1,1), then a = (1,1,1) -> a_n = (-0.53801, 0.87621, 1.39385)" << endl << endl;
			
  cout << "  If R = (1,2,3), then a = (1,0,0) -> a_n = ( 0.26726, 0.53452, 0.80178) <- located on R" << endl;
  cout << "  If R = (1,2,3), then a = (0,1,0) -> a_n = (-0.89443, 0.44721, 0      )" << endl;
  cout << "  If R = (1,2,3), then a = (0,0,1) -> a_n = (-0.35857,-0.71714, 0.59761)" << endl;
  cout << "  These three are orthogonal to each other (dot products = 0) " << endl;
  cout << "  If R = (1,2,3), then a = (1,2,3) -> a_n = (-2.5973, -0.72246, 2.59463)" << endl << endl;
			
			
  cout << "Choose Range vector R = (r_x, r_y, r_z):";
  cout << endl << "r_x = ";
  cin >> xx;
  cout << endl << "r_y = ";
  cin >> yy;
  cout << endl << "r_z = ";
  cin >> zz;
			
  PigVector r;	// range vector
  r.setXYZ(xx,yy,zz);
			
  r.getXYZ(xx,yy,zz);
  cout << "Range vector in old coordinate frame: " << endl;
  cout << "R = (" << xx << ", " << yy << ", " << zz << ")" << endl;
			
  az = r.getAz();   // angle between x-axis and projection of R onto xy-plane
  el = r.getEl();   // angle between xy-plane and R
			
  cout << "r_az = " << az << ", r_el = " << el << endl << endl;
			
  // Calculate quaternion for rotations: aligning x-axis with R
  quat1.setRotation(nyaxis,el);
  quat2.setRotation( zaxis,az);
  quat = quat2*quat1;
			
  cout << "Choose vector a = (a_x,a_y,a_z) to be transformed to a new coordinate system." << endl;
  cout << "(a will be transformed using the same transformation that transforms x-axis to R)";
  cout << endl << "a_x = ";
  cin >> xx;
  cout << endl << "a_y = ";
  cin >> yy;
  cout << endl << "a_z = ";
  cin >> zz;
			
  PigVector a;
  a.setXYZ(xx,yy,zz);
			
  a.getXYZ(xx,yy,zz);
  cout << "Vector a in old coordinate frame: " << endl;
  cout << "a = (" << xx << ", " << yy << ", " << zz << ")" << endl;
			
  //*****************************************************************************
			
  PigVector an = quat*a;
  an.getXYZ(xx,yy,zz);
	
  //*****************************************************************************
  // TEST:
  cout << "Vector a in new coordinate frame: " << endl;
  cout << "a_n = (" << xx << ", " << yy << ", " << zz << ")" << endl;
  //*****************************************************************************
	
  exit(1);
	
}

////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3], int band[3], char *type )
{
  int i;
	
  if (nids == 1)
    {
      // FILE is OPEN with U_FORMAT of xxx to match our buffer.

      // get Unit id
      unit[0] = file_models[0]->getUnit();

      if (file_models[0]->isFileOpen())
	file_models[0]->closeFile();
      zvopen(unit[0], "op", "read", "open_act", "sa", "io_act", "sa", "u_format", "doub", NULL);
      file_models[0]->setFileOpen(TRUE);


      if (file_models[0]->getNB() != 3)
	zvnabend(256, "A single %s file must have three bands", type);

      // Initialize xyz_unit array
      unit[2] = unit[1] = unit[0];

      // Initialize band array
      band[0] = 1;
      band[1] = 2;
      band[2] = 3;
    }
  else if (nids == 3)
    {
      for (i = 0; i < 3; i++)
        {
	  // make sure that file is open
	  if (file_models[i]->isFileOpen())
	    file_models[i]->closeFile();

	  // get Unit id
	  unit[i] = file_models[i]->getUnit();

	  zvopen(unit[i], "op", "read", "open_act", "sa", "io_act", "sa", "u_format", "doub", NULL);
	  file_models[i]->setFileOpen(TRUE);

	  if (file_models[i]->getNB() != 1)
	    zvnabend(256, "A three-file %s must have one band each", type);

	  // check that all files are the same size
	  if ((file_models[i]->getNL() != file_models[0]->getNL()) || (file_models[i]->getNS() != file_models[0]->getNS()))
	    zmabend("Input is of different size than Input #1");
	  band[i] = 1;
        }
    }
  else
    zvnabend(256, "open_inputs requires either 1 3-band file or 3 single band files as input for %s", type);

}

///////////////////////////////////////////////////////////////////////////////////

void open_inputs2(int nids, PigFileModel *file_models[], int unit[2], int band[2], char *type )
{
  int i;
	
  if (nids == 2)
    {
      for (i = 0; i < 2; i++)
        {
	  // make sure that file is open
	  if (file_models[i]->isFileOpen())
	    file_models[i]->closeFile();

	  // get Unit id
	  unit[i] = file_models[i]->getUnit();

	  zvopen(unit[i], "op", "read", "open_act", "sa", "io_act", "sa", "u_format", "doub", NULL);
	  file_models[i]->setFileOpen(TRUE);

	  if (file_models[i]->getNB() != 1)
	    zvnabend(256, "A two-file %s must have one band each", type);

	  // check that all files are the same size
	  if ((file_models[i]->getNL() != file_models[0]->getNL()) || (file_models[i]->getNS() != file_models[0]->getNS()))
	    zmabend("Input is of different size than Input #1");
	  band[i] = 1;
        }
    }
  else
    zvnabend(256, "open_inputs2 requires 2 single band files as input for %s", type);
}

///////////////////////////////////////////////////////////////////////////////////
