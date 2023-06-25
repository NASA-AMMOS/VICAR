/* patch */

/* Casey Handmer 2020*/
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
#include "mat3.h"
#include "xyz_to_uvw.h"

#include "return_status.h"

#include <math.h>
#include <array>
#include <vector>
#include <iostream>
using namespace std;

#ifdef _OPENMP
#include <omp.h>
#endif

/* buffer sizes in main program */
#define MAX_INPUTS 4
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

typedef SimpleImage<float> SIf;

/*
How does the algorithm work?
1) Load the stereo range, and the image, and geometrical info.
1.1) Generate xyz from the range.
2) Filter xyz for poor data, or use marsrfilt to smooth it out
3) Optionally crop the image
4) Produce the  mask
5) Patch the patches using fitting and other guesswork
6) Add noise to the initial patch
7) Generate normal vectors (this function is called a lot)
8) Segment the image into sand, rocks, and sky
9) Initialize energy function parameters and history
10) Generate range perturbations (snow or cross hatch?)
11) Iterate initial patch by performing energy gradient descent
12) Track and print progress
13) Return optimized patch

 */

// Low level functions

// takes an inverse without exploding on zeros
static void safe_inverse(SIf *arr_in, SIf *arr_out);
static void safe_divide(SIf *num_in, SIf *den_in, SIf *quot_out);

// May have to add vector/array functions in here if there's no decent library available
static void arr_add(SIf *A, SIf *B, SIf *C);// A+B=C
static void arr_mult(SIf *A, SIf *B, SIf *C);// A*B=C
static double arr_mean(SIf *A); // mean array value
static void norm_arr(SIf *A); // normalize a 3D SIf representing a field of unit vectors
static void sign(SIf *A, SIf *sign); // Perform an array-based sign test
static double min(SIf *A); // return the minimum element of an array.


// This feels very hacky. Why doesn't VICAR have powerful array and vector types?

// Helper functions

// Will do a sign test on the input image to calculate where stereo matching has not provided a solution.
static void generate_mask(SIf *rng, SIf *mask);

// xyz to rng
static void xyz2rng(SIf *xyz_in, PigPoint rangeOrigin, SIf *rng_out); // May need other inputs

// rng to xyz
static void rng2xyz(SIf *rng_in, PigFileModel *file_models0,
                    PigCameraModel *camera_in0, PigCoordSystem *cs,
                    PigPoint rangeOrigin, SIf *xyz_out);


// Wrapper of some external function
static void generate_uvw(SIf *xyz_in, PigPoint rangeOrigin, SIf *uvw_out);

// Adds random noise to range data
static void add_noise_to_range(SIf *rng_in, SIf *rng_out, double noise);

// Find a library which can do this, hopefully
static void gaussian_filter(SIf *arr_in, int window_vert, int window_hor, SIf *arr_out);

// Calculate the determinant of a 3x3 matrix
static double determinant(SIf *x);

// Find a library
// Performs 2D linear regression of array f over x and y, excluding empty data, then fills it in.
static void linear_regression(SIf *x, SIf *y, SIf *f, SIf *f_out);

/////////////////////////
// Algorithm level functions
/////////////////////////

// Generate initial condition
// generate patch helper
static void gaussian_initial_patch(SIf *rng, SIf *mask, int window_vert, int window_hor, SIf *rng0);

// Performs linear regression of inverse range, blending using a Gaussian kernel
static void generate_initial_patch(SIf *rng, SIf *mask, int window_vert, int window_hor, SIf *rng0);

// Segment image into sand, rocks, sky
// Attempts to generate a "flat" image with the true underlying albedo of each feature. 
// This can be done by binning or wavelets.
// This can stay internal while we only care about sand, but in general should be its own function.
static void intrinsic_albedo(SIf *img, SIf *mask, SIf *albedo);

// Define energy function
// Radiance helper
static void radiance_uvw(SIf *uvw, SIf *albedo, PigVector *s0, SIf *radiance);
// Needs s0 (illumination direction) and camera model info
// A better coder would find some way to throw Unity in here.

// Radiance error 
static void radiance_error(double beta, SIf *radiance, SIf *img, SIf *rad_err);

// norm error
static void norm_error(SIf *uvw_free, SIf *uvw_from_rng, SIf *norm_err);

// mask error
static void mask_error(SIf *rng, SIf *rng_init, SIf *mask, double p, SIf *mask_err);

// baseline error
static void baseline_error(SIf *xyz, SIf *bl_err);

// Overall error
static void energy(double beta, SIf *albedo, SIf *img, PigVector *s0, 
		   SIf *uvw_free, SIf *rng, 
		   SIf *rng_init, SIf *mask, 
		   array<double, 4> pars, double p, 
		   PigFileModel *file_models0, PigCameraModel *camera_in0, PigCoordSystem *cs, PigPoint rangeOrigin,
		   SIf *energy_err);




////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j;
    int status, count, def;
    const size_t msgLen = 255;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];
    int nl, ns, nb;
    int band[3];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs;
    int xyz_unit[3];
    int img_unit[1];
    double point[3];

    // Outputs
    int unit_out;
    int nlo, nso;

    // User Parameters
    PigPoint rangeOrigin;

    zvmessage("PATCH version 1.0", "");
    

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept two or four inputs only, mars_setup
    // does lots of other nice things for us.
    // Inputs are xyz, img, or x, y, z, img.

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
    //ref.toStringAppend(msg);
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Generating RANGE using the %s coordinate frame.",
            cs->getFrameName());
    zvmessage(msg, "");

    // Get the range origin point to use
    zvpcnt("origin", &count);
    if (count == 3) {
        zvp("origin", point, &count);
        rangeOrigin.setXYZ(point);
    }
    else {
        if (pointing_in[0] == NULL) {
            zvmessage("No camera model to extract origin; use ORIGIN parameter", "");
            zabend();
        }
        //Use camera position as a default range origin value
        rangeOrigin = pointing_in[0]->getCameraPosition(cs);
    }

    snprintf(msg, msgLen, "Using POINT (%f, %f, %f) as range origin",
            rangeOrigin.getX(), rangeOrigin.getY(),rangeOrigin.getZ());
    zvmessage(msg, "");

    // Open input file(s).  An XYZ data is 1 3-band file
    // 3 single band files obsolete and commented out
    if (nids == 2) {

        //make sure that file is not open
      if(file_models[0]->isFileOpen()) {
	file_models[0]->closeFile();
      }

      if(file_models[1]->isFileOpen()) {
	file_models[1]->closeFile();
      }

      //open the file
      file_models[0]->openFile();
      file_models[0]->setFileOpen(TRUE);

      file_models[1]->openFile();
      file_models[1]->setFileOpen(TRUE);

      //get Unit id
      xyz_unit[0] = file_models[0]->getUnit();
      
      //check for proper number of bands
      zvget(xyz_unit[0], "nl", &nl, "ns", &ns, "nb", &nb, NULL);
      if (nb != 3) {
	zvmessage("A single XYZ file must have three bands", "");
	zabend();
      }

      //Initialize xyz_unit array
      xyz_unit[2] = xyz_unit[1] = xyz_unit[0];

      //Initialize band array
      band[0] = 1;
      band[1] = 2;
      band[2] = 3;

      //Initialize img_unit id
      img_unit[0] = file_models[1]->getUnit();
      
      //snprintf(msg, msgLen, "file_models[0]: %s , file_models[1]: %s", file_models[0], file_models[1]);
      //zvmessage(msg,"");
    }
    /*
    else if(nids == 4) { // This is obsolete
      int cnt;
      for(cnt = 0; cnt <3; cnt++) {

        //make sure that file is not open
        if(file_models[cnt]->isFileOpen())
          file_models[cnt]->closeFile();

        //open the file
        file_models[cnt]->openFile();
        file_models[cnt]->setFileOpen(TRUE);

        //get Unit id
        xyz_unit[cnt] = file_models[cnt]->getUnit();

        //check for proper number of bands
        zvget(xyz_unit[cnt], "nl", &nl, "ns", &ns, "nb", &nb, NULL);
        if (nb != 1) {
            zvmessage("A three-file XYZ must have one band each (#1)", "");
            zabend();
        }

        //check that all files are the same size
        if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS()) {
            zvmessage("Input is of different size than Input #1", "");
            zabend();
        }
        band[cnt] = 1;
      }
      img_unit[0] = file_models[3]->getUnit();

      }*/
    else {
        zvmessage("PATCH requires a 3-band file (3 single band files not supported) for xyz and 1 1-band file for radiance", "");
        zabend();
    }

    // Open output files.
    // OUT should be 1 file
    zvpcnt("OUT", &count);
    if (count != 1) {
        zvmessage("OUT must have 1 filename", "");
        zabend();
    }


    count = 0;

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();
    snprintf(msg, msgLen, "Output # lines & samples=%10d %10d", nlo, nso);
    zvmessage(msg, "");

    zvselpiu(file_models[0]->getUnit());          // transfer labels
    status=zvunit(&unit_out, "OUT", 1, NULL);
    status=zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL",
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", 3, "U_ORG", "BSQ", "O_FORMAT", "REAL", NULL);

    // Write the parameter output labels

    zvplabel(unit_out, 0, 1);

    // Read the xyz data and compute range, converting CS's if needed as we go

    SimpleImage<float> *xyz = new SimpleImage<float>(3, nlo, nso);
    SimpleImage<float> *xyz_out = new SimpleImage<float>(3, nlo, nso);
    SimpleImage<float> *rng = new SimpleImage<float>(1, nlo, nso);

    SIf *img = new SIf(1, nlo, nso);
    SimpleImage<short int> *img_int = new SimpleImage<short int>(1, nlo, nso);

    SimpleImage<float> *cast = NULL;
    if (zvptst("RAY")) {
	cast = new SimpleImage<float>(3, nlo, nso);
	cast->zero();
	zvmessage("Using original ray for reconstruction", "");
    }
    else {
	zvmessage("Using camera model for reconstruction", "");
    }

    for (j=0; j < nlo; j++) {               // line

        //read the line from the three bands of the single input file
        //or read the line from the three single band files
        zvread(xyz_unit[0], xyz->linePtr(0,j), "LINE", j+1,
							"BAND", band[0], NULL);
        zvread(xyz_unit[1], xyz->linePtr(1,j), "LINE", j+1,
							"BAND", band[1], NULL);
        zvread(xyz_unit[2], xyz->linePtr(2,j), "LINE", j+1,
							"BAND", band[2], NULL);

	zvread(img_unit[0], img_int->linePtr(0,j), "LINE", j+1, "BAND", band[0], NULL);

	// Convert coord sys's if needed and compute range

        for (i=0; i < nso; i++) {           // samp

            // While you generally shouldn't compare floats for equality,
            // 0.0 is being used as a specific flag value and is
	    // representable exactly...

            PigPoint xyz_pt(xyz->get(0,j,i), xyz->get(1,j,i), xyz->get(2,j,i));

	    double range = 0.0;

            if (xyz_pt.getX() != 0.0 && xyz_pt.getY() != 0.0 &&
				        xyz_pt.getZ() != 0.0) {
                if (cs != xyz_cs) {
                    xyz_pt = cs->convertPoint(xyz_pt, xyz_cs);
                    xyz->set(0, j, i, xyz_pt.getX());
                    xyz->set(1, j, i, xyz_pt.getY());
                    xyz->set(2, j, i, xyz_pt.getZ());
                }
		double diff_x = xyz_pt.getX() - rangeOrigin.getX();
		double diff_y = xyz_pt.getY() - rangeOrigin.getY();
		double diff_z = xyz_pt.getZ() - rangeOrigin.getZ();
		range = sqrt(diff_x*diff_x + diff_y*diff_y + diff_z*diff_z);

		if (cast != NULL) {		// Save the direction vector
		    PigPoint cast_vec(diff_x, diff_y, diff_z);
		    cast_vec.normalize();
		    cast->set(0, j, i, cast_vec.getX());
		    cast->set(1, j, i, cast_vec.getY());
		    cast->set(2, j, i, cast_vec.getZ());
		}
	    }
	    rng->set(0, j, i, range);
	}
    }

    // PATCH IMPLEMENTATION
    // At this point have camera models, coord systems, file models, pointers, and rng for range in the system. 
    // We'll also need the raw image.

    /* 
    How does the algorithm work?
    1) Load the xyz, and the image, and geometrical info.                                                       
    4) Produce the  mask                                                                                               
    5) Patch the patches using fitting and other guesswork (include noise)
    8) Segment the image into sand, rocks, and sky (intrinsic albedo)

    9) Initialize energy function parameters and history                             
    10) Generate range perturbations (snow or cross hatch?)
    11) Iterate initial patch by performing energy gradient descent                                                  
    12) Track and print progress                                                                              
    13) Return optimized patch      
    */

    ////////////////
    // Perform preprocessing
    ////////////////

    // Cast short int img to float img for later manipulation
    for (int j=0;j<nlo;j++) {
      for (int i=0;i<nso;i++) {
	img->set(0,j,i,static_cast<float>(img_int->get(0,j,i))/8192.0); // Divide by 2**13. Otherwise radiance errors are unbalanced.
      }
    }

    // Talk to me
    snprintf(msg,msgLen,"Completed import, commencing preprocessing.","");
    zvmessage(msg, "");

    // Generate the mask
    SIf *mask = new SIf(1, nlo, nso);
    generate_mask(rng, mask);
    
    // Generate initial guess.
    snprintf(msg,msgLen,"Generating initial guess.","");
    zvmessage(msg, "");
    SIf *rng0 = new SIf(1, nlo, nso);
    generate_initial_patch(rng, mask, 5, 7, rng0);

    // Generate xyz0 from rng0
    snprintf(msg,msgLen,"Converting to Cartesian coords.","");
    zvmessage(msg, "");
    SIf *xyz0 = new SIf(3, nlo, nso);
    rng2xyz(rng0, file_models[0], camera_in[0], cs, rangeOrigin, xyz0);

    // Generate uvw_free from rng0
    SIf *uvw_free = new SIf(3, nlo, nso);
    generate_uvw(xyz0, rangeOrigin, uvw_free);
    norm_arr(uvw_free); // normalize and trim empty parts

    // Segment image by intrinsic albedo
    snprintf(msg,msgLen,"Segment image by intrinsic albedo.","");
    zvmessage(msg, "");
    SIf *albedo = new SIf(1, nlo, nso);

    // Define sun unit vector in site frame.
    double az = file_models[0]->getSolarAzimuth(999);
    double el = file_models[0]->getSolarElevation(999);
    if (az==999||el==999) {
      zvmessage("Don't know where sun is.","");
    }
    // Uses the default coordinate system.
    PigVector s0t = cs->constructVector(az,el);
    PigVector *s0 = &s0t;
    // Note that as defined, the solar vector points away from the sun, not toward it.

    snprintf(msg, msgLen,"s0 x, y, z: %f, %f, %f", s0->getX(), s0->getY(), s0->getZ());
    zvmessage(msg, "");

    // Work out what the intrinsic albedo is.
    intrinsic_albedo(img, mask, albedo);

    // Estimate beta by generating an initial radiance estimate
    snprintf(msg,msgLen,"Calibrate beta by generating initial radiance estimate.","");
    zvmessage(msg, "");
    SIf *rad0 = new SIf(1, nlo, nso);
    radiance_uvw(uvw_free, albedo, s0, rad0);
    
    snprintf(msg, msgLen, "arr_mean(img), arr_mean(albedo), arr_mean(rad0), %f, %f, %f", arr_mean(img), arr_mean(albedo), arr_mean(rad0));
    zvmessage(msg, "");

    // generate rad0 using rng0
    double beta = arr_mean(img)/arr_mean(rad0);

    if (arr_mean(img)==0.0) {
      snprintf(msg,msgLen,"Image has not imported properly, data is all zero.");
      zvmessage(msg, "");
    }

    if (arr_mean(xyz)==0.0) {
      snprintf(msg,msgLen,"Point cloud has not imported properly, data is all zero.");
      zvmessage(msg, "");
    }

    
    ////////////////
    // Perform optimization
    ////////////////

    snprintf(msg,msgLen,"Performing optimization.","");
    zvmessage(msg, "");

    // The optimization simultaneously solves for beta, rng, and uvw_free (independent)
    // It is based on the schema described in Jiant et al. 2019, with substantial changes

    // Set energy function parameters
    // These are for rad error, norm error, mask error (range), and baseline error (spikes) respectively.
    array<double, 4> energy_pars={1e1,1e-2,1e-1,1e-1};
    static double epsi(0.0001);
    static int num_steps=30;
    static double alpha=1.0;
    static double p=-2.0;

    // Generate rng perturbations
    // This is an alternating cross hatch scaled by epsi and the intrinsic range rng0.
    SIf *rng_eps = new SIf(4, nlo, nso);
    for (int j=0;j<nlo;j++) {
      for (int i=0;i<nso;i++) {
	rng_eps->set(0,j,i,(j%2)*epsi*rng0->get(0,j,i));
	rng_eps->set(1,j,i,(-(j+1)%2)*epsi*rng0->get(0,j,i));
	rng_eps->set(2,j,i,(i%2)*epsi*rng0->get(0,j,i));
	rng_eps->set(3,j,i,(-(i+1)%2)*epsi*rng0->get(0,j,i));
      }
    }

    // Generate some useful SIfs to make helper functions work better.
    SIf *arr_epsi = new SIf(1, nlo,nso);
    SIf *minus_one = new SIf(1, nlo,nso);
    //SIf *uvw_eps = new SIf(nlo,nso);
    //SIf *uvw_eps0 = new SIf(3,nlo,nso);
    //SIf *uvw_eps1 = new SIf(3,nlo,nso);
    //SIf *uvw_eps2 = new SIf(3,nlo,nso);
    //arr_epsi->zero(); // This takes forever
    //minus_one->zero();
    //uvw_eps->zero();
    //uvw_eps0->zero();
    //uvw_eps1->zero();
    //uvw_eps2->zero();
    for (int j=0;j<nlo;j++) {
      for (int i=0;i<nso;i++) {
	arr_epsi->set(0,j,i,epsi);
	minus_one->set(0,j,i,-1.0);
      }
    }
       //	uvw_eps->set(j,i,epsi);
	//uvw_eps0->set(0,j,i,epsi);
	//uvw_eps1->set(1,j,i,epsi);
	//uvw_eps2->set(2,j,i,epsi);
    //}
    //}

    // Declare memory array for retrospective analysis of convergence
    vector<double> energy_memory;
    SIf *old_energy = new SIf(1, nlo,nso);
    energy(beta, albedo, img, s0, uvw_free, rng, rng0, mask, energy_pars, p, 
	   file_models[0], camera_in[0], cs, rangeOrigin, old_energy);

    energy_memory.push_back(arr_mean(old_energy));
    // Print last element of the array

    snprintf(msg,msgLen,"initial energy: %f", arr_mean(old_energy));
    zvmessage(msg,"");

    // Need other stuff here
    SIf *new_energy = new SIf(1, nlo, nso);
    SIf *uvw_pert = new SIf(3, nlo, nso);
    SIf *rng_pert = new SIf(1, nlo, nso);
    double beta_pert(0.0);
    double delta(0.0);

    // rng_opt is the output quantity to be solved for
    SIf *rng_opt = new SIf(1, nlo, nso);
    for (int j=0;j<nlo;j++) {
      for (int i=0;i<nso;i++) {
	rng_opt->set(0,j,i,rng0->get(0,j,i));
      }
    }

    // Do the big loop
    for (int s=0;s<num_steps;s++) {

      snprintf(msg,msgLen,"step: %u of %u", s, num_steps);
      zvmessage(msg,"");

      // Solve uvw_free
      for (int k=0;k<3;k++) {
	// Generate uvw_pert
	for (int j=0;j<nlo;j++) {
	  for (int i=0;i<nso;i++) {
	    for (int b=0;b<3;b++) {
	      if (b==k) {
		uvw_pert->set(k,j,i,epsi+uvw_free->get(k,j,i));
	      }
	      else {
		uvw_pert->set(k,j,i,uvw_free->get(k,j,i));
	      }
	    }
	  }
	}
	// Ensure everything is a unit vector
	norm_arr(uvw_pert);

	// solve new energy
	snprintf(msg,msgLen,"uvw energy");
	zvmessage(msg,"");

	energy(beta, albedo, img, s0, uvw_pert, rng_opt, rng0, mask, energy_pars, p, 
	       file_models[0], camera_in[0], cs, rangeOrigin, new_energy);
	
	// use new energy to update uvw_free
	for (int j=0;j<nlo;j++) {
	  for (int i=0;i<nso;i++) {
	    // This occasionally can give unphysically large amplitudes which tends to mess everything up.
	    // It seems to strongly depend on the absolute numerical value of the energy, which seems to me
	    // to be a conceptual mistake for a parameter which is fundamentally restricted to the surface
	    // of a unit sphere.
	    delta = 0.01*alpha*(new_energy->get(0,j,i)-old_energy->get(0,j,i))/epsi;
	    while (delta*delta>0.01) {
	      delta = 0.1*delta;
	    }
	    //delta = 0.1*alpha*(new_energy->get(0,j,i)-old_energy->get(0,j,i))/old_energy->get(0,j,i);
	    uvw_free->set(k,j,i,uvw_free->get(k,j,i)-delta);
	  }
	}
	// Ensure everything is a unit vector
	norm_arr(uvw_free);

	// reset old_energy
	energy(beta, albedo, img, s0, uvw_free, rng_opt, rng0, mask, energy_pars, p, 
	       file_models[0], camera_in[0], cs, rangeOrigin, old_energy);

	// append energy_memory vector
	energy_memory.push_back(arr_mean(old_energy));
      }
      
      // Solve range
      // This varies the most from Jiang et al. 2019, as the E-L formalism is unstable at oblique angles
      for (int k=0;k<4;k++) {
	// update rng_opt
	for (int j=0;j<nlo;j++) {
	  for (int i=0;i<nso;i++) {
	    rng_pert->set(0,j,i,rng_opt->get(0,j,i)+rng_eps->get(k,j,i));
	  }
	}
	// Update new_energy
	snprintf(msg, msgLen, "range energy");
	zvmessage(msg,"");

	energy(beta, albedo, img, s0, uvw_free, rng_pert, rng0, mask, energy_pars, p, 
	       file_models[0], camera_in[0], cs, rangeOrigin, new_energy);

	for (int j=0;j<nlo;j++) {
	  for (int i=0;i<nso;i++) {
	    delta=0.0;
	    if (rng_eps->get(k,j,i)!=0.0) {
	      delta = (new_energy->get(0,j,i)-old_energy->get(0,j,i))/rng_eps->get(k,j,i);
	      rng_opt->set(0,j,i,rng_opt->get(0,j,i)-0.005*alpha*delta);
	    }
	  }
	}
	// reset old_energy
	energy(beta, albedo, img, s0, uvw_free, rng_opt, rng0, mask, energy_pars, p, 
	       file_models[0], camera_in[0], cs, rangeOrigin, old_energy);

	// append energy_memory vector
	energy_memory.push_back(arr_mean(old_energy));
      }

      // Solve beta
      beta_pert = beta+epsi;
      // Update new_energy
      // Finally can use some helper functions. Beware that array names are changing here.

      snprintf(msg,msgLen,"beta energy");
      zvmessage(msg,"");

      energy(beta_pert, albedo, img, s0, uvw_free, rng_opt, rng0, mask, energy_pars, p, 
	     file_models[0], camera_in[0], cs, rangeOrigin, new_energy);

      arr_mult(old_energy, minus_one, old_energy);
      arr_add(new_energy, old_energy, new_energy);
      safe_divide(new_energy,arr_epsi, new_energy);

      beta -= alpha*arr_mean(new_energy);

      // reset old_energy
      energy(beta, albedo, img, s0, uvw_free, rng_opt, rng0, mask, energy_pars, p, 
	     file_models[0], camera_in[0], cs, rangeOrigin, old_energy);
      
      // append energy_memory vector
      energy_memory.push_back(arr_mean(old_energy));

      // Optionally put more history and print statements about here.

      // Test exit condition might go here.

    }

    // Convert RNG back to XYZ

    sprintf(msg,"Converting RNG back to XYZ.","");
    zvmessage(msg, "");

    for (j=0; j < nlo; j++) {
	for (i=0; i < nso; i++) {
	    xyz_out->set(0, j, i, 0.0);
	    xyz_out->set(1, j, i, 0.0);
	    xyz_out->set(2, j, i, 0.0);

	    double range = rng_opt->get(0,j, i);

	    if (range == 0.0)
		continue;		// invalid point

	    double img_l = j + file_models[0]->getYOffset();
	    double img_s = i + file_models[0]->getXOffset();

	    // Get the view vector, either from the cmod or the saved dir

	    PigVector img_vector;
	    if (cast != NULL) {		// Use the saved direction
		img_vector.setXYZ(cast->get(0,j,i),
				  cast->get(1,j,i),
				  cast->get(2,j,i));
	    } else {
                // Compute the XYZ point.  We get the view ray direction and
                // then go a distance "range" along it.  Although the C point
	        // changes for CAHVORE, we use the same constant origin that
	        // we used to make the range.

                PigPoint img_origin;
                camera_in[0]->LStoLookVector(img_l, img_s,
                            img_origin, img_vector, cs);

                img_vector.normalize();         // make sure it's a unit vector
	    }

	    PigVector new_vector = img_vector * range + rangeOrigin;

	    // Auxiliary output verbiage for debugging.
	    //xyz_out->set(0, j, i, beta*rad0->get(0,j,i));
	    //xyz_out->set(1, j, i, albedo->get(0,j,i));
	    xyz_out->set(0, j, i, new_vector.getX());
	    xyz_out->set(1, j, i, new_vector.getY());
	    xyz_out->set(2, j, i, new_vector.getZ());

	    //for (int k=0;k<3;k++) {
	    // xyz_out->set(k,j,i, uvw_free->get(k,j,i));
	    //}
	}
    }

    // Write the output labels

    PigLabelModel *labelModel = m->createLabelModel(unit_out);
    labelModel->setPatch(file_models, nids, cs);

    // Now write the output

    sprintf(msg,"Writing output.","");
    zvmessage(msg, "");

    for (int b=0; b < 3; b++) {
        for (int j=0; j < nlo; j++) {
	    zvwrit(unit_out, xyz_out->linePtr(b,j), "BAND",b+1,"LINE",j+1,NULL);
	}
    }

    zvclose(unit_out, NULL);
}

// Various helper functions

// Low level functions                                                                                                    

// takes an inverse without exploding on zeros                                                                            
static void safe_inverse(SIf *arr_in, SIf *arr_out)
{
  int nl = arr_in->getNL();
  int ns = arr_in->getNS();

  SIf *mask = new SIf(1, nl, ns);
  generate_mask(arr_in, mask); // mask is zero in places with zeros, 1 elsewhere.

  double result(0.0);
  for (int j=0;j<nl;j++) {
    for (int i=0;i<ns;i++) {
      result = arr_in->get(0,j,i) + 1.0 - mask->get(0,j,i);
      result = 1.0/result;
      result *= mask->get(0,j,i);
      arr_out->set(0,j,i,result);
    }
  }
  mask->free();
  delete mask;


}

static void safe_divide(SIf *num_in, SIf *den_in, SIf *quot_out)
{
  int nl = num_in->getNL();
  int ns = num_in->getNS();
  
  SIf *temp = new SIf(1, nl, ns);

  safe_inverse(den_in, temp);
  arr_mult(temp, num_in, quot_out);
  
  
  temp->free();
  delete temp;

}
  
// May have to add vector/array functions in here if there's no decent library available                                 
static void arr_add(SIf *A, SIf *B, SIf *C)
{
  // A+B=C                                                                     
  int nl = A->getNL();
  int ns = A->getNS();
  
  double sum(0.0);
  for (int j=0; j < nl; j++) {
    for (int i=0; i < ns; i++) {
      sum = A->get(0,j, i);
      sum += B->get(0,j,i);
      C->set(0,j, i, sum);
    }
  }
}

static void arr_mult(SIf *A, SIf *B, SIf *C)
{
  // A*B=C
  int nl = A->getNL();
  int ns = A->getNS();

  double prod(0.0);
  for (int j=0; j < nl; j++) {
    for (int i=0; i < ns; i++) {
      prod = A->get(0,j, i);
      prod *= B->get(0,j,i);
      C->set(0, j, i, prod);
    }
  }
}

static double arr_mean(SIf *A)
{
  // Takes mean of all array values
  int nl = A->getNL();
  int ns = A->getNS();

  double output(0.0);
  for (int j=0;j<nl;j++) {
    for (int i=0;i<ns;i++) {
      output += A->get(0,j,i);
    }
  }
  
  output /= nl*ns;

  return output;

}
 
static void norm_arr(SIf *A)
{
  // takes a 3D SIf and ensures it is normalized across the first three indices, when they represent unit vectors.
  int nl = A->getNL();
  int ns = A->getNS();

  double mag(0.0);
  for (int j=0;j<nl;j++) {
    for (int i=0;i<ns;i++) {
      mag = 0.0;
      for (int k=0;k<3;k++) {
	mag += pow(A->get(k,j,i),2.0);
      }
      mag = pow(mag,0.5);
      if (mag!=0.0) {
	for (int k=0;k<3;k++) {
	  A->set(k,j,i,A->get(k,j,i)/mag);
	}
      }
      else {
	A->set(2,j,i,-1.0);// This is only used for uvw, and (0,0,-1) is flat terrain. For some reason
	// the xyz-to-uvw function isn't dense. 
      }
    }
  }
}

static void sign(SIf *A, SIf *sign)
{
  // returns a SIf with sign test, 0 for 0.
  int nl = A->getNL();
  int ns = A->getNS();
  
  double test(0.0);
  for (int j=0;j<nl;j++) {
    for (int i=0;i<ns;i++) {
      if (A->get(0,j,i)<0) {
	test = -1.0;
      } else if (A->get(0,j,i)>0) {
	test = 1.0;
      } else {
	test = 0.0;
      }
      sign->set(0,j,i,test);
    }
  }

}

static double min(SIf *A)
{
  // returns the minimum element of a given SIf array.
  int nl = A->getNL();
  int ns = A->getNS();

  double out=A->get(0,0,0);
  for (int j=0;j<nl;j++) {
    for (int i=0;i<ns;i++) {
      if (A->get(0,j,i)<out ) {
	out = A->get(0,j,i);
      }
    }
  }

  return out;
}
    
// Helper functions                                                                                                    

// Will do a sign test on the input image to calculate where stereo matching has not provided a solution.               
static void generate_mask(SIf *rng, SIf *mask)
{
  // Mask is 0 where input data is 0, 1 elsewhere. 
  // Can preprocess to remove areas of likely poor stereo performance.
  int nl = rng->getNL();
  int ns = rng->getNS();

  for (int j=0; j<nl; j++) {
    for (int i=0; i<ns; i++) {
      if (rng->get(0,j,i)>0.0) {
	mask->set(0,j,i,1.0);
      }
      else {
	mask->set(0,j,i,0.0);
      }
    }
  }
}  

// xyz to rng                                                                                                           
static void xyz2rng(SIf *xyz_in, PigPoint rangeOrigin, SIf *rng_out) // May need other inputs
{
  // Copied from marsrfilt, functionified for convenience.

  // Assuming using camera model for all info, no artificial image vector given (cast)

  int nl = xyz_in->getNL();
  int ns = xyz_in->getNS();
  for (int j=0; j < nl; j++) {               // line                                                                      
     for (int i=0; i < ns; i++) {           // samp                                                                   

      // While you generally shouldn't compare floats for equality,                                                  
      // 0.0 is being used as a specific flag value and is                                                           
      // representable exactly...                                                                                    

      PigPoint xyz_pt(xyz_in->get(0,j,i), xyz_in->get(1,j,i), xyz_in->get(2,j,i));

      double range = 0.0;

      if (xyz_pt.getX() != 0.0 && xyz_pt.getY() != 0.0 &&
	  xyz_pt.getZ() != 0.0) {
	// Coordinate system is assumed to be consistent, do conversion if necessary outside this function
	double diff_x = xyz_pt.getX() - rangeOrigin.getX();
	double diff_y = xyz_pt.getY() - rangeOrigin.getY();
	double diff_z = xyz_pt.getZ() - rangeOrigin.getZ();
	range = sqrt(diff_x*diff_x + diff_y*diff_y + diff_z*diff_z);

      }
      rng_out->set(0, j, i, range);
    }
  }
}

// rng to xyz                                                                                                           
static void rng2xyz(SIf *rng_in, PigFileModel *file_models0, 
		    PigCameraModel *camera_in0, PigCoordSystem *cs, 
		    PigPoint rangeOrigin, SIf *xyz_out)
{
  // May need other inputs                                       
  int nl = rng_in->getNL();
  int ns = rng_in->getNS();

  for (int j=0; j < nl; j++) {
    for (int i=0; i < ns; i++) {
      xyz_out->set(0, j, i, 0.0);
      xyz_out->set(1, j, i, 0.0);
      xyz_out->set(2, j, i, 0.0);

      double range = rng_in->get(0, j, i);

      if (range == 0.0)
	continue;               // invalid point                                                                    

      double img_l = j + file_models0->getYOffset();
      double img_s = i + file_models0->getXOffset();

      PigVector img_vector;
      // No saved direction.
      // Compute the XYZ point.  We get the view ray direction and                                                
      // then go a distance "range" along it.  Although the C point                                               
      // changes for CAHVORE, we use the same constant origin that                                                
      // we used to make the range.                                                                               

      PigPoint img_origin;
      camera_in0->LStoLookVector(img_l, img_s,
				   img_origin, img_vector, cs);

      img_vector.normalize();         // make sure it's a unit vector                                             

      PigVector new_vector = img_vector * range + rangeOrigin;

      xyz_out->set(0, j, i, new_vector.getX());
      xyz_out->set(1, j, i, new_vector.getY());
      xyz_out->set(2, j, i, new_vector.getZ());
    }
  }
}

// Wrapper of some external function                                                                                       
static void generate_uvw(SIf *xyz_in, PigPoint rangeOrigin, SIf *uvw_out)
//
//			 PigFileModel *file_models0, 
//		    PigCameraModel *camera_in0, PigCoordSystem *cs, 
//		    PigPoint rangeOrigin, SIf *uvw_out)
{
  // call xyz_to_uvw from /mars/src/prog/marsuvw/
  int nlo = xyz_in->getNL();
  int nso = xyz_in->getNS();

  const int MSG_LEN=256;
  char msg[MSG_LEN];

  // Need to create some ancillary parameters to operate a old function through an obsolete interface.
  XyzToUvwParams params;

  // TODO fix this parameter party.
  params.max_point_separation = -1.0;
  params.max_plane_error = -1.0;
  params.min_num_points = 3; // 6 or more recommended. I like to live dangerously.
  params.window_radius = 2;
  params.rejection_ratio = 2.0; // recommended.
  params.x_center = 0.0; // unsure what this does.
  params.y_center = 0.0; // unsure what this does.
  params.box_radius = 200.0; // meters, half width of bounding box. Won't compute outside.
  params.flip_threshold = 1.1; // disaple sign flipping.
  params.flip_distance = params.box_radius; // min distance for sign flipping.
  params.slope_mode = 0; // I'm interested in fine grain vectors etc up close.

  double cameraPosition[3];
  double *p_xyz[3];
  double *p_uvw[3];

  // Get camera position
  rangeOrigin.getXYZ(cameraPosition);

  // Allocate memory for input XYZ.
  for (int i=0; i<3; i++) {
    p_xyz[i] = (double *)malloc(nlo * nso * sizeof(double));
    if (p_xyz[i] == NULL) {
      snprintf(msg, MSG_LEN, "Unable to allocate memory for XYZ input %d", i);
      zvmessage(msg, "");
      zabend();
    }
    p_uvw[i] = (double *)malloc(nlo * nso * sizeof(double));
    if (p_uvw[i] == NULL) {
      snprintf(msg, MSG_LEN, "Unable to allocate memory for UVW output %d", i);
      zvmessage(msg, "");
      zabend();
    }
  }

  // Read in the XYZ file(s)
  for (int band = 0; band < 3; band++) {
    for (int line = 0; line < nlo; line++) {
      for (int i=0;i<nso;i++) {
	*((p_xyz[band]) + (line*nso) + i) = xyz_in->get(band,line,i);
      }
    }
  }

  // Do the work
  double test(0.0);
  int status = xyz_to_uvw(&params, p_xyz, nlo, nso, cameraPosition, p_uvw);


  //  `xyz_to_uvw(XyzToUvwParams*, double**, int, int, double*, double**)'
  
  //  int xyz_to_uvw(XyzToUvwParams *params,   /* parameters */
  //		 double *xyz[3],           /* arrays of x, y, z points */
  //		 int num_rows,             /* number of rows in xyz image */
  //		 int num_columns,          /* number of columns in xyz image */
  //		 double camera[3],         /* location of camera */
  //		 double *uvw[3]);          /* output array */




  if (status != 0) {
    snprintf(msg, MSG_LEN, "xyz_to_uvw failed!! status code=%d", status);
    zvmessage(msg, "");
    zabend();
  }

  // Write out the UVW file(s)
  // Yes, z is defined downwards. 
  for (int band = 0; band < 3; band++) {
    for (int line = 0; line < nlo; line++) {
      for (int i=0;i<nso;i++) {
	uvw_out->set(band,line,i,*(p_uvw[band] + line*nso + i));
      }
    }
  }

  // Free memory.
  for (int i=0;i<3;i++) {
    free (p_xyz[i]);
    free (p_uvw[i]);
  }

}


// Adds random noise to range data                                                                                         
static void add_noise_to_range(SIf *rng_in, SIf *rng_out, double noise)
{
  // Initialize random seed
  srand(time(NULL));
  int nl = rng_in->getNL();
  int ns = rng_in->getNS();

  double range(0.0);
  double random_noise(0.0);
  for (int j=0; j < nl; j++) {
    for (int i=0; i < ns; i++) {
      range = rng_in->get(0,j,i);
      // Very ugly almost uniform distribution [-1,1]
      random_noise = 2.0*0.0001*double(rand()%10000)-1.0;
      range += noise*random_noise;
      rng_out->set(0, j, i, range);
    }
  }
}

// Find a library which can do this, hopefully                                                                             
static void gaussian_filter(SIf *arr_in, int window_vert, int window_hor, SIf *arr_out)
{
  // Generate Gaussian kernel
  SIf *kernel = new SIf(1, 2*window_vert, 2*window_hor);
  for(int j=0;j<2*window_vert;j++) {
    for(int i=0;i<2*window_hor;i++) {
      kernel->set(0,j,i, exp(-pow(double(j-window_vert)/double(window_vert),2)
			   -pow(double(i-window_hor)/double(window_hor),2)));
    }
  }

  // Convolve with arr_in
  int nlo = arr_in->getNL();
  int nso = arr_in->getNS();

 int j = 0;
#ifdef _OPENMP
   #pragma omp parallel for
   for (j = 0; j < nlo; j++) {
#else
   for (j = 0; j < nlo; j++) {
#endif   
    for (int i=0; i < nso; i++) {
      double total = 0.0; // the zeroth moment of the convolution, used for normalization
      arr_out->set(0,j,i,0.0);
      for (int k=0;k<2*window_vert;k++) {
	for (int m=0;m<2*window_hor;m++) {
	  int line_index = j-window_vert+k;
	  int samp_index = i-window_hor+m;
	  // Check that index is within bounds.
	  if( line_index>=0 && line_index<nlo && samp_index>=0 && samp_index<nso) {
	    total += kernel->get(0,k,m);
	    double incr = arr_in->get(0,line_index,samp_index)*kernel->get(0,k,m); // the first moment of the convolution
	    arr_out->set(0,j,i,arr_out->get(0,j,i)+incr);
	  }
	}
      }
      // Apply normalization
      arr_out->set(0,j,i,arr_out->get(0,j,i)/total);
    }
  }

  kernel->free();
  delete kernel;
}

static double determinant(SIf *x) 
{
  // Assumes square matrix of dimension 3.
  //int N = x->getNL();
  double output(0.0);
  output += x->get(0,0,0)*x->get(0,1,1)*x->get(0,2,2);
  output -= x->get(0,0,0)*x->get(0,2,1)*x->get(0,1,2);
  output -= x->get(0,1,0)*x->get(0,0,1)*x->get(0,2,2);
  output += x->get(0,1,0)*x->get(0,2,1)*x->get(0,0,2);
  output += x->get(0,2,0)*x->get(0,0,1)*x->get(0,1,2);
  output -= x->get(0,2,0)*x->get(0,1,1)*x->get(0,0,2);
  return output;
}

// Performs 2D linear regression of array f over x and y, excluding empty data (marked 0), then fills it in.             
static void linear_regression(SIf *x, SIf *y, SIf *f, SIf *f_out)
{
  // f = a + b*x + c*y. Find a, b, and c (the SIf beta).
  // Use Cramer's rule etc.
  
  int nlo = f->getNL();
  int nso = f->getNS();
  
  vector<double> f_vec;
  SIf *X = new SIf(1, nlo*nso, 3);
  int vi(0);
  double f_val(0.0);
  // run through the f array, ignoring zeros
  for(int j=0;j<nlo;j++) {
    for(int i=0;i<nso;i++) {
      f_val = f->get(0,j,i);
      if(f_val!=0.0) {
	f_vec.push_back(f_val);
	X->set(0,vi,0,1.0);
	X->set(0,vi,1,x->get(0,j,i));
	X->set(0,vi,2,y->get(0,j,i));
	vi++;
      }
    }
  }
  
  // Find XTX and XTy
  SIf *XTX = new SIf(1,3,3);
  SIf *XTy = new SIf(1,3,1);
  double value(0.0);
  for (int j=0;j<3;j++) {
    for (int i=0;i<3;i++) {
      value = 0.0;
      for (int k=0;k<vi;k++) {
	value += X->get(0,k,j)*X->get(0,k,i);
      }
      XTX->set(0,j,i,value);
    }
  }

  for (int j=0;j<3;j++) {
    value=0.0;
    for (int k=0;k<vi;k++) {
      value += X->get(0,k,j)*f_vec[k];
    }
    XTy->set(0,j,0,value);
  }

  // Solve XTX.beta = XTy using Cramer's rule
  // beta_i = det(XTX_i)/det(XTX), where XTX_i is XTX with the ith column replaced by XTy.
  SIf *beta = new SIf(1,3,1);
  SIf *XTX_i = new SIf(1,3,3);
  for (int i=0;i<3;i++) {
    // set XTX_i
    for (int j=0;j<3;j++) {
      for (int k=0;k<3;k++) {
	XTX_i->set(0,j,k,XTX->get(0,j,k));
      }
      XTX_i->set(0,j,i,XTy->get(0,j,0));
    }
    beta->set(0,i,0,determinant(XTX_i)/determinant(XTX));
  }

  X->free();  
  delete X;
  XTX->free();
  delete XTX;
  XTy->free();
  delete XTy;
  XTX_i->free();
  delete XTX_i;
  


  

  // Resample across the space densely
  // f = a + b*x + c*y. Find a, b, and c (the SIf beta).
  for(int j=0;j<nlo;j++) {
    for(int i=0;i<nso;i++) {
      f_out->set(0,j,i,beta->get(0,0,0) + beta->get(0,1,0)*x->get(0,j,i) + beta->get(0,2,0)*y->get(0,j,i));
    }
  }
  
  beta->free();  
  delete beta;
  
}

// Algorithm level functions                                                                                               

// Generate initial condition                                                                                              
// generate patch helper                                                                                                   
static void gaussian_initial_patch(SIf *rng, SIf *mask, int window_vert, int window_hor, SIf *rng0)
{
  int nlo = rng->getNL();
  int nso = rng->getNS();
  // Performs in-place gaussian blurring of rng normalized by mask, applied in areas needing patches.
  // rng = mask*rng + (1-mask)*gaussian(rng)/gaussian(mask)
  SIf *rng_blur = new SIf(1,nlo,nso);
  SIf *mask_blur = new SIf(1,nlo,nso);
  SIf *quotient = new SIf(1,nlo,nso);
  
  gaussian_filter(rng, window_vert, window_hor, rng_blur);
  gaussian_filter(mask, window_vert, window_hor, mask_blur);
  safe_divide(rng_blur, mask_blur, quotient);

  SIf *mask_conj = new SIf(1,nlo,nso);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      mask_conj->set(0,j,i,1.0-mask->get(0,j,i));
    }
  }

  SIf *output1 = new SIf(1,nlo,nso);
  arr_mult(rng, mask, output1);

  SIf *output2 = new SIf(1,nlo,nso);
  arr_mult(mask_conj,quotient,output2);
  
  arr_add(output1, output2, rng0);

  rng_blur->free();  
  delete rng_blur;
  mask_blur->free();
  delete mask_blur;
  quotient->free();
  delete quotient;
  mask_conj->free();
  delete mask_conj;
  output1->free();
  delete output1;
  output2->free();
  delete output2;
  

}

// Performs linear regression of inverse range, blending using a Gaussian kernel                                           
static void generate_initial_patch(SIf *rng, SIf *mask, int window_vert, int window_hor, SIf *rng0)
{
  int nlo = rng->getNL();
  int nso = rng->getNS();
  // Take inverse of rng (inv_rng)
  SIf *inv_rng = new SIf(1,nlo,nso);
  safe_inverse(rng,inv_rng);
  
  // Construct SIf of line index, sample index, and inv_rng
  SIf *inv_rng_l = new SIf(1,nlo,nso);
  SIf *inv_rng_s = new SIf(1,nlo,nso);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      inv_rng_l->set(0,j,i,j); 
      inv_rng_s->set(0,j,i,i);
    }
  }
  
  // Fit a linear regression to this SIf, filling the whole space (inv_rng_lin)
  char msg[256];
  sprintf(msg,"    Fit a linear regression to inverse range.","");
  zvmessage(msg, "");
  SIf *inv_rng_lin = new SIf(1,nlo,nso);
  linear_regression(inv_rng_l,inv_rng_s,inv_rng,inv_rng_lin);
  
  inv_rng_l->free();
  delete inv_rng_l;
  inv_rng_s->free();
  delete inv_rng_s;
  
  // Check for zero crossings (need min helper function)
  // Why? We're going to divide inv_rng by inv_rng_lin. If the fit accidentally crosses zero somewhere in the distance
  // This is clearly unphysical
  // Then the division process will be very problematic
  if (min(inv_rng_lin)<0.0) {
    double correction = 1.5*min(inv_rng_lin); // (this is a negative number)
    for (int j=0;j<nlo;j++) {
      for (int i=0;i<nso;i++) {
	inv_rng_lin->set(0,j,i,inv_rng_lin->get(0,j,i)-correction);
      }
    }
  }

  // Divide inv_rng by inv_rng_lin (inv_rng_norm)
  // inv_rng_norm is a more-or-less flat collection of lumps and bumps, apt for smoothing.
  SIf *inv_rng_norm = new SIf(1,nlo,nso);
  safe_divide(inv_rng,inv_rng_lin,inv_rng_norm);

  // Use gaussian blur wrapper above to blend inv_rng_norm
  sprintf(msg,"    Blur inverse range.","");
  zvmessage(msg, "");
  SIf *inv_rng_norm0 = new SIf(1,nlo,nso);
  gaussian_initial_patch(inv_rng_norm, mask, window_vert, window_hor, inv_rng_norm0);
  
  // Do this recursively based on a test of the patch still containing zeros. Let the window size grow during this process.
  SIf *local_mask = new SIf(1,nlo,nso);
  int counter(1);
  while (min(inv_rng_norm0)==0.0) {
    sprintf(msg,"        Recursively filling gaps: %d",counter);
    zvmessage(msg, "");
    sign(inv_rng_norm0,local_mask);
    counter++;
    gaussian_initial_patch(inv_rng_norm0, local_mask, counter*window_vert, counter*window_hor, inv_rng_norm0);
    // Loop exit condition
    if (counter>10) {
      sprintf(msg, "      Exited loop with gaps remaining, consider increasing steps");
      zvmessage(msg, "");
      break;
    }
  }
  // When this exits all the holes have been filled in via some gaussian smoothing.

  // de-normalize the patch by multiplying by inv_rng_lin.
  SIf *inv_rng0 = new SIf(1,nlo,nso);
  arr_mult(inv_rng_norm0,inv_rng_lin,inv_rng0);

  // Take the inverse, write to rng0 as output.
  safe_inverse(inv_rng0,rng0);

  sprintf(msg,"    Complete initial estimate.","");
  zvmessage(msg, "");

  inv_rng->free();
  delete inv_rng;
  inv_rng0->free();  
  delete inv_rng0;
  inv_rng_lin->free();
  delete inv_rng_lin;
  inv_rng_norm->free();
  delete inv_rng_norm;
  inv_rng_norm0->free();
  delete inv_rng_norm0;
  local_mask->free();
  delete local_mask;

}

// Segment image into sand, rocks, sky                                                                                     
// Attempts to generate a "flat" image with the true underlying albedo of each feature.                                    
// This can be done by binning or wavelets.                                                                                
static void intrinsic_albedo(SIf *img, SIf *mask, SIf *albedo)
// In future make a full photometric correction system. 
// For now, just take averages of masked and unmasked parts of the image, trying to exclude sky.
// Perhaps the best approach here is to run two betas, one for sand and one for rocks?
// This depends on the mask definition.
{
  int nlo = img->getNL();
  int nso = img->getNS();
  // Generate mask conjugate
  SIf *mask_conj = new SIf(1,nlo,nso);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      mask_conj->set(0,j,i,1.0-mask->get(0,j,i));
    }
  }
  SIf *rocks = new SIf(1,nlo,nso);
  arr_mult(mask,img,rocks);
  double rock_albedo = arr_mean(rocks)/(arr_mean(mask));

  SIf *sand = new SIf(1,nlo,nso);
  arr_mult(mask_conj,img,sand);
  double sand_albedo = arr_mean(sand)/(arr_mean(mask_conj));

  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      albedo->set(0,j,i,sand_albedo*mask_conj->get(0,j,i)+rock_albedo*mask->get(0,j,i));
    }
  }

  mask_conj->free();
  rocks->free();
  sand->free();
  delete mask_conj;
  delete rocks;
  delete sand;
  
  
  // Returns a segmented image with stereo-defined areas the average rock color and patches the average sand color.
  // This is really basic but it's good enough for this job, because we only care about shading in sandy sections.
}

// Define energy function                                                                                                  
// Radiance helper                                                                                                         
static void radiance_uvw(SIf *uvw, SIf *albedo, PigVector *s0, SIf *radiance)
{
  // A better coder would find some way to throw Unity in here.                                                       

  int nlo = albedo->getNL();
  int nso = albedo->getNS();

  // Calculate a proxy for ambient light
  double albedo_mean = 0.16;//0.4;//arr_mean(albedo); I think I know better than the camera what the surface albedo is.
  double ambient = albedo_mean + pow(albedo_mean,2.0) + pow(albedo_mean,3.0);

  // Define a mask on uvw.
  SIf *uvw_mean = new SIf(1,nlo, nso);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      uvw_mean->set(0,j,i,pow(uvw->get(0,j,i),2)+pow(uvw->get(1,j,i),2.0)+pow(uvw->get(2,j,i),2.0));
    }
  }
  SIf *uvw_mask = new SIf(1,nlo,nso);
  generate_mask(uvw_mean,uvw_mask);

  // cosine of illumination direction and sun
  //SIf *cosine = new SIf(1,nlo,nso);
  SIf *shadow = new SIf(1,nlo,nso);
  double dot(0.0);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      // s0 is the solar irradiance vector pointing away from the sun, not towards it. So, negative signs.
      dot = -uvw->get(0,j,i)*s0->getX() - uvw->get(1,j,i)*s0->getY() - uvw->get(2,j,i)*s0->getZ();
      //cosine->set(j,i,dot);
      if (dot>0) {
	shadow->set(0,j,i,dot+ambient);
      } else {
	shadow->set(0,j,i,ambient);
      }
      //      shadow->set(j,i,0.5*dot+0.5*fabs(dot)+ambient); // This is array-centric shorthand.
    }
  }
  
  arr_mult(uvw_mask,albedo,radiance);
  arr_mult(shadow,radiance,radiance);
  
  uvw_mean->free();
  uvw_mask->free();
  shadow->free();
  delete uvw_mean;
  delete uvw_mask;
  delete shadow;
  
}

// Radiance error                                                                                                   
static void radiance_error(double beta, SIf *radiance, SIf *img, SIf *rad_err)
{
  // return a SIf with radiance error, that is to say, by how much the synthetic image varies from the true one
  int nlo = img->getNL();
  int nso = img->getNS();
  double error(0.0);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      error = 0.5*pow(beta*radiance->get(0,j,i)-img->get(0,j,i),2.0);
      rad_err->set(0,j,i,error);
    }
  }

  //char msg[256];
  //  sprintf(msg,"        beta, radiance, img, rad_err, %f, %f, %f, %f", beta, arr_mean(radiance), \
  //	  arr_mean(img), arr_mean(rad_err));
	  //radiance->get(0,200,200),				\
	  //img->get(0,200,200), rad_err->get(0,200,200));
  //zvmessage(msg, "");

}

 // norm error                                                                                                        
static void norm_error(SIf *uvw_free, SIf *uvw_from_rng, SIf *norm_err)
{
  // return a SIf with the square lengths of the vectors of difference between uvw computed two different ways
  int nlo = uvw_free->getNL();
  int nso = uvw_free->getNS();
  double error(0.0);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      error = 0.0;
      for (int n=0;n<3;n++) {
	error += 0.5*pow(uvw_free->get(n,j,i)-uvw_from_rng->get(n,j,i),2.0);
      }
      norm_err->set(0,j,i,error);
    }
  }

}

// mask error                                                                                                          
static void mask_error(SIf *rng, SIf *rng_init, SIf *mask, double p, SIf *mask_err)
{
  // Ensure that range in areas of mask validity remain closely aligned.
  // Whole thing taken to power p (nominally -2) to ensure that near field remains well-weighted.
  // Convoluted mask gymnastics to avoid /0.
  // Adapted from Jiang et al. 2018 equation 5, though with a missing power of 2 added back in.
  int nlo = rng->getNL();
  int nso = rng->getNS();
  double error(0.0);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      error = 0.5*pow(mask->get(0,j,i)*pow(rng->get(0,j,i)+1.0-mask->get(0,j,i),p)
		      -mask->get(0,j,i)*pow(rng_init->get(0,j,i)+1.0-mask->get(0,j,i),p),2.0);
      mask_err->set(0,j,i,error);
    }
  }
}

// baseline error                                                                                                     
static void baseline_error(SIf *xyz, SIf *bl_err)
{
  // Intended to ensure that there are no deep spikey features. 
  // The basic idea here is to compute cartesian distance between all adjacent points in xyz.
  // Generally speaking, a point to the left and right will be closest, and dunes or rocks may have 
  // big dislocations near the edge.
  // Therefore, for any given point we select the (squared) distance to the nearest adjacent point as 
  // a proxy for whether that point is on some horrific spike. 
  // This does not prevent spikes that are a pair of closely aligned points.
  // Because this is VICAR, we can do the whole thing pointwise.
  int nlo = xyz->getNL();
  int nso = xyz->getNS();
  double error(0.0);

  // specify adjacent ints clockwise from top left
  int adjL[8] = {1,1,1,0,-1,-1,-1,0};
  int adjS[8] = {-1,0,1,1,1,0,-1,-1};

  double min_dist(0.0);
  double distance(0.0);

  // iterate through points
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      min_dist=-1.0;
      for (int a=0;a<8;a++) {
	// check boundaries
	if (j+adjL[a]>=0 && j+adjL[a]<nlo && i+adjS[a]>=0 && i+adjS[a]<nso) {
	  // compute distance
	  distance = 0.0;
	  for (int k=0;k<3;k++) {
	    distance += pow(xyz->get(k,j,i)-xyz->get(k,j+adjL[a],i+adjS[a]),2.0);
	  }
	  // test if distance less than current distance
	  if (min_dist<0.0) {
	    min_dist = distance;
	  } else if (distance < min_dist) {
	    min_dist = distance;
	  }
	}
      }
      bl_err->set(0,j,i,min_dist);
    }
  }
}




// Overall error                                                                                                   
// I'm not wild about the ordering of these function arguments
static void energy(double beta, SIf *albedo, SIf *img, PigVector *s0, 
		   SIf *uvw_free, SIf *rng, 
		   SIf *rng_init, SIf *mask, 
		   array<double, 4> pars, double p, 
		   PigFileModel *file_models0, PigCameraModel *camera_in0, PigCoordSystem *cs, PigPoint rangeOrigin,
		   SIf *energy_err)
{
  char msg[256];

  // Combine the previous four quantities according to pars.
  

  int nlo = rng->getNL();
  int nso = rng->getNS();

  // Create spaces to drop intermediate results
  SIf *rad_err = new SIf(1,nlo,nso);
  SIf *norm_err = new SIf(1,nlo,nso);
  SIf *mask_err = new SIf(1,nlo,nso);
  SIf *bl_err = new SIf(1,nlo,nso);

  // Compute radiance
  SIf *radiance = new SIf(1,nlo,nso);
  radiance_uvw(uvw_free, albedo, s0, radiance);
  radiance_error(beta, radiance, img, rad_err);

  // Compute uvw_from_rng
  SIf *xyz = new SIf(3,nlo,nso);
  SIf *uvw_from_rng = new SIf(3, nlo, nso);
  rng2xyz(rng, file_models0, camera_in0, cs, rangeOrigin, xyz);

  generate_uvw(xyz, rangeOrigin, uvw_from_rng);
  norm_error(uvw_free, uvw_from_rng, norm_err);
  
  // Compute nothing (treat stereo solution as fixed)
  mask_error(rng, rng_init, mask, p, mask_err);

  // Compute xyz (already done)
  baseline_error(xyz, bl_err);

  // Combine all the errors
  double nrg(0.0);
  for (int j=0;j<nlo;j++) {
    for (int i=0;i<nso;i++) {
      nrg = pars[0]*rad_err->get(0,j,i) + pars[1]*norm_err->get(0,j,i) \
	+ pars[2]*mask_err->get(0,j,i) + pars[3]*bl_err->get(0,j,i);
      energy_err->set(0,j,i,nrg);
    }
  }

  sprintf(msg,"    rad_err, norm_err, mask_err, bl_err, energy_err: %f, %f, %f, %f, %f", arr_mean(rad_err), arr_mean(norm_err), arr_mean(mask_err), arr_mean(bl_err), arr_mean(energy_err));
  zvmessage(msg,"");
    
  rad_err->free();
  norm_err->free();
  mask_err->free();
  bl_err->free();
  radiance->free();
  xyz->free();
  uvw_from_rng->free();  
  delete rad_err;
  delete norm_err;
  delete mask_err;
  delete bl_err;
  delete radiance;
  delete xyz;
  delete uvw_from_rng;
  

}

