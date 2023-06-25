////////////////////////////////////////////////////////////////////////
// marstie - multimission tiepoint gathering program
////////////////////////////////////////////////////////////////////////
#include "vicmain_c"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "gruen.h"

#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigMission.h"

#include <string.h>

/* buffer sizes in main program */
#define MAX_INPUTS 1000
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS
#define MAX_LEFT_AREA 101
#define MAX_RIGHT_AREA 151
#define MAX_TPS_PER_PAIR 10	/* limit on CULL parameter */
#define MAX_AREAS 2000

//Assumes "buf", "MAX_RIGHT_AREA" local variables. 
#define BUF(i,j) (buf[(j) + (i) * MAX_RIGHT_AREA])

void generate_tiepoints(int &n_overlaps, int nids,
		PigPointingModel *pointing_in[], PigCameraModel *camera_in[],
		PigSurfaceModel *surface,
		TiePoint tiepoints[],
		PigFileModel *file_models[], int band,
		int tmpl_area, int search_area, int corner, int interactive,
		double min_quality, double max_shift, PigCoordSystem *cs,
		int add_pairs);
int read_area(double *area, int line, int samp,
		int array_height, int array_width,
		PigFileModel *file_model, int band, int height, int width);
int rotate_area(double *area, int line, int samp,
		int array_height, int array_width,
		PigFileModel *file_model, int band, int height, int width,
		double rotation,double *buf,int border);

////////////////////////////////////////////////////////////////////////
// MarsTIE program
////////////////////////////////////////////////////////////////////////

void main44()
{

    int i, j, status;
    int count, def;
	const size_t msgLen = 150;
    char msg[msgLen];

    // Inputs

    int nids;
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface_model;
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *dummy_cs;

    // Tiepoints

    TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
    TiePoint *old_tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
    if(tiepoints == NULL || old_tiepoints == NULL){
       zvmessage("Memory allocation error during TiePoints[MARS_MAX_TIEPOINTS] initialization","");
       zabend();
    }

    int n_overlaps, old_n_overlaps;
    int readtpts;

    // User Parameters

    char outfilename[150];
    int band;
    char tptlist[150];
    char mission[64], instrument[64];
    int tmpl_area, search_area;
    int interactive;
    int corner;
    double min_quality;
    double max_shift;
    int multi_edit;
    int all_pairs;
    int new_pairs;
    int add_pairs;
    int start_key;


    zvmessage("MARSTIE version 2019-12-10", "");

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, dummy_cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    // Check for an input tiepoint table

    readtpts = FALSE;

    zvp("in_tpt", tptlist, &count);
    n_overlaps = 0;
    if (count == 1) {
	n_overlaps = MARS_MAX_TIEPOINTS;
	mars_load_tiepoints(tptlist, tiepoints, n_overlaps, file_models,
			nids, NULL, PigMission::getMissionObject(mission));
	readtpts = TRUE;
	snprintf(msg, msgLen, "%d tiepoints read from %s", n_overlaps, tptlist);
	zvmessage(msg, "");
    }
 
    // get output parameter file name, and check for existence

    zvp("out_tpt", outfilename, &count);

    FILE *fp = fopen(outfilename, "r");
    if (fp != NULL) {
	fclose(fp);
	zvmessage("Output tiepoint file already exists - will not overwrite",
									"");
	zabend();
    }

    // get parameter overrides if any

    zvp("BAND", &band, &count);

    corner = zvptst("CORNER");
    interactive = zvptst("INTERACT");
    multi_edit = zvptst("MULTI_EDIT");
    all_pairs = zvptst("ALL_PAIRS");
    new_pairs = zvptst("NEW_PAIRS");
    add_pairs = zvptst("ADD_PAIRS");
    zvp("START_KEY", &start_key, &count);

    zvp("TEMPLATE", &tmpl_area, &count);
    zvp("SEARCH", &search_area, &count);

    zvparmd("QUALITY", &min_quality, &count, &def, 1, 0);
    zvparmd("MAXSHIFT", &max_shift, &count, &def, 1, 0);

    if (interactive)
	zvmessage("Interactive mode", "");
    else
	zvmessage("Batch mode", "");

    // Create surface model based on the first input.  Looks at user
    // parameters.

    surface_model = PigSurfaceModel::create(file_models[0]);

    cs = surface_model->getCoordSystem();

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in, camera_in, file_models,
			homogeneous_inputs, mission, instrument);

    if (nids == 1) {
	zvmessage("Only one input file; nothing to do!", "");
	return;
    }

    // If we didn't read tiepoints from a file, generate them now

    if (!readtpts || add_pairs) {
	int save_n = n_overlaps;
	generate_tiepoints(n_overlaps, nids, pointing_in, camera_in,
		surface_model, tiepoints, file_models, band,
		tmpl_area, search_area, corner, interactive,
		min_quality, max_shift, cs, add_pairs);
	if (add_pairs) {
	    new_pairs = TRUE;		// pretend like new_pairs, now
	    snprintf(msg, msgLen, "%d new tiepoints; %d total", n_overlaps-save_n,
					n_overlaps);
	    zvmessage(msg, "");
	}

	if (n_overlaps < 1 && !(interactive && multi_edit && all_pairs))
	    zabend();		// msg issued already
    }

    // edit the tiepoints interactively, if requested... original style

    if (interactive && !multi_edit) {
        for (i=0; i < n_overlaps; i++) {
	    if (tiepoints[i].quality >= min_quality)
	        continue;		// skip over high quality points

	    snprintf(msg, msgLen, "Editing tiepoint %d of %d, left image=%d, right image=%d, quality=%6.2f",
		i+1, n_overlaps,
		tiepoints[i].left_image+1, tiepoints[i].right_image+1,
		tiepoints[i].quality);
	    zvmessage(msg, "");

	    int status = mars_edit_tiepoint(i, n_overlaps, tiepoints,
		    file_models);
	    int status2;
	    if (zvptst("OLD"))		// save every time...
		status2 = mars_save_tiepoints(outfilename,tiepoints,n_overlaps);
	    else
	        status2 = mars_save_tiepoints(outfilename, tiepoints,
			n_overlaps, file_models, nids, start_key, cs);
	    if (status2 != 0) {
		snprintf(msg, msgLen, "Error saving tipoints!! code=%d", status2);
		zvmessage(msg, "");
	    }
	    if (status > 0) {
	        zvmessage("session aborted, saving tiepoints", "");
	        return;		// exit from program
	    }
        }

	// remove points flagged as bad

	mars_remove_bad_points(tiepoints, n_overlaps, 0.0);
    }

    // edit the tiepoints interactively, if requested... new, multi-point style.
    // Go through all combinations of images, pull out all tiepoints for each
    // combo, and present all the tiepoints for the pair at once for editing.
    // Use the results to build a new table.

    if (interactive && multi_edit) {

	// Save all original tiepoints, and clear out the array

	for (i=0; i < n_overlaps; i++) {
	    old_tiepoints[i] = tiepoints[i];
	}
	old_n_overlaps = n_overlaps;
	n_overlaps = 0;

	// Now go through each pair, extract all points for the pair, and
	// store them in the temporary

	int special_exit = FALSE;

	for (int img1 = 0; img1 < nids-1; img1++) {
	    for (int img2 = img1+1; img2 < nids; img2++) {

		TiePoint *temp_tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS_PER_EDIT];
                if(temp_tiepoints == NULL){
                  zvmessage("Memory allocation failed during temp_tiepoints[MARS_MAX_TIEPOINTS_PER_EDIT]","");
                  zabend();
                }
		int n_temp_overlaps = 0;
		int non_interactive_ties = 0;	// how many haven't been edited

		for (i=0; i < old_n_overlaps; i++) {
		    if ((old_tiepoints[i].left_image == img1 &&
			 old_tiepoints[i].right_image == img2) ||
			(old_tiepoints[i].left_image == img2 &&
			 old_tiepoints[i].right_image == img1)) {

			// Found a match.  Remove it from old and add to temp

			if (n_temp_overlaps > MARS_MAX_TIEPOINTS_PER_EDIT) {
			    zvmessage("Too many tiepoints in one pair!","");
			    zabend();
			}
			temp_tiepoints[n_temp_overlaps++] = old_tiepoints[i];
			if (!old_tiepoints[i].interactive)
			    non_interactive_ties++;
			for (j=i; j < old_n_overlaps-1; j++)
			    old_tiepoints[j] = old_tiepoints[j+1];
			old_n_overlaps--;
			i--;
		    }
		}

		// Now, edit these tiepeoints.  If there aren't any, skip the
		// pair, unless we're forcing all pairs.  If special exit
		// status is selected, just copy the rest... no editing.
		// Finally, if new_pairs is on, we only edit pairs for which
		// a tiepoint has the interactive flag set to false.

		int edited = FALSE;
		if ((n_temp_overlaps != 0 || all_pairs) && !special_exit &&
		    !(new_pairs && (non_interactive_ties == 0))) {

		    snprintf(msg, msgLen, "Editing tiepoints for image pair %d, %d: %d points",
					img1+1, img2+1, n_temp_overlaps);
		    zvmessage(msg, "");

		    special_exit = mars_edit_tiepoints(n_temp_overlaps,
				img1, img2,
				temp_tiepoints, file_models);
		    snprintf(msg, msgLen, "Have %d tiepoints for this pair now",
							n_temp_overlaps);
		    zvmessage(msg, "");
		    edited = TRUE;
		}

		// Copy the results 

		for (i=0; i < n_temp_overlaps; i++)
		    tiepoints[n_overlaps++] = temp_tiepoints[i];

		// Save the results so far... but only if edited interactively
		// (otherwise it gets REALLY slow)

		// Add the ramining old ties back in temporarily, for the save.
		// They'll get ignored later, since we don't change n_overlaps.

		for (i=0; i < old_n_overlaps; i++)
		    tiepoints[n_overlaps+i] = old_tiepoints[i];
		if (edited) {
		    if (zvptst("OLD"))
		        status = mars_save_tiepoints(outfilename,
					tiepoints, n_overlaps + old_n_overlaps);
		    else
		        status = mars_save_tiepoints(outfilename, tiepoints,
			    n_overlaps + old_n_overlaps,
			    file_models, nids, start_key, cs);
		    if (status != 0) {
		        snprintf(msg, msgLen, "Error saving tipoints!! code=%d", status);
		        zvmessage(msg, "");
		    }
		}

                delete[] temp_tiepoints;
	    }
	}

	if (old_n_overlaps != 0) {
	    snprintf(msg, msgLen, "INTERNAL ERROR!!  %d tiepoints are left after the multi-editing loop.", old_n_overlaps);
	    zvmessage(msg, "");
	    zvmessage("WARNING!!  These tiepoints will be discarded:", "");
	    mars_print_tiepoint_list(old_tiepoints, old_n_overlaps);
	    zvmessage("END OF WARNING list", "");
	}

    }

    // save tiepoints

    if (zvptst("OLD"))
	status = mars_save_tiepoints(outfilename, tiepoints, n_overlaps);
    else
	status = mars_save_tiepoints(outfilename, tiepoints, n_overlaps,
			file_models, nids, start_key, cs);
    if (status != 0) {
	snprintf(msg, msgLen, "Error saving tipoints!! code=%d", status);
	zvmessage(msg, "");
    }

    // print out tiepoints

    zvmessage("Final tiepoint list (Camera coordinates)", "");
    mars_print_tiepoint_list(tiepoints, n_overlaps);


    // determine which pictures are represented (when present[i] is 1)

    int present[MAX_INPUTS];

    for (i=0; i < nids; i++)
	present[i] = FALSE;
    for (i=0; i < n_overlaps; i++) {
	present[tiepoints[i].left_image] = TRUE;
	present[tiepoints[i].right_image] = TRUE;
    }

    for (i=0; i < nids; i++) {
	if (!present[i]) {
	    snprintf(msg, msgLen, "Input image %d not represented by tiepoints", i+1);
	    zvmessage(msg, "");
	}
    }

    delete[] tiepoints;
    delete[] old_tiepoints;

}

////////////////////////////////////////////////////////////////////////
// Generate a set of tiepoints to use, if we didn't read them from a file.
////////////////////////////////////////////////////////////////////////

void generate_tiepoints(int &n_overlaps, int nids,
		PigPointingModel *pointing_in[], PigCameraModel *camera_in[],
		PigSurfaceModel *surface,
		TiePoint tiepoints[],
		PigFileModel *file_models[], int band,
		int tmpl_area, int search_area, int corner, int interactive,
		double min_quality, double max_shift, PigCoordSystem *cs,
		int add_pairs)
{

    int i, j, k, ii=0;
	const size_t msgLen = 150;
    char msg[msgLen];

    double percent;
    int limits;
    int mode;
    int kl = -1000;
    int kr = -1000;
    int ind = 0;

    int nlw, nsw, nlw2, nsw2, candidate_tiepoints,n,m=0,nbest,border;
    int count;
    int tptinit[MAX_AREAS][2]; // 0=sample, 1=line
    float busy_threshold;

    int left_line, left_samp, right_line, right_samp;
    double line2=0,samp2=0,line_offset, samp_offset;
    double quality=0.,business,busy[1000],rline_save[1000],rsamp_save[1000];
    double business_limit;

    double line_coef_limits[3][2],samp_coef_limits[3][2];
    double line_coef[3],samp_coef[3];
    double line_temp[3],samp_temp[3];

    double left_template[MAX_LEFT_AREA][MAX_LEFT_AREA];
    double right_template[MAX_RIGHT_AREA][MAX_LEFT_AREA];
    double correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];

    double *angles = new (std::nothrow) double[MARS_MAX_TIEPOINTS];
    double *angle = new (std::nothrow) double[MARS_MAX_TIEPOINTS];
    if(angles == NULL || angle == NULL){
       zvmessage("Memory allocation for angles[MARS_MAX_TIEPOINTS] and/or angle[MARS_MAX_TIEPOINTS] failed","");
       zabend();
    }
    
    float sep_angle;
    double sep_cos;

    int start_overlaps = 0;
    if (add_pairs)
	start_overlaps = n_overlaps;
    else
        n_overlaps = 0;

    // compute max length/width of correlations

    if (tmpl_area % 2 == 0)
	tmpl_area += 1;				// make sure it's odd
    if (search_area % 2 == 0)
	search_area += 1;				// make sure it's odd

    nlw = tmpl_area;
    nsw = tmpl_area;
    nlw2 = search_area;
    nsw2 = search_area;

    percent = 100.;
    limits = 10000;
    mode = 0;
    nbest=10; // # of initial tiepoints to keep. See dx[10],dy[10],tpid[10]
    
    zvp("BORDER", &border, &count);
    zvp("BUSY", &busy_threshold, &count);

    zvp("SEP_ANGLE", &sep_angle, &count);	// max sep angle for frames
    sep_cos = cos(PigDeg2Rad(sep_angle));

    // ref or left picture loop

    for (i=0; i < nids; i++) {

	// ref picture candidate tiepoint locations in camera coordinates

	int sl, ss, el, es;
	double sl_d, ss_d, el_d, es_d;
	file_models[i]->getImageBorders(sl_d, ss_d, el_d, es_d);
	// conversion to int, above returns doubles.
	ss=(int)ss_d;
	sl=(int)sl_d;
	el=(int)el_d;
	es=(int)es_d;
	/* int nl = */ file_models[i]->getNL();
	/* int ns = */ file_models[i]->getNS();

        // top line
        k=0;
        for(j = ss+border+nsw/2; j < es-border-nsw/2; j=j+nsw) {
          tptinit[k][0]=j;
          tptinit[k][1]=sl+border+nlw/2;
          k++;
	  if (k > MAX_AREAS) {
	    zvmessage("k got too big\n", "");
	    zabend();
	  }
        }

        // bottom line
        for(j = ss+border+nsw/2; j < es-border-nsw/2; j=j+nsw) {
          tptinit[k][0]=j;
          tptinit[k][1]=el-border-nlw/2;
          k++;
	  if (k > MAX_AREAS) {
	    zvmessage("k got too big\n", "");
	    zabend();
	  }
        }

        // left column
        for(j = sl+border+nlw+nlw/2; j < el-border-nlw-nlw/2; j=j+nlw) {
          tptinit[k][0]=ss+border+nsw/2;
          tptinit[k][1]=j;
          k++;
	  if (k > MAX_AREAS) {
	    zvmessage("k got too big\n", "");
	    zabend();
	  }
        }
        
        // right column
        for(j = sl+border+nlw+nlw/2; j < el-border-nlw-nlw/2; j=j+nlw) {
          tptinit[k][0]=es-border-nsw/2;
          tptinit[k][1]=j;
          k++;
	  if (k > MAX_AREAS) {
	    zvmessage("k got too big\n", "");
	    zabend();
	  }
        }
        candidate_tiepoints=k;

	// open left image

	if (kl != i) {
	    if (kl != -1000)
		file_models[kl]->closeFile();
	    kl = i;
	    if (kr == kl) {
		kr = -1000;		// make sure it's not closed twice
	    }
	    else {
		zvopen(file_models[kl]->getUnit(), "OP", "READ",
				"U_FORMAT", "DOUB", "OPEN_ACT", "SA", NULL);
		file_models[kl]->setFileOpen(TRUE);
	    }
	}


	for (j=0; j < nids; j++) {		// right picture loop

	    // Skip pairs that already exist

	    if (add_pairs) {
		int match = FALSE;
		// Only look up to start_overlaps, which is the number we
		// had at the beginning (i.e. what came in from the file)
		for (int kk = 0; kk < start_overlaps; kk++) {
		    if (    tiepoints[kk].left_image == i &&
			    tiepoints[kk].right_image == j) {
			match = TRUE;
			break;		// Match; skip the rest
		    }
		    if (    tiepoints[kk].left_image == j &&
			    tiepoints[kk].right_image == i) {
			match = TRUE;
			break;		// Match the other way; skip the rest
		    }
		}

		if (match)
		    continue;		// skip this tiepoint pair
	    }

	    if (i != j) {

		// If angle between the images is too great, skip this pair
		//!!!! Distance is arbitrary, should be parameterized!!!!

		if ((pointing_in[i]->getCameraOrientation(cs) %
		pointing_in[j]->getCameraOrientation(cs)) < sep_cos)
		    continue;
		    
		for (k=0; k < candidate_tiepoints; k++) {
		    busy[k]=0.0;

		    // Project 2 left points into right image, second is templ. center

		    double samp,samp1;
		    double line,line1;
		    PigPoint origin;
		    PigVector look;
		    PigPoint surf_pt;
		    int hits;
		    
		    samp = tptinit[k][0] - 10.0;
		    line = tptinit[k][1];
		    camera_in[i]->LStoLookVector(line, samp, origin, look, cs);
		    hits=surface->intersectRay(origin, look, surf_pt);
		    camera_in[j]->XYZtoLS(surf_pt, (hits<=0), &line1, &samp1, cs);

		    samp = tptinit[k][0];
		    line = tptinit[k][1];
		    camera_in[i]->LStoLookVector(line, samp, origin, look, cs);
		    hits=surface->intersectRay(origin, look, surf_pt);
		    camera_in[j]->XYZtoLS(surf_pt, (hits<=0), &line, &samp, cs);
		    rline_save[k]=line;
		    rsamp_save[k]=samp;

                    // rotation of right image rel to left image, +=countercl
                    angles[k]=-atan2(line-line1,samp-samp1);

		    // Check if the entire corr. area is inside the image
		    if ((file_models[j]->testPixelLocation(
					line - nlw2/2,
					samp - nsw2/2) == 0) &&
			(file_models[j]->testPixelLocation(
					line - nlw2/2,
					samp + nsw2/2) == 0) &&
			(file_models[j]->testPixelLocation(
					line + nlw2/2,
					samp - nsw2/2) == 0) &&
			(file_models[j]->testPixelLocation(
					line + nlw2/2,
					samp + nsw2/2) == 0)) {
					
	                // read left image template
	                ind = read_area(&left_template[0][0],
	                  tptinit[k][1],tptinit[k][0],
			  MAX_LEFT_AREA, MAX_LEFT_AREA,
			  file_models[kl], band, nlw, nsw);

                        // Compute the business of the area.
                        business=0.0;
                        for(n=0; n < nsw-1; n++){
                          for(m=0; m < nlw-1; m++){
                            business += fabs(left_template[m][n]-
                            left_template[m+1][n]) + fabs(
                            left_template[m][n]-left_template[m][n+1]);
                          }
                        }
                        business=business/((nsw-1)*(nlw-1));
                        //printf("business= %f %d %d\n",business,i+1,j+1);
                        if(business < busy_threshold)business=0.0; //clear point
                        busy[k]=business; // priority of each candidate tiepoint area
                    }   // inside image
                }       // candidate loop
                
                // select the nbest best candidates based upon business
                for(n=0; n < nbest; n++){
                  business_limit=-1.0;
		  for (k=0; k < candidate_tiepoints; k++) {
		    if(busy[k] > business_limit){
		      business_limit=busy[k];
		      m=k;
		    }
		  }
		  busy[m]=-busy[m]; // the best are < 0, all others are >= 0
		}
                
		// store initial search locations
		for (k=0; k < candidate_tiepoints; k++) {
                    if(busy[k] < 0.0){

			if (n_overlaps >= MARS_MAX_TIEPOINTS) {
			    zvmessage("tpt buffer too small", "");
			    zabend();
			}
			tiepoints[n_overlaps].left_sample = tptinit[k][0];
			tiepoints[n_overlaps].left_line = tptinit[k][1];
			tiepoints[n_overlaps].right_sample = rsamp_save[k];
			tiepoints[n_overlaps].right_line = rline_save[k];
			tiepoints[n_overlaps].left_image = i;
			tiepoints[n_overlaps].right_image = j;
			tiepoints[n_overlaps].corrected_sample = 0.0;
			tiepoints[n_overlaps].corrected_line = 0.0;
			tiepoints[n_overlaps].quality = 0.0;
			tiepoints[n_overlaps].active = TRUE;
			tiepoints[n_overlaps].type = TIEPOINT_TRADITIONAL;
			tiepoints[n_overlaps].cs = NULL;
			angle[n_overlaps]=angles[k];
			n_overlaps++;
		    }
		}	// candidate loop
	    }		// i != j
	}		// right picture loop
    }		 	// left picture loop

    if (n_overlaps < 1) {
	zvmessage("No candidate overlaps found", "");
    }
    snprintf(msg,msgLen,"%d candidate tiepoints located before correlation",n_overlaps);
    zvmessage(msg, "");
    if (add_pairs) {
	snprintf(msg,msgLen, "%d of those were loaded from the input file (%d new)",
		start_overlaps, n_overlaps-start_overlaps);
	zvmessage(msg, "");
    }

    // Refine tiepoints, are in camera coordinates!

    for (i=start_overlaps; i < n_overlaps; i++) {
	left_line = (int)tiepoints[i].left_line;
	left_samp = (int)tiepoints[i].left_sample;
	right_line = (int)(tiepoints[i].right_line + 0.5);
	right_samp = (int)(tiepoints[i].right_sample + 0.5);

	// open left image

	if (kl != tiepoints[i].left_image) {
	    if (kl != -1000)
		file_models[kl]->closeFile();
	    kl = tiepoints[i].left_image;
	    if (kr == kl) {
		kr = -1000;		// make sure it's not closed twice
	    }
	    else {
		zvopen(file_models[kl]->getUnit(), "OP", "READ",
				"U_FORMAT", "DOUB", "OPEN_ACT", "SA", NULL);
		file_models[kl]->setFileOpen(TRUE);
	    }
	}

	// open right image

	if (kr != tiepoints[i].right_image) {
	    if (kr != -1000)
		file_models[kr]->closeFile();
	    kr = tiepoints[i].right_image;
	    if (kr == kl) {
		kl = -1000;		// make sure it's not closed twice
	    }
	    else {
		zvopen(file_models[kr]->getUnit(), "OP", "READ",
				"U_FORMAT", "DOUB", "OPEN_ACT", "SA", NULL);
		file_models[kr]->setFileOpen(TRUE);
	    }
	}

	// read left image template

	ind = rotate_area(&left_template[0][0], left_line, left_samp,
			MAX_LEFT_AREA, MAX_LEFT_AREA,
			file_models[kl], band, nlw, nsw,angle[i],
			&right_template[0][0],border);

	// read right search area

	if (ind == 0) {
	ind = read_area(&right_template[0][0], right_line, right_samp,
			MAX_RIGHT_AREA, MAX_RIGHT_AREA,
			file_models[kr], band, nlw2, nsw2);
	}

	if (ind == 0) {		// successfully read
 
	    // set initial coefficient values

	    line_coef[0]=0.;
	    line_coef[1]=1.;
	    line_coef[2]=0.;
	    samp_coef[0]=1.;
	    samp_coef[1]=0.;
	    samp_coef[2]=0.;
 
	    // correct candidate tiepoints by correlation

	    ind = gruen(&left_template[0][0], nlw, nsw, MAX_LEFT_AREA,
		      &right_template[0][0], nlw2, nsw2, MAX_RIGHT_AREA,
		      &correl[0][0], &line_offset, &samp_offset,
		      line_coef, samp_coef, line_coef_limits, samp_coef_limits,
		      line_temp, samp_temp, percent, limits, &quality, mode);

	    if (ind == 0) {
 
		// compute right image coordinates from offsets
		line2 = right_line + line_offset;
		samp2 = right_samp + samp_offset;
	    }
	}

	if (ind == 0) {
	    tiepoints[i].corrected_sample = samp2;	// in right image
	    tiepoints[i].corrected_line = line2;
	    tiepoints[i].quality = quality;		// quality 0 to 1
	}
	else {
	   tiepoints[i].quality = 0.0;			// bad flag
	}
    }

    delete[] angles; 
    delete[] angle;
    

    // In batch edit out the bad ones based upon poor correlation value

    if (!interactive) {
	int old_n = n_overlaps;
	mars_remove_bad_points(tiepoints, n_overlaps, min_quality);
	snprintf(msg, msgLen, "%d points rejected for low correlation",
						old_n - n_overlaps);
	zvmessage(msg, "");
    }

    // edit out the bad ones based upon line shift
    k = start_overlaps;
    for (i=start_overlaps; i < n_overlaps; i++) {
	if (fabs(tiepoints[i].right_line - tiepoints[i].corrected_line)
			< max_shift) {
	    tiepoints[k] = tiepoints[i];
	    k++;
	}
    }
    snprintf(msg,msgLen, "%d points rejected for excessive line shift",n_overlaps-k);
    zvmessage(msg, "");
    n_overlaps = k;

    // edit out the bad ones based upon sample shift
    k = start_overlaps;
    for (i=start_overlaps; i < n_overlaps; i++) {
	if (fabs(tiepoints[i].right_sample - tiepoints[i].corrected_sample)
			< max_shift) {
	    tiepoints[k] = tiepoints[i];
	    k++;
	}
    }
    snprintf(msg,msgLen,"%d points rejected for excessive sample shift", n_overlaps-k);
    zvmessage(msg, "");
    n_overlaps = k;

    // print out tiepoints

    zvmessage("Initial tiepoint list (Camera coordinates)", "");
    mars_print_tiepoint_list(tiepoints, n_overlaps);

    /////////////////////////
    // locate points with excessive shifts based upon the statistics
    // of all the other points in that pair. By pair we mean n,m or m,n
    // but not both n,m and m,n together.
    ////////////////////////
    double dx[10],dy[10],sumdx,sumdy,x_mean,y_mean,sdev_x,sdev_y;
    int tpid[10],nn;
    float sigma,worst;
    
    zvp("SIGMA", &sigma, &count);	// stan. dev. factor
    for (i=0; i < nids; i++) {
      for (j=0; j < nids; j++){
        n=0;
        // extract points for pair i,j
        for (k=start_overlaps; k < n_overlaps; k++){
          if((tiepoints[k].left_image == i && tiepoints[k].right_image == j) &&
            (tiepoints[k].quality > 0.) && 
            (i != j)){
            dx[n]=fabs(tiepoints[k].right_sample - 
                    tiepoints[k].corrected_sample);
            dy[n]=fabs(tiepoints[k].right_line - 
                    tiepoints[k].corrected_line);
            tpid[n]=k; // remember the index k
            n++;
          }
        }
        m=n;
        restart:
        if(m >= 5){
          // compute the mean and standard deviation for the tiepoint shifts
          sumdx=0.0;
          sumdy=0.0;
          nn=0;
          for(k=0; k < n; k++){
            if(tpid[k] > -1){
              sumdx+= dx[k];
              sumdy+= dy[k];
              nn++;
            }
          }
          x_mean=sumdx/(double)nn; // mean
          y_mean=sumdy/(double)nn;
          sumdx=0.0;
          sumdy=0.0;
          for(k=0; k < n; k++){
            if(tpid[k] > -1){
              sumdx+= pow(dx[k]-x_mean,2);
              sumdy+= pow(dy[k]-y_mean,2);
            }
          }
          sdev_x=sqrt(sumdx/(double)nn); // standard deviation
          sdev_y=sqrt(sumdy/(double)nn);
          // find the worst point
          worst=-1.0;
          for(k=0; k < n; k++){
            if(tpid[k] > -1){
              if(dx[k] > worst){
                worst=dx[k];
                ii=k;
              }
              if(dy[k] > worst){
                worst=dy[k];
                ii=k;
              }
            }
          }
          if((worst > sigma*sdev_x) || (worst > sigma*sdev_y)){
            tiepoints[tpid[ii]].quality = 0.;
            tpid[ii]=-1;  // mark as bad
            m--; // reduce number of good points
            goto restart;
          }
        }
      }  // j loop
    }    // i loop
    // edit out the bad ones with quality=0
    k = start_overlaps;
    for (i=start_overlaps; i < n_overlaps; i++) {
      if (tiepoints[i].quality > 0.) {
          tiepoints[k] = tiepoints[i];
          k++;
      }
    }
    snprintf(msg,msgLen,"%d points rejected by sigma test", n_overlaps-k);
    zvmessage(msg, "");
    n_overlaps = k;
    // print out tiepoints
    zvmessage("Tiepoint list after sigma test", "");
    mars_print_tiepoint_list(tiepoints, n_overlaps);

    
    ////////////////////////
    // Weed out extra tiepoints so we keep "CULL" per image pair.
    // CULL = 0 means to keep everything.
    ////////////////////////

    // Note:  these are static to avoid blowing the stack size on Linux...
    static double best_quality[MAX_INPUTS][MAX_INPUTS][MAX_TPS_PER_PAIR];
    static int best_index[MAX_INPUTS][MAX_INPUTS][MAX_TPS_PER_PAIR];

    int ncull;
    int replaced;

    zvp("CULL", &ncull, &count);		// # of points 

    if (ncull != 0) {				// no culling if 0

	for (i=0; i < ncull; i++) {
	    for (n=0; n < nids; n++) {
		for (m=0; m < nids; m++) {
		    best_quality[m][n][i] = 0.0;
		    best_index[m][n][i] = -1;
		}
	    }
	}

	// For each tiepoint, check it against the tables to see if it should
	// be kept or not.

	for (i=start_overlaps; i < n_overlaps; i++) {	// check each tiepoint
	    m = tiepoints[i].left_image;
	    n = tiepoints[i].right_image;
	    if (n < m) {			// nm and mn are identical
		int temp = m;
		m = n;
		n = temp;
	    }
	    replaced = FALSE;
	    for (j=0; j < ncull; j++) {		// check against # to keep
		if (best_quality[m][n][j] < tiepoints[i].quality) {

		    // Delete tiepoint at end of list

		    if (best_index[m][n][ncull-1] != -1)
			tiepoints[best_index[m][n][ncull-1]].quality = 0.0;

		    // Replace old entry in table, move everything else down

		    for (k = j; k < ncull-1; k++) {
			best_quality[m][n][k+1] = best_quality[m][n][k];
			best_index[m][n][k+1] = best_index[m][n][k];
		    }

		    // Add this new entry in

		    best_quality[m][n][j] = tiepoints[i].quality;
		    best_index[m][n][j] = i;
		    replaced = TRUE;

		    break;				// done with j loop
		}
	    }

	    if (!replaced)	// this was worse than all in table, so nuke it
		tiepoints[i].quality = 0.0;

	}					// next tiepoint
    }

    // Remove tiepoints with 0 quality

    mars_remove_bad_points(tiepoints, n_overlaps, 0.0);

    // print out tiepoints		//!!!!

    snprintf(msg, msgLen, "Culled tiepoint list (Camera coordinates) (%d)", n_overlaps);		//!!!!
    zvmessage(msg, "");		//!!!!
    mars_print_tiepoint_list(tiepoints, n_overlaps);		//!!!!


}

////////////////////////////////////////////////////////////////////////
// Reads an area from the file into a double-precision buffer.  Assumes
// that the file is open with U_FORMAT, DOUB.  array_width is the physical
// size of the array (sample dimension) which is being read into - it
// does not have to match the area being read.  Ditto for array_height (it is
// the physical size of the first dimension, used only for error checking).
// width/height are the sizes of the area to be read.
// Important:  line/samp are the *middle* of the area to be read, not the
// edges!  And they are in camera coordinates; they're converted to image
// coords via the x/y_offset in FileInfo.
//
// Returns 0 for success, 1 for error.
////////////////////////////////////////////////////////////////////////

int read_area(double *area, int line, int samp,
		int array_height, int array_width,
		PigFileModel *file_model, int band, int height,
	        int width)
{

    // Convert line/samp to physical (0-based) coords, and the upper-left edge

    int start_line = (int)((line - file_model->getYOffset()) - height/2);
    int start_samp = (int)((samp - file_model->getXOffset()) - width/2);

    if (start_line < 0) return 1;
    if (start_samp < 0) return 1;
    if (start_line+height-1 >= file_model->getNL()) return 1;
    if (start_samp+width-1 >= file_model->getNS()) return 1;
    if (height > array_height || width > array_width) return 1;

    for (int i=0; i < height; i++) {
	zvread(file_model->getUnit(), area+(i*array_width),
		"LINE", start_line + i + 1,
		"SAMP", start_samp + 1,
		"BAND", band, "NSAMPS", width, NULL);
    }
    return 0;
}

int rotate_area(double *area, 
                int line, int samp,
		int array_height, int array_width,
		PigFileModel *file_model, int band, int height,
	        int width,double rotation,
	        double *buf, int border)
  {


  int ind,i,j,ii,jj;
  double costh,sinth,xcenbuf,ycenbuf,xcenarea,ycenarea,x,y,wr,wl,wt,wb;
  double top,bot,rot;
  
    // Convert line/samp to physical (0-based) coords, and the upper-left edge
    int start_line = (int)((line - file_model->getYOffset()) - height/2);
    int start_samp = (int)((samp - file_model->getXOffset()) - width/2);

    // read oversized template into buf
    ind=0;
    if (start_line-border < 0){
     ind=1;
     printf("start_line-border %d\n",start_line-border);
    }
    if (start_samp-border < 0){
     ind=1;
     printf("start_samp-border %d\n",start_samp-border);
    }
    if (start_line+height-1+border >= file_model->getNL()){
     ind=1;
     printf("start_line+height %d\n",start_line+height-1+border);
    }
    if (start_samp+width-1+border >= file_model->getNS()){
     ind=1;
     printf("start_samp+width %d\n",start_samp+width-1+border);
    }
    if(ind == 1) {
      printf("reading outside image\n");
      return 1;
    }
    if (height+2*border > MAX_RIGHT_AREA || 
        width+2*border > MAX_RIGHT_AREA){
          printf(" template+2*border > MAX_RIGHT_AREA\n");
          zabend();
    }
    for (int i=0; i < height+2*border; i++) {
	zvread(file_model->getUnit(), buf+(i*MAX_RIGHT_AREA),
		"LINE", start_line + i + 1 -border,
		"SAMP", start_samp + 1 -border,
		"BAND", band, "NSAMPS", width+2*border, NULL);
    }
    
    // rotate sub area from buf into area, + rotation is counterclockwise
    rot=rotation;
    costh=cos(rot);
    sinth=sin(rot);
    xcenbuf=(width-1)/2+border;
    ycenbuf=(height-1)/2+border;
    xcenarea=(width-1)/2;
    ycenarea=(height-1)/2;
            
    for (j=0; j < height; j++){
      for (i=0; i < width; i++){
        x=(i-xcenarea)*costh-(j-ycenarea)*sinth+xcenbuf;
        y=(j-ycenarea)*costh+(i-xcenarea)*sinth+ycenbuf;
        ii=(int)x;
        jj=(int)y;
        wr=x-ii;
        wl=1.0-wr;
        wb=y-jj;
        wt=1.0-wb;
        top=wl*BUF(jj, ii)+wr*BUF(jj, ii+1);
        bot=wl*BUF(jj+1, ii)+wr*BUF(jj+1, ii+1);
	area[(i) + (j) * MAX_LEFT_AREA] = top*wt+bot*wb;
        //area[j][i]=top*wt+bot*wb;
      }
    }
        
    return 0;
}

