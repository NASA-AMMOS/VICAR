/** Source code for:      disp2tp.c
**/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string>
#include <vector>

#include "vicmain_c"
#include "defines.h"
#include <zmabend.h>
#include <zifmessage.h>

#include "mars_support.h"
#include "mars_tiepoints.h"
#include "PigMission.h"
#include "PigFileModel.h"

void main44(void)
{   
  int dspUnit, xyzUnit=0, parmCt, parmDf;
  float *dispLineBuf = NULL, *dispSampBuf = NULL;
  float *xyzXBuf = NULL, *xyzYBuf = NULL, *xyzZBuf = NULL;
  int line, samp;
  char outTiePath[1000];
  int period;
  int tiepointCount;

  /* adapted from marsdispwarp.cc and marstie.cc */
#define MAX_INPUTS 1000
#define MAX_NS 2048
#define MAX_NL 65536

  char mission[64], instrument[64];
  PigFileModel *file_models[MAX_INPUTS];
  PigFileModel *rhs_file_models[MAX_INPUTS];
  PigFileModel *xyz_file_models[MAX_INPUTS];
  PigFileModel *concat_file_models[2 * MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  int homogeneous_inputs = TRUE;
  PigCoordSystem *cs;
  TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
  if(tiepoints == NULL)
       zmabend("Memory allocation error during TiePoints[MARS_MAX_TIEPOINTS] initialization");
  int n_ties = MARS_MAX_TIEPOINTS;
  char unique_id[33];
  char dsp_unique_id[33];
  char rhs_unique_id[33];

  zifmessage("disp2tp version Thu Sep 25 2014");

  if (1 != zvparm ("out", outTiePath, &parmCt, &parmDf, 1, 999))
    zmabend("failure reading output tiepoint file name");

  // parse cnt parm
  if (1 != zvp("cnt", &tiepointCount, &parmCt))
    zmabend("failure reading tiepoint count");

  if (tiepointCount <= 0)
    zmabend("tiepoint count must be positive");

  // parse tiepoint type
  int tpType = zvptst("MISS_DISTANCE")? TIEPOINT_MISS_DISTANCE : TIEPOINT_TRADITIONAL;

  // setup INP dsp files
  int nids;
  mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
	     mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

  printf("%s:%d: INP count: %d\n", __FILE__, __LINE__, nids);

  // setup RHS files
  // adapted from marsslope.com
  char **rhs_filenames = new char *[MAX_INPUTS];
  if (rhs_filenames == NULL)
    zmabend("failure allocating rhs filenames");

  int rhs_nids;
  mars_get_filelist("RHS", rhs_nids, rhs_filenames, MAX_INPUTS, FALSE);

  printf("%s:%d: RHS count: %d\n", __FILE__, __LINE__, rhs_nids);

  if (nids != rhs_nids)
    zmabend("INP count must equal RHS count");

  for (int i = 0; i < rhs_nids; i++) 
    {
      rhs_file_models[i] = PigFileModel::create(rhs_filenames[i]);
      if (rhs_file_models[i] == NULL) 
        {
    const size_t msgLen = 150;
	  char msg[msgLen];
	  snprintf(msg,msgLen, "Unable to create file model for RHS input %d", i);
	  zmabend(msg);
        }
    }

  // concatenate dsp and rhs file models for tiepoint loader
  for (int i = 0; i < nids; ++i)
    concat_file_models[i] = file_models[i];
  for (int i = 0; i < nids; ++i)
    concat_file_models[nids + i] = rhs_file_models[i];

  // setup XYZ files
  char **xyz_filenames = new char *[MAX_INPUTS];
  if (xyz_filenames == NULL)
    zmabend("failure allocating xyz filenames");

  int xyz_nids;
  mars_get_filelist("XYZ", xyz_nids, xyz_filenames, MAX_INPUTS, FALSE);

  printf("%s:%d: XYZ count: %d\n", __FILE__, __LINE__, xyz_nids);

  if (nids < xyz_nids)
    zmabend("There are more XYZ files than DSP files");

  for (int i = 0; i < xyz_nids; i++) 
    {
      xyz_file_models[i] = PigFileModel::create(xyz_filenames[i]);
      if (xyz_file_models[i] == NULL) 
        {
    const size_t msgLen = 150;
	  char msg[msgLen];
	  snprintf(msg, msgLen, "Unable to create file model for XYZ input %d", i);
	  zmabend(msg);
        }
    }

  // setup TIE files
  char **tie_filenames = new char *[MAX_INPUTS];
  if (tie_filenames == NULL)
    zmabend("failure allocating tie filenames");

  int tie_nids;
  mars_get_filelist("INPTIE", tie_nids, tie_filenames, MAX_INPUTS, FALSE);

  printf("%s:%d: TIE count: %d\n", __FILE__, __LINE__, tie_nids);

  if (nids < tie_nids)
    zmabend("There are more TIE files than DSP files");

  std::vector<TiePoint> tps;

  int tpCnt = 0;

  // iterate through dsp files
  for (int dspI = 0; dspI < nids; ++dspI) {
    printf("%s:%d: processing dsp %d/%d\n", __FILE__, __LINE__, dspI, nids);

    // open DSP for reading
    dspUnit = file_models[dspI]->getUnit();
    if (file_models[dspI]->isFileOpen())
      file_models[dspI]->closeFile();

    if (1 != zvopen(dspUnit,"OP","READ", NULL))
      zmabend("failure opening disparity file for reading");

    int nl = file_models[dspI]->getNL();
    int ns = file_models[dspI]->getNS();
    int nb = file_models[dspI]->getNB();

    file_models[dspI]->getUniqueId(dsp_unique_id);
    rhs_file_models[dspI]->getUniqueId(rhs_unique_id);

    // open xyz if available
    if (xyz_nids > dspI) {

      xyzUnit = xyz_file_models[dspI]->getUnit();

      if (xyz_file_models[dspI]->isFileOpen())
	xyz_file_models[dspI]->closeFile();

      if (1 != zvopen(xyzUnit,"OP","READ", NULL))
	zmabend("failure opening XYZ for reading");
    }

    // read disparity map file label
    char fmtStr[100], orgStr[100];
    fmtStr[0] = '\0';
    orgStr[0] = '\0';
    if (1 != zvget (dspUnit, "FORMAT", fmtStr, NULL) ||
	1 != zvget (dspUnit, "ORG", orgStr, NULL))
      zmabend("failure reading disparity file label");

    if (nb != 2 || strcmp(fmtStr, "REAL") || strcmp(orgStr, "BSQ")) {
      fprintf(stderr, "disparity map nb %d fmtStr %s orgStr %s\n", nb, fmtStr, orgStr);
      zmabend("input disparity map must have two real-valued, BSQ-organized bands");
    }

    // allocate line/samp buffers for this dsp map
    if (dispLineBuf)
      free(dispLineBuf);
    if (! (dispLineBuf = (float *) malloc( ns * sizeof(float))))
      zmabend("failure allocating input line buffer");

    if (dispSampBuf)
      free(dispSampBuf);
    if (! (dispSampBuf = (float *) malloc( ns * sizeof(float))))
      zmabend("failure allocating input line buffer");

    // if xyz available, allocate x/y/z bufs
    if (xyz_nids > dspI) {
      if (xyzXBuf)
	free(xyzXBuf);
      if (! (xyzXBuf = (float *) malloc( ns * sizeof(float))))
	zmabend("failure allocating input xyzX buffer");

      if (xyzYBuf)
	free(xyzYBuf);
      if (! (xyzYBuf = (float *) malloc( ns * sizeof(float))))
	zmabend("failure allocating input xyzY buffer");

      if (xyzZBuf)
	free(xyzZBuf);
      if (! (xyzZBuf = (float *) malloc( ns * sizeof(float))))
	zmabend("failure allocating input xyzZ buffer");
    }

    // if no inp tp available, sample based on inp tiepointCount
    if (tie_nids <= dspI) {
      printf("%s:%d: sampling dsp based on requested tp count %d\n", __FILE__, __LINE__, tiepointCount);

      // Sample tiepoints by calculated linear period
      period = (int) MAX(1, sqrt((double) nl * ns / tiepointCount));
      printf("%s:%d: sampling period %d\n", __FILE__, __LINE__, period);

      for (line = 0; line < nl; line += period) {
	if (1 != zvread(dspUnit, dispLineBuf, "line", line + 1, "band", 1, NULL) ||
	    1 != zvread(dspUnit, dispSampBuf, "line", line + 1, "band", 2, NULL)) {
	  zmabend("failure reading disparity file");
	}
	
	if (xyz_nids > dspI) {
	  if (1 != zvread(xyzUnit, xyzXBuf, "line", line + 1, "band", 1, NULL) ||
	      1 != zvread(xyzUnit, xyzYBuf, "line", line + 1, "band", 2, NULL) ||
	      1 != zvread(xyzUnit, xyzZBuf, "line", line + 1, "band", 3, NULL))
	    zmabend("failure reading XYZ file");
	}
	
	for (samp = 0; samp < ns; samp += period) {
	  if (dispLineBuf[samp] && dispSampBuf[samp] &&
	      (xyz_nids <= dspI || xyzXBuf[samp] != 0.0 || xyzYBuf[samp] != 0.0 || xyzZBuf[samp] != 0.0)) {
	    
	    tps.resize(tpCnt + 1);
	    tps[tpCnt].type = tpType; // int, either TIEPOINT_TRADITIONAL or TIEPOINT_MISS_DISTANCE
	    tps[tpCnt].left_image = dspI; // index to dsp in concat_file_models
	    tps[tpCnt].right_image = dspI + nids; // index to rhs in concat_file_models
	    tps[tpCnt].left_key = 0; // ignored by mars_save_tiepoints
	    tps[tpCnt].right_key = 0; // ignored by mars_save_tiepoints
	    tps[tpCnt].left_sample = samp + 1;
	    tps[tpCnt].left_line = line + 1;
	    tps[tpCnt].right_sample = dispSampBuf[samp];
	    tps[tpCnt].right_line = dispLineBuf[samp];
	    tps[tpCnt].corrected_sample = dispSampBuf[samp];
	    tps[tpCnt].corrected_line = dispLineBuf[samp];
	    tps[tpCnt].angle = 0.0; // not used by traditional or miss distance
	    tps[tpCnt].xyz = PigVector(0.0, 0.0, 0.0); // not used by traditional or miss distance
	    tps[tpCnt].quality = 1.0;
	    tps[tpCnt].residual = 0.0;
	    tps[tpCnt].interactive = 0;
	    tps[tpCnt].active = 1;
	    tps[tpCnt].cs = cs;
	    ++tpCnt;
	  }
	}
      }
    } else { // use input tiepoint file to guide point selection
      printf("%s:%d: sampling dsp based on tp file %s\n", __FILE__, __LINE__, tie_filenames[dspI]);

      mars_load_tiepoints(tie_filenames[dspI], tiepoints, n_ties, concat_file_models,
			  2* nids, NULL, PigMission::getMissionObject(mission));

      printf("%s:%d: read %d tiepoints\n", __FILE__, __LINE__, n_ties);

      // iterate through tiepoints
      for (int i = 0; i < n_ties; ++i) {
	file_models[tiepoints[i].left_image]->getUniqueId(unique_id);
	
	// tp left image is this dsp map
	if (!strcmp(dsp_unique_id, unique_id)) {
	  samp = (int) round(tiepoints[i].left_sample) - 1; // "-1" to be consistent with the zero-based code above
	  line = (int) round(tiepoints[i].left_line) - 1;
	  
	  // the rest of this if body is an exact copy from code above -- should be factored
	  if (1 != zvread(dspUnit, dispLineBuf, "line", line + 1, "band", 1, NULL) ||
	      1 != zvread(dspUnit, dispSampBuf, "line", line + 1, "band", 2, NULL)) {
	    zmabend("failure reading disparity file");
	  }
	  
	  if (xyz_nids > dspI) {
	    if (1 != zvread(xyzUnit, xyzXBuf, "line", line + 1, "band", 1, NULL) ||
		1 != zvread(xyzUnit, xyzYBuf, "line", line + 1, "band", 2, NULL) ||
		1 != zvread(xyzUnit, xyzZBuf, "line", line + 1, "band", 3, NULL))
	      zmabend("failure reading XYZ file");
	  }
	  
	  // however, rather than iterating over samp like above, it was pulled from the current tp

	  if (dispLineBuf[samp] && dispSampBuf[samp] &&
	      (xyz_nids <= dspI || xyzXBuf[samp] != 0.0 || xyzYBuf[samp] != 0.0 || xyzZBuf[samp] != 0.0)) {
	    
	    tps.resize(tpCnt + 1);
	    tps[tpCnt].type = tpType; // int, either TIEPOINT_TRADITIONAL or TIEPOINT_MISS_DISTANCE
	    tps[tpCnt].left_image = dspI; // index to dsp in concat_file_models
	    tps[tpCnt].right_image = dspI + nids; // index to rhs in concat_file_models
	    tps[tpCnt].left_key = 0; // ignored by mars_save_tiepoints
	    tps[tpCnt].right_key = 0; // ignored by mars_save_tiepoints
	    tps[tpCnt].left_sample = samp + 1;
	    tps[tpCnt].left_line = line + 1;
	    tps[tpCnt].right_sample = dispSampBuf[samp];
	    tps[tpCnt].right_line = dispLineBuf[samp];
	    tps[tpCnt].corrected_sample = dispSampBuf[samp];
	    tps[tpCnt].corrected_line = dispLineBuf[samp];
	    tps[tpCnt].angle = 0.0; // not used by traditional or miss distance
	    tps[tpCnt].xyz = PigVector(0.0, 0.0, 0.0); // not used by traditional or miss distance
	    tps[tpCnt].quality = 1.0;
	    tps[tpCnt].residual = 0.0;
	    tps[tpCnt].interactive = 0;
	    tps[tpCnt].active = 1;
	    tps[tpCnt].cs = cs;
	    ++tpCnt;
	  }
	}
      }
    }

    if (! zvclose(dspUnit, NULL))
      zmabend("failure closing open dspfile");

    if (xyz_nids > dspI)
      if (! zvclose(xyzUnit, NULL))
	zmabend("failure closing open xyzfile");
  }

  // does mars_save_tiepoints have a version accepting a TiePoint vector?
  TiePoint *tpArray = new (std::nothrow) TiePoint[tpCnt];
  if (tpArray == NULL)
     zmabend("Memory allocation for Tie Point array failed");
  for (int i = 0; i < tpCnt; ++i)
    tpArray[i] = tps[i];

  if (mars_save_tiepoints(outTiePath, tpArray, tpCnt, concat_file_models, 2*nids, 0 /* start key */, cs))
    zmabend("failure saving tiepoints");

  delete[] tpArray;
  delete[] tiepoints;
}
