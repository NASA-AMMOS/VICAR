/** Source code for:      tp2disp.c
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
#include "PigLabelModel.h"
#include "PigFileModel.h"

struct Pair {
  char* lhs;
  PigFileModel* lModel;
  char* rhs;
  PigFileModel* rModel;
};

void main44(void)
{   
  int parmCt, parmDf;
  float *dispLineBuf = NULL, *dispSampBuf = NULL;

  /* adapted from marsdispwarp.cc and marstie.cc */
#define MAX_INPUTS 1000
#define MAX_NS 2048
#define MAX_NL 65536

  char mission[64], instrument[64];
  PigFileModel *file_models[MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  int homogeneous_inputs = TRUE;
  PigCoordSystem *cs;
  TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
  if(tiepoints == NULL)
     zmabend("Memory allocation error during TiePoints[MARS_MAX_TIEPOINTS] initialization");
  int n_ties = MARS_MAX_TIEPOINTS;
  char lhs_id[33], rhs_id[33];

  zifmessage("tp2disp version Wed Mar  4 2015");

  // setup INP img files
  int nids;
  mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
	     mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);
  PigMission *m = PigMission::getMissionObject(mission);
  if (!m)
    zmabend("failure creating mission object");

  // setup TIE file
  char tiePath[1000];
  if (1 != zvparm ("tp", tiePath, &parmCt, &parmDf, 1, 999))
    zmabend("failure reading input tiepoint file name");

  // load TP file
  mars_load_tiepoints(tiePath, tiepoints, n_ties, file_models, nids, NULL, m);

  printf("%s:%d: read %d tiepoints\n", __FILE__, __LINE__, n_ties);

  // iterate through tiepoints
  std::vector<char *> ids;
  for (int i = 0; i < n_ties; ++i) {
    file_models[tiepoints[i].left_image]->getUniqueId(lhs_id);
    file_models[tiepoints[i].right_image]->getUniqueId(rhs_id);

    int foundLhs = 0, foundRhs = 0;

    for (std::vector<char*>::iterator it = ids.begin() ; it != ids.end(); ++it) {
      if (! foundLhs && ! strcmp(lhs_id, *it))
	foundLhs = 1;
      if (! foundRhs && ! strcmp(rhs_id, *it))
	foundRhs = 1;
      if (foundLhs && foundRhs)
	break;
    }

    if (!foundLhs)
      ids.push_back(strdup(lhs_id));
    if (!foundRhs)
      ids.push_back(strdup(rhs_id));
  }

  // create id pairs
  std::vector<Pair *> pairs;
  for (std::vector<char*>::iterator it = ids.begin() ; it != ids.end(); ++it)
    for (std::vector<char*>::iterator it2 = ids.begin() ; it2 != ids.end(); ++it2) {
      if (!strcmp(*it, *it2))
	continue;

      Pair* pair_p = new Pair();
      pair_p->lhs = strdup(*it);
      pair_p->rhs = strdup(*it2);
      pair_p->lModel = pair_p->rModel = NULL;

      char unique_id[33];
      for (int i = 0; i < nids; ++i) {
	file_models[i]->getUniqueId(unique_id);
	if (! strcmp(unique_id, *it))
	  pair_p->lModel = file_models[i];
	else if (! strcmp(unique_id, *it2))
	  pair_p->rModel = file_models[i];

	if (pair_p->lModel && pair_p->rModel)
	  break;
      }

      pairs.push_back(pair_p);
    }

  // iterate through pairs
  char dispPath[1000];
  for (std::vector<Pair *>::iterator it = pairs.begin() ; it != pairs.end(); ++it) {
    sprintf(dispPath, "%s_%s_DSP.VIC", (*it)->lhs, (*it)->rhs);
    printf("Generating %s\n", dispPath);

    // get LHS nl, ns
    char unique_id[33];
    int nl=0, ns=0;
    int model_i;
    for (model_i = 0; model_i < nids; ++model_i) {
      file_models[model_i]->getUniqueId(unique_id);

      if (! strcmp(unique_id, (*it)->lhs)) {
	nl = file_models[model_i]->getNL();
	ns = file_models[model_i]->getNS();
	break;
      }
    }

    if (!nl || !ns)
      zmabend("failed reading nl/ns from file_model");
    
    if (! (dispLineBuf = (float *) malloc( nl * ns * sizeof(float))))
      zmabend("failure allocating ouput line buffer");
    memset(dispLineBuf, 0, nl * ns * sizeof(float));

    if (! (dispSampBuf = (float *) malloc( nl * ns * sizeof(float))))
      zmabend("failure allocating output samp buffer");
    memset(dispSampBuf, 0, nl * ns * sizeof(float));

    // iterate through tiepoints
    std::vector<char *> ids;
    for (int i = 0; i < n_ties; ++i) {
      file_models[tiepoints[i].left_image]->getUniqueId(lhs_id);
      file_models[tiepoints[i].right_image]->getUniqueId(rhs_id);
      if (! strcmp(lhs_id, (*it)->lhs) && ! strcmp(rhs_id, (*it)->rhs)) {
	int left_sample = (int) round(tiepoints[i].left_sample - 1); // line/samp are one-based
	int left_line = (int) round(tiepoints[i].left_line - 1); // but used here as zero-based indices
	dispLineBuf[left_line * ns + left_sample] = tiepoints[i].right_line;
	dispSampBuf[left_line * ns + left_sample] = tiepoints[i].right_sample;

	continue;		// do not need to check tiepoint rhs
      }

      file_models[tiepoints[i].right_image]->getUniqueId(rhs_id);
      if (! strcmp(rhs_id, (*it)->lhs) && ! strcmp(lhs_id, (*it)->rhs)) {
	int left_sample = (int) round(tiepoints[i].right_sample - 1);
	int left_line = (int) round(tiepoints[i].right_line - 1);
	dispLineBuf[left_line * ns + left_sample] = tiepoints[i].left_line;
	dispSampBuf[left_line * ns + left_sample] = tiepoints[i].left_sample;
      }

    }    

    // use nl, ns
    // adapted from marscor3.com
    int unit_out;

    if (zvunit(&unit_out, "foo", 1, "U_NAME", dispPath, NULL) != 1)
      zmabend("failure creating output disp");

    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL", "O_FORMAT", "REAL",
	   "U_NS", ns, "U_NL", nl,
	   "OPEN_ACT", "AS", "U_NB", 2, "U_ORG", "BSQ", NULL);
    zvplabel(unit_out, 0, 1);

    // file_models of two related images
    PigFileModel *two_models[2];
    two_models[0] = (*it)->lModel;
    two_models[1] = (*it)->rModel;

    PigLabelModel* labelModel = m->createLabelModel(unit_out);

    labelModel->setDisparity(two_models, two_models[1], 2, "DISPARITY_MAP");

    for (int line = 0; line < nl; ++line) {
      zvwrit (unit_out, &dispLineBuf[line * ns],
	      "LINE", line + 1, /* LINE is one based; line is zero based */
	      "SAMP", 1,
	      "BAND", 1,
	      "NSAMPS", ns, NULL);
      zvwrit (unit_out, &dispSampBuf[line * ns],
	      "LINE", line + 1, /* LINE is one based; line is zero based */
	      "SAMP", 1,
	      "BAND", 2,
	      "NSAMPS", ns, NULL);
    }

    zvclose(unit_out, NULL);

    free(dispLineBuf);
    free(dispSampBuf);
  }
  delete[] tiepoints;
}
