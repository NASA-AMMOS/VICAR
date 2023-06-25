/** Source code for:      cattp.c
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
  int parmCt, parmDf, n_ties;
  char outTiePath[1000];

  /* adapted from marsdispwarp.cc and marstie.cc */
#define MAX_INPUTS 5000

  char mission[64];
  PigFileModel *img_file_models[MAX_INPUTS];
  PigCoordSystem *cs = NULL;
  TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
  if(tiepoints == NULL)
       zmabend("Memory allocation error during TiePoints[MARS_MAX_TIEPOINTS] initialization");

  zifmessage("cattp version Thu Sep 18 2014");

  if (1 != zvparm ("out", outTiePath, &parmCt, &parmDf, 1, 999))
    zmabend("failure reading output tiepoint file name");

  // setup IMG files
  // adapted from marsslope.com
  char **img_filenames = new char *[MAX_INPUTS];
  if (img_filenames == NULL)
    zmabend("failure allocating img filenames");

  int img_nids;
  mars_get_filelist("IMG", img_nids, img_filenames, MAX_INPUTS, FALSE);

  for (int i = 0; i < img_nids; i++) 
    {
      img_file_models[i] = PigFileModel::create(img_filenames[i]);
      if (img_file_models[i] == NULL) 
        {          
    const size_t msgLen = 150;
	  char msg[msgLen];
	  snprintf(msg,msgLen,"Unable to create file model for IMG input %d", i);
	  zmabend(msg);
        }
    }

  // setup INP tp files
  char **tie_filenames = new char *[MAX_INPUTS];
  if (tie_filenames == NULL)
    zmabend("failure allocating tie filenames");

  int tie_nids;
  mars_get_filelist("INP", tie_nids, tie_filenames, MAX_INPUTS, FALSE);

  std::vector<TiePoint> tps;

  int tpCnt = 0;

  // iterate through inp tp files
  for (int tpI = 0; tpI < tie_nids; ++tpI) {
    n_ties = MARS_MAX_TIEPOINTS;
    mars_load_tiepoints(tie_filenames[tpI], tiepoints, n_ties, img_file_models,
			img_nids, NULL, PigMission::getMissionObject(mission));

    // iterate through tiepoints
    for (int i = 0; i < n_ties; ++i) {
      tps.resize(tpCnt + 1);
      tps[tpCnt] = tiepoints[i];
      ++tpCnt;
    }
  }

  // does mars_save_tiepoints have a version accepting a TiePoint vector?
  TiePoint *tpArray = new (std::nothrow) TiePoint[tpCnt];
  if (tpArray == NULL)
     zmabend("Memory allocation error during tpArray initialization");
  for (int i = 0; i < tpCnt; ++i)
    tpArray[i] = tps[i];

  printf("saving %d tiepoints\n", tpCnt);

  if (mars_save_tiepoints(outTiePath, tpArray, tpCnt, img_file_models, img_nids, 0 /* start key */, cs))
    zmabend("failure saving tiepoints");

  delete[] tpArray;
  delete[] tiepoints;
}
