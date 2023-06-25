////////////////////////////////////////////////////////////////////////
// mars_create_pointing_model.cc
//
// Return codes:
// -1 Pointing correction file exists but doesn't contain matching solution
//    default PointingModel has been created
// 0  No Pointing Correction file has been provided, create default model
// 1  Match has been found in Pointing Correction file
//
////////////////////////////////////////////////////////////////////////
#include "mars_support.h"

#include "PigPointingModel.h"
#include "PigCameraModel.h"
#include "PigFileModel.h"

#include "zvproto.h"

#include <stdio.h>
#include <string.h>

PigPointingModel *mars_create_pointing_model(PigCameraModel *camera_model, 
                               PigFileModel *file_model,
		               const char *sid,
                               PigPointingCorrections *pointing_corrections,
                               int &status)
{   
    const char *point_model_type;
    char val[10];
    char msg[PIG_MAX_FILENAME_SIZE+1];
    int count, match_method;
    PigPointingParams *param = NULL;
    
    if (pointing_corrections == NULL) {
    	// no nav file, create from the label info
	status = 0;
    	return PigPointingModel::create(camera_model, file_model, NULL, true);
	
    }
     
    zvp("MATCH_METHOD", val, &count);
    if (strcasecmp(val, "loose"))
        match_method = 1;
    else
        match_method = 0;
	
    double match_tol;
    int def;
    zvparmd("MATCH_TOL", &match_tol, &count, &def, 1, 0);
    
    PigPointingModel *pointing_model = NULL;
    	if (match_method) {
	    // do tight match
	    char unique_id[33] = "";
            file_model->getUniqueId(unique_id);
            if (!strcmp(unique_id, "")) {
                sprintf(msg, "Empty unique id for file %s", file_model->getFilename());
                zvmessage(msg, "");
		status = -1;
                return NULL;
            }
            param = pointing_corrections->getPointing(unique_id, sid, NULL);

	    // If no match for unique_id, check UniqueId2

	    if (param == NULL) {
		char unique_id2[33] = "";
		file_model->getUniqueId2(unique_id2);
		if (strcmp(unique_id, unique_id2) != 0) {
		    param = pointing_corrections->getPointing(unique_id2, sid, NULL);
		}
	    }

	    if (param != NULL) {
	        //found match
		PigPointingModel *pm = PigPointingModel::create(camera_model, file_model, param->getType(), true);
		pm->pointCamera(file_model);
                pm->setPointingParameters(param->getParameters(),param->getPointingParamCount());
                status = 1;
		return pm;
	    }
	}
	else {  // loose match
	  int count = 0;
	  char **pointing_model_types = pointing_corrections->getPointingModelTypes(sid, file_model->getInstrumentId(), count);
	  for (int cnt = 0; cnt < count; cnt++) {
	      pointing_model = PigPointingModel::create(camera_model, file_model, pointing_model_types[cnt], true);
	      if (pointing_model == NULL)
	          continue;
	      pointing_model->pointCamera(file_model);
	      double params[PIG_MAX_PARAMS];
	      pointing_model->getPointingParameters(params, PIG_MAX_PARAMS);
	      param=pointing_corrections->getPointing(params, match_tol, sid, pointing_model->getModelName());
	      if (param != NULL) {
	          // found match
		  pointing_model->setPointingParameters(param->getParameters(), param->getPointingParamCount());
		  status = 1;
	          return pointing_model;
	      }
	      // no match is found for given pointing params, try different pointing model type
	      delete pointing_model;
	      }   
	  } 
    // no match found in the nav file, create default model
    status = 0;	  
    return PigPointingModel::create(camera_model, file_model, NULL, true);
}

/////////////////////////////////////////////////////////////////////// 
// Simplified form of mars_create_pointing_model that does all the
// grunt work to get sid and pointing_corrections and deals with the
// output.  The caller need only check for null (and does not need to
// print any errors, they're already printed).  The camera will have been
// pointed, and the navtable applied.  "descr" simply describes the
// file for error messages (e.g. "image 1", "stereo partner", etc).
///////////////////////////////////////////////////////////////////////
PigPointingModel *mars_create_pointing_model(PigCameraModel *camera_model,
                                PigFileModel *file_model, char *descr)
{
    int status;
    char msg[1024];

    char solution_id[256];
    int count;
    zvp("SOLUTION_ID", solution_id, &count);
    char *sid = NULL;
    if (count != 0) {
        sid = solution_id;
    }

    PigMission *m =PigMission::getMissionObject(camera_model->getMissionName());
    PigPointingCorrections *pointing_corrections =
					mars_get_pointing_corrections(m);

    PigPointingModel *pointing = mars_create_pointing_model(camera_model,
			file_model, sid, pointing_corrections, status);

    if (pointing == NULL) {
	sprintf(msg, "Unable to create pointing model for %s", descr);
	zvmessage(msg, "");
	return NULL;
    }
    if (status == -1) {		// Nav file exists but no match found
	sprintf(msg, "No match in the navtable found for %s", descr);
	zvmessage(msg, "");
	pointing->pointCamera(file_model);
    }
    else if (status == 0) {	// No nav file present
	pointing->pointCamera(file_model);
    }
    else if (status == 1) {	// Correction has been applied
	sprintf(msg, "Pointing Correction has been applied for %s", descr);
	zvmessage(msg, "");
	// camera has already been pointed in mars_create_pointing_model
    }
    else {
	sprintf(msg, "Internal error creating pointing model for %s", descr);
	zvmessage(msg, "");
	return NULL;
    }

    return pointing;
	
}

