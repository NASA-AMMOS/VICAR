/* marscheckcm */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"

#include "lbl_image_data.h"
#include "lbl_camera_model.h"
#include "return_status.h"

#include <stdio.h>

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 1
#define MAX_OPEN 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define PRINT_MODEL_COMPONENT(Source, Number)                                 \
                                                                              \
    if (CM_##Source.ModelComponentId[Number - 1].Valid) {                     \
        val[0] = val[1] = val[2] = 0.0;                                       \
        cnt = 0;                                                              \
        while ((cnt < 3) && CM_##Source.ModelComponent##Number[cnt].Valid) {  \
    	    val[cnt] = CM_##Source.ModelComponent##Number[cnt].Value;         \
	    cnt++;                                                            \
        }                                                                     \
                                                                              \
        snprintf(msg, msgLen, "%s_%s = %+15.8f %+15.8f %+15.8f",                       \
	        CM_##Source.ModelComponentId[Number - 1].Value,               \
                #Source,                                                      \
	        val[0], val[1], val[2]);	                              \
                                                                              \
    zvmessage(msg, "");                                                       \
    }                                                        

// tol[] = |min(L:K)|*epsilon
#define PRINT_MODEL_COMPONENT_DIFF(Number)                                    \
                                                                              \
    if (CM_LABEL.ModelComponentId[Number - 1].Valid &&                        \
	CM_KINEM.ModelComponentId[Number - 1].Valid) {                        \
        val[0] = val[1] = val[2] = 0.0;                                       \
        tol[0] = tol[1] = tol[2] = 0.0;                                       \
        cnt = 0;                                                              \
        while ((cnt < 3)                                  &&                  \
	       CM_LABEL.ModelComponent##Number[cnt].Valid &&                  \
               CM_KINEM.ModelComponent##Number[cnt].Valid) {                  \
    	    val[cnt] = fabs(CM_LABEL.ModelComponent##Number[cnt].Value -      \
                       CM_KINEM.ModelComponent##Number[cnt].Value);           \
                                                                              \
            ((fabs(CM_LABEL.ModelComponent##Number[cnt].Value) -              \
              fabs(CM_KINEM.ModelComponent##Number[cnt].Value)) < 0 ) ?       \
    	    tol[cnt] = (fabs(CM_LABEL.ModelComponent##Number[cnt].Value) *    \
                       epsilon) :                                             \
    	    tol[cnt] = (fabs(CM_KINEM.ModelComponent##Number[cnt].Value) *    \
                       epsilon);                                              \
	    cnt++;                                                            \
        }                                                                     \
                                                                              \
        snprintf(msg, msgLen, "%s = %15.8f %15.8f %15.8f",                             \
                "|DIFF |",                                                    \
	        val[0], val[1], val[2]);	                              \
                                                                              \
        zvmessage(msg, "");                                                   \
                                                                              \
        snprintf(msg, msgLen, "%s_%s = %17s %15s %15s\n",                              \
	        CM_LABEL.ModelComponentId[Number - 1].Value,                  \
                "DIFF ",                                                      \
	        (fabs(val[0]) < tol[0]) ? "OK  " : "ERROR *",                 \
	        (fabs(val[1]) < tol[1]) ? "OK  " : "ERROR *",                 \
	        (fabs(val[2]) < tol[2]) ? "OK  " : "ERROR *");                \
	                                                                      \
                                                                              \
        zvmessage(msg, "");                                                   \
    }                                                        
	 

static int cm = 0;
static void getParam(char *name, void *value, int *count,
	     int maxcnt, int length, void *clientData);
////////////////////////////////////////////////////////////////////////

void main44()
{
    int count, def;
    int status;
    const size_t msgLen = 500;
    char msg[msgLen];

    int band;
    int nids, nods;		// # of inputs, outputs (must match!)
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];		// not used
    PigPointingModel *pointing_in[MAX_INPUTS];		// not used
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;

    LblCameraModel_typ CM_LABEL;
    LblCameraModel_typ CM_KINEM;

    zvmessage("MARSCM version 1", "");

    PigParamFunction func = getParam;
    PigModelBase::setDefaultParamFunction(func,NULL);

    // Get the input file list, and set up initial camera/pointing models
    // for each input (not used)
    mars_setup(nids, file_models, camera_in, pointing_in, NULL, 
	       cs, mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in, camera_in, file_models,
			homogeneous_inputs, mission, instrument);

    // create camera model using label camera models
    PigCameraModel *camera = PigCameraModel::create(file_models[0], NULL);

    camera->setupWriteToLabel(CM_LABEL, camera->getCoordSystem());

    // switch to kinematics mode
    cm = 1;
    // create camera model using kinematics camera models
    camera = PigCameraModel::create(file_models[0], NULL);

    // Create Pointing Model based on the input. 
    PigPointingModel *pointing_out = PigPointingModel::create(camera,
						  camera->getMissionName(), 
						  camera->getInstrumentName(),
	                                          NULL,
	                                          true);
    // Point camera model
    pointing_out->pointCamera(file_models[0]);

    camera->setupWriteToLabel(CM_KINEM, camera->getCoordSystem());

    // write comparison of two models to the std output
    if (!CM_LABEL.ModelType.Valid || !CM_KINEM.ModelType.Valid) {
        zvmessage("CM.ModelType.Valid = 0 \n ERROR!!!!", "");
	zabend();
    }
    if (strcmp(CM_LABEL.ModelType.Value, CM_KINEM.ModelType.Value)) {
        zvmessage("CM.ModelType.Value are not the same \n ERROR!!!!", "");
	zabend();
    }

    int cnt;
    double val[3];
    double tol[3];
    double epsilon;
    zvparmd("TOL", &epsilon, &count, &def, 1, 0);

    snprintf(msg, msgLen, "\nComparing LABEL and KINEM camera models...\n");
    zvmessage(msg, "");
    snprintf(msg, msgLen,
	    "IF (|DIFF| < |min(LABEL,KINEM)| * Tolerance) THEN OK ELSE ERROR");
    zvmessage(msg, "");

    snprintf(msg, msgLen, "\nTolerance=%f\n", epsilon);
    zvmessage(msg, "");


    PRINT_MODEL_COMPONENT(LABEL, 1);   
    PRINT_MODEL_COMPONENT(KINEM, 1); 
    PRINT_MODEL_COMPONENT_DIFF(1);

    PRINT_MODEL_COMPONENT(LABEL, 2);   
    PRINT_MODEL_COMPONENT(KINEM, 2);
    PRINT_MODEL_COMPONENT_DIFF(2);

    PRINT_MODEL_COMPONENT(LABEL, 3);   
    PRINT_MODEL_COMPONENT(KINEM, 3);
    PRINT_MODEL_COMPONENT_DIFF(3);

    PRINT_MODEL_COMPONENT(LABEL, 4);   
    PRINT_MODEL_COMPONENT(KINEM, 4); 
    PRINT_MODEL_COMPONENT_DIFF(4);

    PRINT_MODEL_COMPONENT(LABEL, 5);   
    PRINT_MODEL_COMPONENT(KINEM, 5);
    PRINT_MODEL_COMPONENT_DIFF(5); 

    PRINT_MODEL_COMPONENT(LABEL, 6);   
    PRINT_MODEL_COMPONENT(KINEM, 6);
    PRINT_MODEL_COMPONENT_DIFF(6); 
                                 
    PRINT_MODEL_COMPONENT(LABEL, 7);   
    PRINT_MODEL_COMPONENT(KINEM, 7);
    PRINT_MODEL_COMPONENT_DIFF(7);
}

// Custom implementation of getParam.
// Works as default except for POINT_METHOD
// Assumes that POINT_METHOD string doesn't 
// contain cm="method" string.
void getParam(char *name, void *value, int *count,
	     int maxcnt, int length, void *clientData)
{
    // use default implementation 
    int def;		// dummy
    zvparmd(name, value, count, &def, maxcnt, length);
    //check for point_method
    if (!strcmp(name, "POINT_METHOD")) {
        char str_cm[100];
	if (cm == 0)
	    strcpy(str_cm, "cm=label");
	else
	    strcpy(str_cm, "cm=kinematics");

        if (*count == 0) {	    
	    strcpy((char*)value, str_cm);
	    *count = 1;
	}
	else {
	  strcat((char*)value, str_cm);
	}
    }
}
