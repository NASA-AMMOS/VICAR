// vim:set tabstop=4:
// vim:set expandtab:
// vim:set shiftwidth=4:
    


/////////////////////////////////////////////////////////////////////////
//marschkovl - multimission overlap check
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"
#include "mars_support.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigMission.h"

#include <string.h>
#include <stdlib.h>
#include<ctype.h>

/* buffer sizes in main program */
#define MAX_INPUTS 2048


    


void
test_overlap(
    PigCameraModel * camera_in[],
    PigSurfaceModel * surface,
    PigFileModel * file_models[],
    PigCoordSystem * cs, int border, float overlap,
    int spacing, double sep_cos, float sep_angle,
    PigPointingModel * pointing_in[],int ref,int test,
    char **filenames,FILE* fdl,FILE* fdr);

// modified version of mars_setup, calls mars_chkovl_get_filelist instead of
//mars_get_filelist
void mars_chkovl_setup(int &nids, PigFileModel *files[],
    PigCameraModel *cameras[], PigPointingModel *pointings[],
    PigSurfaceModel *&surface_model, RadiometryModel *radiometric[],
    PigCoordSystem *&def_cs, char *mission, char *instrument,
    int &homogeneous_inputs, 
    const int max_inputs,int *outnids,char **filenames);


// handles a single list or 2 lists (left and right)
void mars_chkovl_get_filelist(const char *param_name, int &nids, 
    char *filenames[], const int max_inputs,int *outnids);
void get_output_files(FILE **fds);

////////////////////////////////////////////////////////////////////////
//MarsCHKOVL program
////////////////////////////////////////////////////////////////////////

void            main44() {

    int             count;
    
    /* Inputs */
    
    int             nids;
    PigFileModel   *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface_model;
    int             homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *dummy_cs=NULL;
    int             border;
    float           overlap;
    int             spacing;
    float           sep_angle;
    double          sep_cos;
    int outnids[2];
    
    /* User Parameters */
    char            mission[64], instrument[64];
    int ref,test;
    char **filenames = new char *[MAX_INPUTS];
    
    
    zvmessage("MARSCHKOVL version 1", "");
    FILE *fds[2];
    get_output_files(fds);
    
    
    outnids[0]=0;
    outnids[1]=0;
    mars_chkovl_setup(nids, file_models, camera_in, pointing_in,  
        surface_model,NULL,dummy_cs, mission, instrument, 
        homogeneous_inputs, MAX_INPUTS,outnids,filenames);
    
    
    
    /* get parameter overrides if any */
    zvp("BORDER", &border, &count);
    zvp("OVERLAP", &overlap, &count);
    zvp("SPACING", &spacing, &count);
    zvp("SEP_ANGLE", &sep_angle, &count);
    /* max sep angle for frames */
    sep_cos = cos(PigDeg2Rad(sep_angle));
    
    
    /*
    * Create surface model based on the first input.Looks at user
    * parameters.
    */
    
    surface_model = PigSurfaceModel::create(file_models[0]);
    
    cs = surface_model->getCoordSystem();
    
    /* Print out input status from labels */
    
    mars_print_inputs(outnids[0] + outnids[1], pointing_in, 
         camera_in, file_models, homogeneous_inputs, mission, instrument);
    
    if (nids == 1) {
        // Single list. 
        for(ref=0;ref  < outnids[0] -1;++ref)
            for(test=ref+1;test < outnids[0];++ test)
                test_overlap(camera_in, surface_model, file_models, cs, border,
                    overlap, spacing, sep_cos, sep_angle, pointing_in,
                    ref,test,filenames,fds[0],fds[1]);
    }
    else {
    // Two lists. The left and right lists are stored end to end
    // so if the first list has n elements, the m'th file in the second
    // list has an index of n+m
        for(ref=0;ref  < outnids[0] ;++ref)
        for (test= outnids[0];test <  outnids[0] +  outnids[1];++test)
            test_overlap(camera_in, surface_model, file_models, cs, border,
                overlap, spacing, sep_cos, sep_angle, pointing_in,ref,
                test,filenames,fds[0],fds[1]);
    }
   fclose(fds[0]);
   fclose(fds[1]);
}


// tests an image pair for sufficient overlap
// The variables "ref" and "test" are indices into the arrays 
// pointing_in,file_models,camera_in,filenames
void
test_overlap( PigCameraModel * camera_in[], PigSurfaceModel * surface,
    PigFileModel * file_models[], PigCoordSystem * cs, int border, 
    float overlap, int spacing, double sep_cos, float sep_angle,
    PigPointingModel * pointing_in[],int ref,int test,
    char **filenames, FILE * fdl,FILE* fdr)
{

    int             i, j;
    int             sl, ss, el, es;
    double          sl_d, ss_d, el_d, es_d;
    PigPoint        origin;
    PigVector       look;
    int             hits;
    PigPoint        surf_pt;
    int             valid_pts = 0;
    int             total_pts = 0;
    int             nlw, nsw;
    nlw = spacing;
    nsw = spacing;
    const size_t messageLen = 256;
    char message[messageLen]; //living dangerously



    if ((pointing_in[ref]->getCameraOrientation(cs) %
         pointing_in[test]->getCameraOrientation(cs)) < sep_cos) {
        zvmessage("quitting test  early because pointing test failed","");
        snprintf(message, messageLen, " angle= %f theshold= %f",
              PigRad2Deg(acos(pointing_in[ref]->getCameraOrientation(cs) %
                       pointing_in[test]->getCameraOrientation(cs))),sep_angle);
        zvmessage(message,"");
        return;
    }

    file_models[ref]->getImageBorders(sl_d, ss_d, el_d, es_d);
    /* conversion to int, above returns doubles. */
    ss = (int) ss_d;
    sl = (int) sl_d;
    el = (int) el_d;
    es = (int) es_d;
    /* Brute force testing of every point */
    for (j=sl+border+nlw; j<el-border-nlw; j=j+nlw ) {
        for (i=ss+border+nsw;i<es-border-nsw;i=i+nsw) {
            total_pts++;
            camera_in[ref]->LStoLookVector(j, i, origin, look, cs);
            hits = surface->intersectRay(origin, look, surf_pt);
            double          line1, samp1;
            camera_in[test]->XYZtoLS(surf_pt, (hits <= 0), &line1, &samp1, cs);
            if (file_models[test]->testPixelLocation(line1, samp1) == 0)
                ++valid_pts;
        }
    }
    snprintf(message, messageLen, "Overlap for ref=%s test=%s %f",filenames[ref], 
                             filenames[test],((100.0 * valid_pts) / total_pts));
    zvmessage(message,"");
    if ((100.0 * valid_pts) / total_pts > overlap) {
        fprintf(fdl,"%s\n",filenames[ref]);
        fprintf(fdr,"%s\n",filenames[test]);
        
    }
}



// this is a copy of mars_setup
// the only difference is that it calls mars_chkovl_get_filelist
void mars_chkovl_setup(int &nids, PigFileModel *files[],
        PigCameraModel *cameras[], PigPointingModel *pointings[],
        PigSurfaceModel *&surface_model, RadiometryModel *radiometric[],
        PigCoordSystem *&def_cs, char *mission, char *instrument,
        int &homogeneous_inputs,  
        const int max_inputs,int *outnids,char **filenames)
{
    int i;
    int status;
    const size_t msgLen = PIG_MAX_FILENAME_SIZE+1;
    char msg[msgLen];

    homogeneous_inputs = TRUE;

    ////////////////////////////////////////////////////////////////////////
    // Get the list of all files

    //char **filenames = new char *[max_inputs];
    if (filenames == NULL) {
        zvmessage("Memory error in mars_setup, filename array", "");
        zabend();
    }
    mars_chkovl_get_filelist("INP", nids, filenames, max_inputs,outnids);

    ////////////////////////////////////////////////////////////////////////
    // Open all files

    for (i = 0; i < outnids[0] + outnids[1]; i++) {
        files[i] = PigFileModel::create(filenames[i]);
        if (files[i] == NULL) {
            snprintf(msg, msgLen, "Unable to create file model for input %d", i);
            zvmessage(msg, "");
            zabend();
        }

    }

    ////////////////////////////////////////////////////////////////////////
    // Coord system stuff

    // Initialize the RSF's, if necessary.
    //!!!! THIS SHOULD BE DONE ONCE PER MISSION !!!!
    // FIXME: Is this correct??

    mars_read_rsf(files[0]->getMissionName());

    // Default coordinates

    def_cs = mars_setup_coords(files[0]->getMissionName(), 
            outnids[0] + outnids[1], files);

    PigMission *m = PigMission::getMissionObject(files[0]->getMissionName());
    PigPointingCorrections *pointing_corrections = 
        mars_get_pointing_corrections(m);

    char solution_id[256];
    int count;
    zvp("SOLUTION_ID", solution_id, &count);
    char *sid = NULL;
    if (count != 0) {
        sid = solution_id;
    } 
    else if (pointing_corrections != NULL) {
        // no solution_id found on a command line
        // default to highest priority in the xml nav file if it exists
        sid = pointing_corrections->getHighestPriority();
    }

    // construct surface model
    if (pointing_corrections != NULL) {
        //look for surface in the nav file
        PigSurfaceModelParams *smp = pointing_corrections->getSurfaceModel(sid);

        //!!!! This is a hack to get around the problem that we can't create
        // proper coordinate system knowing only it's name.  For example
        // specifying on a command line SURF_COORD=ROVER is not enough to create
        // proper Rover Coordinate System.  Because of that we check for command
        // line parameter here, and if it's given we discard NAV file 
        //surface model
        // if any and go directly to PigSurfaceModel::create(file).  
        //That one knows
        // how to create proper CS.  For more info see comments in 
        //PigMission.cc
        // -ozp
        int cnt=0;
        char surface_coord[20];
        PigModelBase::getStaticParam("SURF_COORD", surface_coord, &cnt, 1, 0);    
        PigMission *m = pointing_corrections->getMission();
        if ((smp == NULL) || (cnt==1)) {
            // no surface model definition found in the nav file,
            // or surface model definition has been specified
            // on the command line 
            //create surface model using label info from the first
            // input file
            surface_model = PigSurfaceModel::create(files[0]);
        }
        else {  
            PigCSReference *csr = smp->getCoordSystemParams();
            PigCSReference csRef(m, csr->getFrameName(),
                                sid,
                                csr->getIndices(),
                                csr->getNumIndices(),
                                csr->getInstId());

            surface_model = PigSurfaceModel::create(m->getMissionName(),
                    instrument,
                    smp->getPointingParams()->getType(),
                    smp->getPointingParams()->getParameters(),
                    smp->getPointingParams()->getPointingParamCount(),
                    m->getCoordSystem(&csRef));
        }
    }
    else {
        // no nav file in xml format exists, 
        //create default one using label info from the first
        // input file
        surface_model = PigSurfaceModel::create(files[0]);  
    }    

    ////////////////////////////////////////////////////////////////////////
    // Now loop through and initialize all files.  This must be done after
    // the above coord system setups

    for (i = 0; i < outnids[0] + outnids[1]; i++) {

        ////////////////////////////////////////////////////////////////////////
        // Compute initial camera and pointing models

        cameras[i] = PigCameraModel::create(files[i], NULL);

        if (cameras[i] == NULL) {
            snprintf(msg,msgLen, "Unable to create camera model for input %d", i);
            zvmessage(msg, "");
            zabend();
        }

        pointings[i] = mars_create_pointing_model(cameras[i], files[i], 
            sid, pointing_corrections, status);


        if (pointings[i] == NULL) {
            snprintf(msg, msgLen, "Unable to create pointing model for input %d", i+1);
            zvmessage(msg, "");
            zabend();
        }

        if (status == -1) {
            //nav file exists but no match has been found
            snprintf(msg, msgLen, "No match in the Navtable has been found for input %d", i+1);
            zvmessage(msg, "");
            // Point the input camera
            pointings[i]->pointCamera(files[i]);
        }
        else if (status == 0) {
            // no pointing correction file present
            // Point the input camera
            pointings[i]->pointCamera(files[i]);    
        }
        else if (status == 1) {
            // correction has been applied
            snprintf(msg,msgLen, "Pointing Correction has been applied for input %d", i+1);
            zvmessage(msg, "");
        }


        // Check for varying missions and instruments, for information only

        if (i != 0) {
            if (strcasecmp(mission, cameras[i]->getMissionName()) != 0) {
                snprintf(msg,msgLen,
                        "Note: Input list contains more than one mission: %s",
                        mission);
                zvmessage(msg, "");
                homogeneous_inputs = FALSE;
            }
            if (strcasecmp(instrument, cameras[i]->getInstrumentName()) != 0) {
                snprintf(msg,msgLen,
                        "Note: Input list contains more than one instrument: %s"
                        , instrument);
                zvmessage(msg, "");
                homogeneous_inputs = FALSE;
            }
        }
        strcpy(mission, cameras[i]->getMissionName());
        strcpy(instrument, cameras[i]->getInstrumentName());


        ////////////////////////////////////////////////////////////////////////
        // Create Radiometry Models for each input image, if requested

        if (radiometric != NULL) {
            if (zvptst("NORAD")) {        // No radiometric correction
                radiometric[i] = NULL;
            }
            else {
                radiometric[i] = RadiometryModel::create(files[i]);

                if (radiometric[i] == NULL) {
                    snprintf(msg, msgLen,
                            "Unable to create radiometric model for input %d", i);
                    zvmessage(msg, "");
                }
            }
        }

        files[i]->closeFile();            // don't have too many open
    }

    // check for old style, text-based nav file, if it exists, apply it's values
    if (pointing_corrections == NULL) {
        //check for old format style and if nav file in old format found
        //print warning message and apply pointing corrections
        mars_apply_navtable(outnids[0] + outnids[1], pointings, NULL);
    }

    //for (i=0; i < outnids; i++)
    //delete filenames[i];
    //delete filenames;

    zvmessage("All input labels processed", "");

    return ;
}



/////////////////////////////////////////////////////////////////
// Read the file names from a list and fill in the filenames array
/////////////////////////////////////////////////////////////////
void get_files(const char *param_name,char *filenames[],int listnum, 
        int max_inputs,int *outnids, int &readsofar) {

    const size_t msgLen =PIG_MAX_FILENAME_SIZE+1;
    char msg[msgLen];
    char *filename;
    zvpone((char *)param_name, msg, listnum+1, PIG_MAX_FILENAME_SIZE);
    FILE *fd;
    filename = new char[strlen(msg)+1];
    strcpy(filename, msg);
    if ((fd = fopen(filename, "r")) == NULL) {           // Can't open
        snprintf(msg, msgLen, "Error opening file list %s\n", filename);
        zvmessage(msg, "");
        zabend();
    }
    int i;
    int thiscount=0;
    for (i = 0; i < max_inputs; i++) {
        if (fgets(msg, sizeof(msg), fd) == NULL) {
            break;
        }
    
        for (int j=0; j<strlen(msg); j++)   // clear non printing stuff
            if (!isprint(msg[j])) msg[j] = '\0';
        filenames[i+readsofar] = new char[strlen(msg)+1];
        if (filenames[i+readsofar] == NULL) {
            zvmessage("Memory error in mars_get_filelist", "");
            zabend();
        }
        strcpy(filenames[i+readsofar], msg);
        thiscount += 1;
    }
    fclose(fd);
    if (thiscount == 0) {
        zvmessage("No input files found in listoffiles", "");
        zabend();}
    else {
        snprintf(msg, msgLen, "%d files located in filelist  %s", thiscount, filename);
        zvmessage(msg, "");
    }
    outnids[listnum] = thiscount;
    readsofar += thiscount;
    return;
}

///////////////////////////////////////////////////////////////
// Wrapper around get_files
// For each file specified by the INP parm, read the file names
// and store them sequentially in the filenames array
///////////////////////////////////////////////////////////////
void mars_chkovl_get_filelist(const char *param_name, int &nids, 
    char *filenames[], const int max_inputs,int *outnids)
{
    int listnum;
    //int status;
    //char msg[PIG_MAX_FILENAME_SIZE+1];
    //int i;

    zvpcnt((char *)param_name, &nids);
    if (nids != 2 && nids != 1) {
        zvmessage("Need 1 or 2 lists", "");
        zabend();
        return;
    }
 

    int numread=0; 
    for (listnum=0;listnum<nids;++listnum) {
        get_files(param_name,filenames,listnum, max_inputs,outnids,numread);
    }
 
    return;
}


/////////////////////////////////////////////////////////
// fill in the fd array with FILE descriptors returned from opening
// the files specied by the OUT parm.
/////////////////////////////////////////////////////////
void get_output_files(FILE **fds) {
    char leftoutfilename[PIG_MAX_FILENAME_SIZE];
    char rightoutfilename[PIG_MAX_FILENAME_SIZE];
    zvpone("OUT", leftoutfilename, 1, PIG_MAX_FILENAME_SIZE);
    zvpone("OUT", rightoutfilename, 2, PIG_MAX_FILENAME_SIZE);
    if ((fds[0] = fopen(leftoutfilename, "w")) == NULL) {
        char msg[256 + PIG_MAX_FILENAME_SIZE];
        snprintf(msg, 256 + PIG_MAX_FILENAME_SIZE, "Error opening output file %s\n", leftoutfilename);
        zvmessage(msg, "");
        zabend();
    }
    if ((fds[1]= fopen(rightoutfilename, "w")) == NULL) {
        char msg[256 + PIG_MAX_FILENAME_SIZE];
        snprintf(msg, 256 + PIG_MAX_FILENAME_SIZE, "Error opening output file %s\n", rightoutfilename);
        zvmessage(msg, "");
        zabend();
    }
}

