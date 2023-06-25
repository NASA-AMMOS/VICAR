#include "vicmain_c"
#include "applic.h"
#include <string.h>
#include<math.h>
#include<stdio.h>

#include "PigMission.h"
#include "PigLabelModel.h"
#include "PigFileModel.h"
#define SQR(x) (x)*(x)
#define MAX_INPUTS 2

//TODO
#include <iostream>


////////////////////////////////////////////////////////////////////////
// Program to  compare a L->R and R->L image
// Requires 2 single band disparity input.

void main44()
{

    int unit_in[2];                               //input disparity images
    int unit_out;                                 //output disparity mask
    int nlin[2],nsin[2];                          //image dimensions
    int unit_quality[MAX_INPUTS];                 //input quality images
    float *line_buf_left, *samp_buf_left;         //input disparity image data
    float  *line_buf_right, *samp_buf_right;
    double  min_dist;                             //minimum distance for a point to be accepted
    // all neighbor values must  be within this    distance of each other
    double o_thresh; 

    int i;
    int def;
    int status;
    int count;
    int nids;

    zveaction("SA", "");

    // Open and read in the input

    zvpcnt("INP", &nids);
    if (nids > MAX_INPUTS) {
        zvmessage("Too many Input images.", "");
        zabend();
    }

    for (i=0; i < nids; i++) {
        zvunit(unit_in + i, "INP", i+1, NULL);
        status=zvopen(unit_in[i], "OP", "READ",
            "U_FORMAT", "REAL", "OPEN_ACT", "AS", NULL);
        if (status != SUCCESS) {
            zvmessage("Couldnt open input images", "");
            zabend();
        }
        zvget(unit_in[i], "NL", nlin + i , "NS", nsin+i, NULL);
    }



    // Do we need to scale the thresholds w.r.t a scale difference between the 
    // L and R images
    double scaleFactor = 1;
    if (zvptst("SCALING")) {   
       status = zlget(unit_in[0], "PROPERTY", "CORRELATION_AVERAGE_SCALE", 
                      &scaleFactor, "FORMAT", "DOUB", "ERR_ACT", "", 
                      "PROPERTY", "DERIVED_IMAGE_PARMS", NULL);

       if (scaleFactor < 1 && scaleFactor != 0) 
          scaleFactor = 1.0 / scaleFactor;
    }


    zvparmd("MINDIST", &min_dist, &count, &def, 1, 0);
    min_dist = sqrt(min_dist*min_dist);
    zvparmd("O_THRESH", &o_thresh, &count,&def, 1, 0);

    // Adjust min_dist and o_thresh with scale factor 
    // Default is 1
    min_dist *= scaleFactor;
    o_thresh *= scaleFactor;


    int size= nlin[0] * nsin[0];
    int nl, ns;                                   //image dimensions
    nl = nlin[0];
    ns = nsin[0];

    int sizer = nlin[1] * nsin[1];
    int nlr, nsr;
    nlr = nlin[1];
    nsr = nsin[1];

    line_buf_left = new float[size];
    samp_buf_left = new float[size];

    line_buf_right = new float[sizer];
    samp_buf_right = new float[sizer];

    if (line_buf_left == NULL || samp_buf_left == NULL ||
    line_buf_left == NULL || samp_buf_left == NULL ) {
        zvmessage("out of memory allocating output buffer!","");
        zabend();
    }

    // read in the left/right disparity images
    for (int j=0; j < nl; j++) {
        zvread(unit_in[0],line_buf_left + j*ns , "line", j+1, "band", 1, NULL);
        zvread(unit_in[0], samp_buf_left + j*ns, "line", j+1, "band", 2, NULL);
    }

    for (int j=0; j < nlr; j++) {
        zvread(unit_in[1],line_buf_right+j*nsr , "line", j+1, "band", 1, NULL);
        zvread(unit_in[1], samp_buf_right + j*nsr, "line", j+1, "band", 2,NULL);
    }

    char filename[256];
    int qlrcount,qrlcount;
    //name of the L->R disparity image
    zvp("QUALITYLR", filename, &qlrcount);
    if (qlrcount == 1) {
        zvunit(unit_quality, "QUAL_LR",1, "U_NAME", filename, NULL);
        status=zvopen(unit_quality[0], "OP", "READ",
            "U_FORMAT", "REAL", "OPEN_ACT", "AS", NULL);
    }

    //name of the R->L disparity image
    zvp("QUALITYRL", filename, &qrlcount);
    if  (qrlcount == 1) {
        zvunit(unit_quality + 1, "QUAL_RL",1, "U_NAME", filename, NULL);
        status=zvopen(unit_quality[1], "OP", "READ",
            "U_FORMAT", "REAL", "OPEN_ACT", "AS", NULL);
    }

    float quality_thresh[2];
    int thresh_count=0;
    //values (1 or 2)  that quality values must be greater than
    if ( qlrcount ==1 || qrlcount == 1) {
        zvp("QUALITY_THRESH",quality_thresh , &thresh_count);
    }

    // check quality image sizes
    if(qlrcount) {
        int nlin_quality,nsin_quality;
        zvget(unit_quality[0], "NL", &nlin_quality , "NS", &nsin_quality, NULL);
        if (nlin_quality != nl ||  nsin_quality != ns) {
            zvmessage("Image sizes must agree", "");
            zabend();
        }

    }

    if(qrlcount) {
        int nlin_quality,nsin_quality;
        zvget(unit_quality[1], "NL", &nlin_quality , "NS", &nsin_quality, NULL);
        if (nlin_quality != nlr ||  nsin_quality != nsr) {
            zvmessage("Image sizes must agree", "");
            zabend();
        }

    }

    float *quality_buf_left,*quality_buf_right;

    if(qlrcount) {
        quality_buf_left = new float[size];
        for (int j=0; j < nl; j++) {
            zvread(unit_quality[0],quality_buf_left+j*ns , "line", j+1,
                "band", 1, NULL);
        }
    }

    if(qrlcount) {
        quality_buf_right = new float[sizer];
        for (int j=0; j < nlr; j++) {
            zvread(unit_quality[1],quality_buf_right+j*nsr , "line", j+1,
                "band", 1, NULL);
        }
    }

   
    char filename_1[PIG_MAX_FILENAME_SIZE+1];
    char filename_2[PIG_MAX_FILENAME_SIZE+1];
    PigFileModel *file_models[MAX_INPUTS];
    PigMission *m = NULL;

    // close the input files first
    zvclose(unit_in[0], "CLOS_ACT", "FREE", NULL);

    // change the default error handling action
    // to ignore any errors in vicar image i/o
    zveaction("", "");

    // Create PIG mission object based on the first input file
    zvpone("INP",filename_1,1,sizeof(filename_1));
    m = PigMission::getMissionObject(filename_1, &unit_in[0]);
    if (m) { //if we were able to create Mission Object
       file_models[0] = m->createFileModel(filename_1, unit_in[0]);
       status = zvpone("INP",filename_2,2,sizeof(filename_2));
       if (status)
            file_models[1] = m->createFileModel(filename_2, unit_in[1]);
    }
    
    //open output image
    zvunit(&unit_out, "OUT",1, NULL);
    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "BYTE", "O_FORMAT", "BYTE",
        "U_NS", ns, "U_NL", nl, "OPEN_ACT", "AS",
        "U_NB", 1, "U_ORG", "BSQ", "METHOD", "RANDOM", "LAB_ACT", "", NULL);
    zvplabel(unit_out, 0, 1);

    // write out label items specific to this program.
    PigLabelModel *label_model = NULL;
    if (m) {
    	label_model = m->createLabelModel(unit_out);
    	label_model->setMask(file_models, 2, NULL, NULL, NULL, FALSE, 0.0);
    }

    // switch back to original error handling actions.
    zveaction("SA", ""); 
    
#define SAMP_BUF_RIGHT(j,k)  samp_buf_right[(j)*nsr + (k)]
#define LINE_BUF_RIGHT(j,k)  line_buf_right[(j)*nsr + (k)]
#define SAMP_BUF_LEFT(j,k)  samp_buf_left[(j)*ns + (k)]
#define LINE_BUF_LEFT(j,k)  line_buf_left[(j)*ns + (k)]
#define QUALITY_BUF_LEFT(j,k)  quality_buf_left[(j)*ns + (k)]
#define QUALITY_BUF_RIGHT(j,k)  quality_buf_right[(j)*nsr + (k)]
    double right_sample,right_line;
    double dn_sample,dn_line;
    unsigned char *bbuf;
    bbuf = new unsigned char[ns];
    //FILE *fp;
    //fp=fopen("dists","w");
    for (int j=0; j < nl; j++) {
        // mask value of 255 means a point should be ignored
        memset(bbuf,255,ns);
        for (int k=0; k < ns; k++) {

            if(qlrcount && QUALITY_BUF_LEFT (j,k) < quality_thresh[0]) continue;
            right_sample=SAMP_BUF_LEFT(j,k);
            right_line=LINE_BUF_LEFT(j,k);
            if(right_sample < 1 || right_sample > nsr -2  ||
                right_line < 1 || right_line > nlr -2 ) continue;

            int right_sample_trunc = (int) right_sample;
            int right_line_trunc = (int) right_line;

            if(qrlcount) {
                int thresh_index;
                if(thresh_count == 1) thresh_index=0;
                else thresh_index=1;
                // require all 4 neighbors to satisfy quality threshold
                if(QUALITY_BUF_RIGHT((int)right_line,(int)right_sample) <
                    quality_thresh[thresh_index] ||
                    QUALITY_BUF_RIGHT((int)right_line,(int)right_sample+1) <
                    quality_thresh[thresh_index] ||
                    QUALITY_BUF_RIGHT((int)right_line+1,(int)right_sample) <
                    quality_thresh[thresh_index] ||
                    QUALITY_BUF_RIGHT((int)right_line+1,(int)right_sample+1) <
                    quality_thresh[thresh_index]
                    )  continue;
            }

            double wr = right_sample - right_sample_trunc;
            double wl = 1.0 - wr;
            double wb = right_line - right_line_trunc;
            double wt= 1.0 - wt;
            double top, bot;
            double first_point=SAMP_BUF_RIGHT(right_line_trunc,right_sample_trunc);
            //check for outliers
            if(fabs(first_point-SAMP_BUF_RIGHT(right_line_trunc,right_sample_trunc+1))
		> o_thresh ||
            fabs(first_point-SAMP_BUF_RIGHT(right_line_trunc+1,right_sample_trunc+1))
		> o_thresh ||
            fabs(first_point-SAMP_BUF_RIGHT(right_line_trunc+1,right_sample_trunc))
		> o_thresh) continue; 

            top = wl*SAMP_BUF_RIGHT(right_line_trunc,right_sample_trunc) +
                wr*SAMP_BUF_RIGHT(right_line_trunc,right_sample_trunc+1);
            bot  = wl*SAMP_BUF_RIGHT(right_line_trunc+1,right_sample_trunc) +
                wr*SAMP_BUF_RIGHT(right_line_trunc+1,right_sample_trunc+1);
            dn_sample = (bot*wb + top * (1.0-wb));

            //check for outliers
            first_point=LINE_BUF_RIGHT(right_line_trunc,right_sample_trunc);
            if(fabs(first_point-LINE_BUF_RIGHT(right_line_trunc,right_sample_trunc+1))
		> o_thresh ||
            fabs(first_point-LINE_BUF_RIGHT(right_line_trunc+1,right_sample_trunc+1))
		> o_thresh ||
            fabs(first_point-LINE_BUF_RIGHT(right_line_trunc+1,right_sample_trunc))
		> o_thresh) continue; 
            //interpolate line values
            top = wl*LINE_BUF_RIGHT(right_line_trunc,right_sample_trunc) +
                wr*LINE_BUF_RIGHT(right_line_trunc,right_sample_trunc+1);
            bot  = wl*LINE_BUF_RIGHT(right_line_trunc+1,right_sample_trunc) +
                wr*LINE_BUF_RIGHT(right_line_trunc+1,right_sample_trunc+1);
            dn_line = (bot*wb + top * (1.0-wb));

            //fprintf(fp,"%f\n",sqrt(SQR(dn_sample - k) + SQR(dn_line - j)));
            if(sqrt(SQR(dn_sample - k) + SQR(dn_line - j)) < min_dist)
                bbuf[k] = 0;

        }
        zvwrit(unit_out, bbuf, "LINE", j+1,"BAND",1, NULL);
    }
    //cleanup
    zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
    zvclose(unit_in[0], "CLOS_ACT", "FREE", NULL);
    zvclose(unit_in[1], "CLOS_ACT", "FREE", NULL);
    if(qlrcount)
        zvclose(unit_quality[0], "CLOS_ACT", "FREE", NULL);
    if(qrlcount)
        zvclose(unit_quality[1], "CLOS_ACT", "FREE", NULL);

}
