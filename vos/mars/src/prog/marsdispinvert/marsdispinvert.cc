#include "vicmain_c"
#include <string.h>
#include "applic.h"
#include "PigMission.h"
#include "PigLabelModel.h"
#include "PigFileModel.h"

#define MAX_INPUTS 2
////////////////////////////////////////////////////////////////////////
// Program to invert a disparity map from L->R to R->L (or vice-versa).
// Requires 2-band disparity input.

void main44()
{
    int in_unit, out_unit;
    //char label_filename[256];
    int sl, ss, nl, ns, nlo, nso, nli, nsi;
    float *line_buf, *samp_buf;
    int count;
    int nids;
    int unit_in[MAX_INPUTS];
    int num_bands = 2;
    char filename[PIG_MAX_FILENAME_SIZE+1];
    int disp_pyr = 0;
    int disp_zoom = 1;
    char msg[2048];


    zveaction("SA", "");

    zvpcnt("IN_DISP", &nids);
    if (nids != 1) {
        zvmessage("There should be 1 Input Disparity Image.", "");
        zabend();
    }

    zvp("DISP_PYRAMID", &disp_pyr, &count);
    disp_zoom = 1 << disp_pyr;			// convert to zoom factor
    snprintf(msg, 2048, "Disparity zoomed by %d (pyramid level %d)",
					disp_zoom, disp_pyr);
    zvmessage(msg, "");

    // Open the input disparity

    zvpone("IN_DISP", filename, 1, PIG_MAX_FILENAME_SIZE);
    zvunit(&in_unit, "IN_DISP", 1, "u_name", filename, NULL);
    zvopen(in_unit, "OP", "READ", "U_FORMAT", "REAL", NULL);
    char format[256];
    zvget(in_unit, "FORMAT", format, NULL);

    zvselpiu(in_unit);				// only for zvsize
    zvsize(&sl, &ss, &nl, &ns, &nli, &nsi);       // we don't use sl/ss

    // Check if input disparity labels contains CORRELATION characteristics
    double correlationAvgScale = 0;
    int status = zlget(in_unit, "PROPERTY", "CORRELATION_AVERAGE_SCALE", 
                       &correlationAvgScale, "FORMAT", "DOUB", "ERR_ACT", "", 
                       "PROPERTY", "DERIVED_IMAGE_PARMS", NULL);

    zvpcnt("INP", &nids);
    if (nids > MAX_INPUTS) {
    	zvmessage("Too many Input images.", "");
        zabend();
    }
    for (int i=0; i < nids; i++) {
    	zvunit(&unit_in[i], "INP", i+1, NULL);
        zvopen(unit_in[i], "OP", "READ",
                "U_FORMAT", "DOUB", "OPEN_ACT", "AS", NULL);
    }

    int unit_inp;
    char filename_inp[PIG_MAX_FILENAME_SIZE+1];
    PigMission *m = NULL;

    // Create PIG mission object based on the first input file
    status = zvpone("INP",filename_inp,1,sizeof(filename_inp));
    if (status) //success
        m = PigMission::getMissionObject(filename_inp, &unit_inp);

    // Open output file
    // Note that we use the label from the first input.  That's because that
    // one should be the same eye as we're going TO.  I.e. if we're inverting
    // a L eye to be a R eye, the inputs will be (R,L) so we want to use the
    // label from the R eye.  We also get the output size from the first
    // input, in case it's different.  But we adjust the size by the pyramid
    // level.

    zvselpiu(unit_in[0]);
    zvsize(&sl, &ss, &nlo, &nso, &nli, &nsi);       // we don't use sl/ss
    if (disp_zoom <= 0) disp_zoom = 1;		// just in case
    nlo /= disp_zoom;
    nso /= disp_zoom;

    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "OP", "WRITE", "U_FORMAT", "REAL", "O_FORMAT", format,
	"U_NS", nso, "U_NL", nlo, "U_NB", 2, NULL);


    // We only put integers in the output but the file is float so let's
    // be consistent.

    line_buf = new float[nlo * nso];
    samp_buf = new float[nlo * nso];

    if (line_buf == NULL || samp_buf == NULL) {
        zvmessage("out of memory allocating output buffer!","");
        zabend();
    }

    float sample_fill_value;
    float line_fill_value;
    int do_fill = FALSE;
    int counts,countl;
    zvp("S_FILL_VALUE", &sample_fill_value,&counts);
    zvp("L_FILL_VALUE", &line_fill_value,&countl);
    if (counts == 1 && countl == 1) do_fill = TRUE;

    memset(line_buf, 0, sizeof(float)*nlo*nso);
    memset(samp_buf, 0, sizeof(float)*nlo*nso);
    if (do_fill) {
        for (int j=0; j < nlo; j++) {
            for (int i = 0; i < nso; i++) {
                line_buf[j*nso+i]=i-sample_fill_value;
                samp_buf[j*nso+i]=j-line_fill_value;
                if (line_buf[j*nso+i] < 0) line_buf[j*nso+i]=0;
                if (line_buf[j*nso+i] > nlo-1) line_buf[j*nso+i]=nlo -1;
                if (samp_buf[j*nso+i] < 0) samp_buf[j*nso+i]=0;
                if (samp_buf[j*nso+i] > nso-1) samp_buf[j*nso+i]=nso -1 ;
            }
        }
    }

    // Process...

    float *in_line_disp = new float[ns];
    float *in_samp_disp = new float[ns];
    if (in_line_disp == NULL || in_samp_disp == NULL) {
        zvmessage("out of memory allocating input buffer!","");
        zabend();
    }

    // Counter for valid correlation
    long nbValid = 0;

    for (int j=0; j < nl; j++) {

        // Read the disparity line.

        zvread(in_unit, in_line_disp, "line", j+1, "band", 1, NULL);
        zvread(in_unit, in_samp_disp, "line", j+1, "band", 2, NULL);

        for (int i = 0; i < ns; i++) {

            if ((in_line_disp[i] == 0.0) && (in_samp_disp[i] == 0.0)) {
                continue;                         // no input disparity
            }

            // Retrieve disp value and round it off

            int line = (int)((in_line_disp[i]-1) + 0.5);
            int samp = (int)((in_samp_disp[i]-1) + 0.5);

            // The disp value SHOULD be within the bounds of the image... but...

            if (line <= 0 || samp <= 0) {         // sanity check
                continue;
            }
            // sanity check
            if (line >= (nlo-1) || samp >= (nso-1)) {
                continue;
            }

            // Store the disparity in the output map
            line_buf[line*nso + samp] = j+1;
            samp_buf[line*nso + samp] = i+1;

            // That pixel was valid, increment counter
            nbValid++;
        }
    }

    // Now write the output disparity map.  We write line and then sample
    // just for efficiency (to avoid random seeks in the file).

    for (int j=0; j < nlo; j++) {
        zvwrit(out_unit, &line_buf[j*nso], "line", j+1, "band", 1, NULL);
    }
    for (int j=0; j < nlo; j++) {
        zvwrit(out_unit, &samp_buf[j*nso], "line", j+1, "band", 2, NULL);
    }


    zvplabel(out_unit, 0, 1);
    PigFileModel *file_models[MAX_INPUTS+1];
    for (int i = 0; i < MAX_INPUTS + 1; i++)
        file_models[i] = NULL;
    if (m)  {// if we were able to create Mission object
        for (int i = 0; i < nids; i++)
            file_models[i] = m->createFileModel("", unit_in[i]);
        file_models[nids] = m->createFileModel("", in_unit);  //disparity

        PigLabelModel *labelModel_0 = NULL;
        labelModel_0 = m->createLabelModel(out_unit);

        if (labelModel_0) {
            labelModel_0->setDisparity(file_models, file_models[1], nids+1, 
                                       "DISPARITY_MAP");
            // Adding Disparity Extra if present in input disparity
            if (correlationAvgScale) {
               labelModel_0->setDisparityExtra(nbValid, 
                                               (double) (1.0/correlationAvgScale), 
                                               (double)nbValid / (nl*ns) * 100.,
                                               -1);

            }
        }
    }


    zvclose(in_unit, NULL);
    zvclose(out_unit, NULL);

}

