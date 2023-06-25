/* marsmask */
using namespace std;

#include "vicmain_c"


#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"

#include "return_status.h"


/* buffer sizes in main program */
#define MAX_INPUTS 1
#define MAX_NS 4096
#define MAX_NL 4096

#define MAX_BANDS 16

////////////////////////////////////////////////////////////////////////


void main44()
{
    int i, j;
    int status, count, def;
    const size_t msgLen = 250;
    char msg[msgLen];

    int nids;
    //char mission[64], instrument[64];
    char filename[PIG_MAX_FILENAME_SIZE+1];
    int nl, ns, nb, nb_mask;
    int line, samp;
    int sl, ss;
    int nli, nsi;
    int band;
    double* inp[MAX_BANDS];    //input image data
    double *out_d[MAX_BANDS];  // output image
    unsigned char *mask[MAX_BANDS];
    int inp_unit,mask_unit;
    //unsigned char *mask_image;
    int out_unit;
    char mask_filename[PIG_MAX_FILENAME_SIZE];
    char input_filename[PIG_MAX_FILENAME_SIZE];
    int invert_mask = 0;
    int use_image_label = 0;		// default to mask

    // Outputs

    int out_band[MAX_BANDS];
    char format[10];                   // input data format

    // User Parameters

    // Images

    zvmessage("MARSMASK version 1", "");

    zvpone("INP", input_filename, 1, PIG_MAX_FILENAME_SIZE);
    
    zvunit(&inp_unit, "INP", 1, NULL);
    zvopen(inp_unit, "op", "read", "open_act", "sa",
                "io_act", "sa", "u_format", "doub", NULL);
    
    zvget(inp_unit, "FORMAT", format, "NB", &nb, NULL);
    zvp("MASK", mask_filename, &count);
    zvunit(&mask_unit, "MASK", 1, "u_name", mask_filename, NULL);
    zvopen(mask_unit, "op", "read", "open_act", "sa",
                "io_act", "sa", "u_format", "byte", NULL);

    zvget(mask_unit, "NB", &nb_mask, "NAME", mask_filename, NULL);
    //zvmessage(mask_filename, "");
    //zvmessage(msg, "");
    if (nb > MAX_BANDS) {
	zvmessage("Too many bands in input file","");
	zabend();
    }
    if (nb_mask > 1 && (nb_mask != nb)) {
        snprintf(msg, msgLen, "\nWarning: Number of Bands in the mask(%d) is not equal\n \
to the number of bands in the input image(%d),\n will use only Band 1 \
of the mask and ignore the other bands.\n", nb_mask, nb);
        zvmessage(msg, "");
        nb_mask = 1;
    }
    //Get the dimensions of the input 
    zvsize(&sl, &ss, &nl, &ns, &nli, &nsi);       // we don't use sl/ss
 
    ////////////////////////////////////////////////////////////////////////
    // Allocate memory for input and mask, and output image.  We read
    // the inputs a line at a time but keep the entire output in memory due to
    // the band-sequential layout and the all-bands-for-each-pixel calculation.

    for (i=0; i<nb; i++) {
        inp[i] = (double *)malloc(ns * sizeof(double));
        mask[i] = (unsigned char *)malloc(ns * sizeof(unsigned char));
        if (inp[i] == NULL || mask[i] == NULL) {
            snprintf(msg, msgLen, "Unable to allocate memory for Input or Mask Image %d", i);
            zvmessage(msg, "");
            zabend();
        }
    }

    for (i=0; i<nb; i++) {
        out_d[i] = (double *)malloc(nl * ns * sizeof(double));
            if (out_d[i] == NULL) {
                snprintf(msg, msgLen, "Unable to allocate memory for output band %d", i);
                zvmessage(msg, "");
                zabend();
            }
    }

    invert_mask = zvptst("INVERT");
    use_image_label = zvptst("IMAGE");

    ////////////////////////////////////////////////////////////////////////
    // Go through the images a line at a time...

    double dn;
    int pixel_count[MAX_BANDS];
    for (i=0; i < nb; i++)
	pixel_count[i] = 0;

    for (line = 0; line < nl; line++) {

        if (line % 100 == 0) {
            snprintf(msg, msgLen, "Line %d", line);
            zvmessage(msg, "");
        }


        // Read in the input file

        for (band = 0; band < nb; band++) {
            zvread(inp_unit, inp[band],
                        "BAND", band+1, "LINE", line+1, NULL);
            // Read in the mask file...
            if (nb_mask ==1)
                zvread(mask_unit, mask[band], "BAND", 1, "LINE", line+1, NULL);
            else
                zvread(mask_unit, mask[band], "BAND", band+1, "LINE", line+1, NULL);
            for (int samp = 0; samp < ns; samp++) {
                if (!invert_mask)
                    dn = inp[band][samp]*(mask[band][samp]==0);
                else
                    dn = inp[band][samp]*(mask[band][samp]!=0);
		*(out_d[band] + line*ns + samp) = dn;
		if (dn != 0)
		    pixel_count[band]++;

            }               // end sample loop
        }                   // end band loop
    }                       // end line loop

    ////////////////////////////////////////////////////////////////////////
    // Open output file.
    // OUT is a single 3-band file...

    if (use_image_label)
	zvselpiu(inp_unit);
    else
	zvselpiu(mask_unit);

    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
               "u_ns", ns, "u_nl",nl,
               "u_nb", nb,
               "open_act", "sa", "u_org", "bsq",
               "u_format", "doub", "o_format", format, NULL);
    zvplabel(out_unit, 0, 1);

    // Write out the necessary label items.
    PigMission* m = PigMission::getMissionObject(input_filename, &inp_unit);
    PigFileModel *file_models[2];
    PigLabelModel *labelModel = NULL;
    if (m) {  //if we were able to create Mission object
        file_models[0] = m->createFileModel(input_filename, inp_unit);   
        file_models[1] = m->createFileModel(mask_filename, mask_unit);
        labelModel = m->createLabelModel(out_unit);

	labelModel->setMasked(file_models, 2);

    }
  
    if (zvptst("CORR")) {		// output pixel count

	// Get the minimum count across all bands - they really should
	// all be the same for a disparity map, but just in case not...
	int pix_count = pixel_count[0];
	for (i=1; i < nb; i++) {
	    if (pix_count > pixel_count[i])
		pix_count = pixel_count[i];
	}

	// Get the average scale if needed (if we're using the image
	// label this is not needed because it's already there)

	double avg_scale = 0;
	if (!use_image_label) {
	    if (file_models[0] != NULL)
		avg_scale = file_models[0]->getCorrelationAverageScale(0);
	}

	// Add to label

	if (labelModel != NULL)
	    labelModel->setDisparityExtra(pix_count, avg_scale, -1.0, -1);
    }

    ////////////////////////////////////////////////////////////////////////
    // Write out masked file...

    for (band = 0; band < nb; band++) {
        for (line = 0; line < nl; line++) {
            zvwrit(out_unit, out_d[band] + (line * ns),
                        "BAND", band+1, "LINE", line+1, NULL);
        }
    }

    zvclose(out_unit, NULL);

}
