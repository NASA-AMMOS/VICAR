////////////////////////////////////////////////////////////////////////
// mars_read_inputs.cc
//
// Reads a set of input files into memory, and optionally marks their
// borders (for footprinting).
//
// This routine requires the following parameters be in the PDF:
//
// PARM FOOTPRT TYPE=KEYWORD VALID=("NOFOOTPRINT", "FOOTPRINT", "OVERLAP") +
//		DEFAULT=NOFOOTPRINT
// PARM FOOTDN TYPE=INTEGER DEFAULT=4096
// PARM DNSCALE_IN TYPE=KEYWORD COUNT=1 VALID=("AUTOSCALE", "RESCALE", "NOSCALE") +
//              DEFAULT=AUTOSCALE
// PARM DNSCALE_OUT TYPE=KEYWORD COUNT=1 VALID=("STATIC", "DYNAMIC", "IDENTITY") +
//              DEFAULT=STATIC
//
// (only the FOOTPRINT keyword is used from FOOTPRT).  If the parameters
// are not present, no footprinting will be done.
// You can avoid freeing the input units by setting free_units to FALSE
// (in case they need to be re-used).
// If the radiometry model array itself is NULL, no rad correction will
// If an individual element is NULL, no correction will be done on that 
// file.  mars_setup() examines the RAD parameter to turn this on and off.
//
// This routine has the capability of reading a set of input files into
// SimpleImage objects, short int buffers, and float buffers.  
//
// If radiometric model exists and is provided, then radiometric correction
// will be performed on the input files.
//
// If brightness model exists and is provided, then brightness correction
// will be performed on the input files.
////////////////////////////////////////////////////////////////////////
#include "mars_support.h"
#include "PigFileModel.h"
#include "RadiometryModel.h"
#include "PigBrtCorrModel.h"
#include "SimpleImage.h"
#include "zvproto.h"

#define MAX_OPEN 2000           // Largest size for max_open
#define MAX_NB 3	

////////////////////////////////////////////////////////////////////////
// Note: Callers of this function either need to initialize SimpleImage
// pointers array to NULL or supply pre-allocated SimpleImage pointers
// array. Callers are responsible for supplying SimpleImage in the correct 
// type.
//
// Reads a set of input files into memory, and optionally marks their
// borders (for footprinting).
//
// PARM FOOTPRT TYPE=KEYWORD VALID=("NOFOOTPRINT", "FOOTPRINT", "OVERLAP") +
//              DEFAULT=NOFOOTPRINT
// PARM FOOT_DN TYPE=REAL DEFAULT=4096 COUNT=(1:32)
// PARM DNSCALE_IN TYPE=KEYWORD COUNT=1 VALID=("AUTOSCALE", "RESCALE", "NOSCALE") + 
//              DEFAULT=AUTOSCALE
// PARM DNSCALE_OUT TYPE=KEYWORD COUNT=1 VALID=("STATIC", "DYNAMIC", "IDENTITY") + 
//              DEFAULT=STATIC
//
// See the example usages below.  In the latter case, the SimpleImage
// itself must be pre-allocated, but it will call alloc() to make sure there's
// enough storage (which does nothing if there is, or re-alloc's if not).
//
// There are three versions of this function in this routine,
// one handles SimpleImage, and one handles short int type, and one handles
// float type. This is the full API, and the other two are legacy APIs left
// there for backward compatibility. New users should use this version. 
//
// Example usage 1: read short int images with SimpleImage pointer array
//                  set to NULL.
//     SimpleImage<short int> *short_int_images[MAX_INPUTS];
//     memset(short_int_images, 0, sizeof(short_int_images));
//     mars_read_inputs(first_input, last_input, files, short_int_images,  
//                 MAX_NL, MAX_NS, band, radiometric, brt_corr);
// 
// Example usage 2: read float images with SimpleImage pointer array set
//                  to NULL.
//     SimpleImage<float> *float_images[MAX_INPUTS];
//     memset(float_images, 0, sizeof(float_images));
//     mars_read_inputs(first_input, last_input, files, float_images, 
//                 MAX_NL, MAX_NS, band, radiometric, brt_corr);
//   
// Example usage 3: read short int images with pre-allocated SimpleImage
//                  pointer array.
//     SimpleImage<short int> *short_int_images[MAX_INPUTS];
//     short int *ibuf[MAX_INPUTS];
//     
//     for (int i = first_input; i<= last_input; i++) {
//         ibuf[i - first_input] = new short int[fileNL * fileNS];
//         short_int_images[i - first_input] = new SimpleImage<short int>
//                           (ibuf[i - first_input],fileNL, fileNS, fileNS);
//     }
//     
//     mars_read_inputs(first_input, last_input, files, short_int_images,
//                 MAX_NL, MAX_NS, band, radiometric, brt_corr);
//
// If bands is given as 0, it reads all bands from the file into a 3-D
// SimpleImage (if there's only one band, that's the same as a 2-D SI).
// If bands is > 0, only the one specified band is read.  If you pre-allocate
// your SI, keep this in mind!
//
// If radiometric model exists and is provided, then radiometric correction
// will be performed on the input files.
// 
// If brightness model exists and is provided, then brightness correction
// will be performed on the input files.
//
// If an element of the input (PigFileModel *) array is NULL, then that
// element is ignored, and its corresponding element in the output
// (SimpleImage<T> *) array is NULL.
////////////////////////////////////////////////////////////////////////
template <class T>
void mars_read_inputs(int first_input, int last_input, PigFileModel *files[],
                 SimpleImage<T> *images[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr[])
{   
    int fileNB, fileNL, fileNS;
    int nb;
    int count;
    float footDN[MARS_MAX_NB];
    float dn;
    const char *vicarType;
    SimpleImage<T> *imagePtr;

    int footprint = zvptst("FOOTPRINT");
    zvp("FOOT_DN", footDN, &count);

    int do_rad_scaling = FALSE;
    if ((zvptst("AUTOSCALE") && (!(zvptst("norad") && zvptst("STATIC")))) || 
         zvptst("RESCALE")) {
        do_rad_scaling = TRUE;
    }

    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        fileNB = files[i]->getNB();
	nb = fileNB;
	int sb = band;
	if (band == 0)
	    sb = 1;		// read all bands, start at 1
	else
	    nb = 1;		// force to a single band
        fileNL = files[i]->getNL();
        fileNS = files[i]->getNS();

        if (images[i - first_input] == NULL) {
            images[i - first_input] = new SimpleImage<T>(nb, fileNL,fileNS);
        } else {
	    // Re-alloc's if not big enough; does nothing otherwise
	    images[i - first_input]->alloc(nb, fileNL, fileNS);
        }
        imagePtr = images[i - first_input];

        vicarType = imagePtr->getVicarTypeString();
        files[i]->closeFile();
        zvopen(files[i]->getUnit(), "OP", "READ", "U_FORMAT", vicarType,
                            "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
        files[i]->setFileOpen(TRUE);

	for (int b = 0; b < nb; b++) {
	    int bb = b+sb;
	    if (bb > fileNB)
		bb = fileNB;	// Keep reading last band
            for (int line = 0; line < fileNL; line++) {
                zvread(files[i]->getUnit(), imagePtr->linePtr(b,line),
				"BAND", bb, "LINE", line + 1, NULL);
            }

            // If there is a radiometry model, use it.
            if (radiometric != NULL && radiometric[i] != NULL) {
		//!!!! Need multi-band rad correction eventually!!!!
                radiometric[i]->applyCorrection(imagePtr->linePtr(b,0),
		    fileNL, fileNS, bb-1, files[i]->hasRadiometricCorrectionLabel());
            }

            // If do_rad_scaling flag is on, then we convert the integer values
            // to the true radiance values (float). Callers are responsible
            // for supplying SimpleImage in the correct type.
            if (do_rad_scaling && files[i]->canConvertRADToFloat()) {
                float radiance_offset = files[i]->getRadianceOffset(0.0);
                float radiance_scale_factor =
                                       files[i]->getRadianceScaleFactor(1.0);

                for (int line = 0; line < fileNL; line++) {
                    for (int samp = 0; samp < fileNS; samp++) {
                        dn = imagePtr->get(b, line, samp);
                        dn = dn * radiance_scale_factor + radiance_offset;
                        imagePtr->set(b, line, samp, (T)dn);
                    }
                }
            }

            // If there is a brightness correction model, use it.
            if (brt_corr != NULL && brt_corr[i] != NULL) {
                brt_corr[i]->applyCorrection(imagePtr->linePtr(b,0),
						fileNL, fileNS, bb-1);
            }

            if (footprint) {
                double sl, ss, el, es;
                int isl, iss, iel, ies;
           
		float foot_dn = footDN[count-1];
		if (b < count)
		     foot_dn = footDN[b];

                files[i]->getImageBorders(sl, ss, el, es);
                isl = (int)(sl - files[i]->getYOffset() + 0.5);
                iss = (int)(ss - files[i]->getXOffset() + 0.5);
                iel = (int)(el - files[i]->getYOffset() + 0.5);
                ies = (int)(es - files[i]->getXOffset() + 0.5);

                for (int line = 0; line < fileNL; line++) {
                    imagePtr->set(b, line, iss, (T)foot_dn);
                    imagePtr->set(b, line, ies, (T)foot_dn);   
                }

                for (int samp = 0; samp < fileNS; samp++) {
                    imagePtr->set(b, isl, samp, (T)foot_dn);
                    imagePtr->set(b, iel, samp, (T)foot_dn); 
                }
            }
        }
        files[i]->closeFile();

    }
}

////////////////////////////////////////////////////////////////////////
// Reads a set of input files into memory, returning the intensity part of
// an HSI color space transform on the input.  The intensity is defined as
// the max value across all bands.  The standard mars_read_inputs is called
// for each band, so rad and brt correction are applied.  Then the max value
// is determined and returned.
//
// The band parameter is currently ignored but should be passed in as 3
// for future expansion (hue=1, saturation=2, intensity=3).
//
// Note: Callers of this function either need to initialize SimpleImage
// pointers array to NULL or supply pre-allocated SimpleImage pointers
// array. See the example usages below.
//
// Example usage 1: with SimpleImage pointer array set to NULL.
//     SimpleImage<short int> *short_int_images[MAX_INPUTS];
//     memset(short_int_images, 0, sizeof(short_int_images));
//     mars_read_inputs_hsi(first_input, last_input, files, short_int_images, 
//                 MAX_NL, MAX_NS, band, radiometric, brt_corr);
//     
// Example usage 2: with pre-allocated SimpleImage pointer array.
//     SimpleImage<short int> *short_int_images[MAX_INPUTS];
//     short int *ibuf[MAX_INPUTS];
//     
//     for (int i = first_input; i<= last_input; i++) {
//         ibuf[i - first_input] = new short int[fileNL * fileNS];
//         short_int_images[i - first_input] = new SimpleImage<short int>
//                             (ibuf[i - first_input], fileNL, fileNS, fileNS);
//     }
//     
//     mars_read_inputs_hsi(first_input, last_input, files, short_int_images,
//                 MAX_NL, MAX_NS, band, radiometric, brt_corr);
//
// If an element of the input (PigFileModel *) array is NULL, then that
// element is ignored, and its corresponding element in the output
// (SimpleImage<T> *) array is NULL.
////////////////////////////////////////////////////////////////////////
template <class T>
void mars_read_inputs_hsi(int first_input, int last_input, PigFileModel *files[],
                 SimpleImage<T> *siImages[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr[])
{
    int fileNL, fileNS, fileNB;
    SimpleImage<T> *imagePtr;

    //Create buffers for the bands
    int max_nb = 0;
    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        if (files[i]->getNB() > max_nb) {
            max_nb = files[i]->getNB();
        }
    }

    if (max_nb > MAX_NB) {
        zvmessage("Too many bands in mars_read_inputs_hsi", "");
        return;
    }

    if (last_input >= MAX_OPEN) {
        zvmessage("Too many inputs in mars_read_inputs_hsi", "");
        return;
    }

    SimpleImage<T> *temp_img = new SimpleImage<T>(max_nb,
				files[0]->getNL(), files[0]->getNS());

    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        fileNL = files[i]->getNL();
        fileNS = files[i]->getNS();
        fileNB = files[i]->getNB();

	// Re-alloc if needed
	temp_img->alloc(max_nb, fileNL, fileNS);

        if (siImages[i - first_input] == NULL) {
            siImages[i - first_input] = new SimpleImage<T>(fileNL, fileNS);
        } else {
	    // Re-alloc's if not big enough; does nothing otherwise
	    siImages[i - first_input]->alloc(fileNL, fileNS);
        }
        imagePtr = siImages[i - first_input];

        //Read all bands (band=0 does this).
        mars_read_inputs(0, 0, &files[i], &temp_img, fileNL, fileNS, 0, 
                         &radiometric[i], &brt_corr[i]); 

        //Now compute and return the max.
        for (int line = 0; line < fileNL; line++) {
            for (int samp = 0; samp < fileNS; samp++) {
                T max = 0;

                for (int b = 0; b < fileNB; b++) {
                    T dn = temp_img->get(b, line, samp);

                    if (dn > max) {
                        max = dn;
                    }
                }

                imagePtr->set(line, samp, max);
            }
        }

        temp_img->free();
    }
}

////////////////////////////////////////////////////////////////////////
// Handles short int type buffer. If radiometric and brt_corr models exist, 
// then radiometric and brightness corrections will be performed on input
// images. 
// 
// NOTE: New users should use the SimpleImage version. This is a legacy API 
// for mars_read_inputs. Left here for backward compatibility.
//
// DO NOT attempt to use this with multiple bands (band=0).  The allocations
// are not set up to support this.  Use the SimpleImage version instead.
//
// Example Usage:
//     short int *ibuf[MAX_INPUTS];
//     mars_read_inputs(first_input, last_input, files, ibuf, MAX_NL,
//                 MAX_NS, band, radiometric, brt_corr);
////////////////////////////////////////////////////////////////////////
void mars_read_inputs(int first_input, int last_input, PigFileModel *files[],
                 short int *ibuf[], int MAX_NL, int MAX_NS, int band, 
                 RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr[])
{
    int fileNL, fileNS;
    SimpleImage<short int> *siImages[last_input - first_input + 1];

    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        fileNL = files[i]->getNL();
        fileNS = files[i]->getNS();
        siImages[i - first_input] = new SimpleImage<short int>(
            ibuf[i - first_input], fileNL, fileNS, fileNS);
    }

    mars_read_inputs(first_input, last_input, files, siImages, MAX_NL, MAX_NS, 
                     band, radiometric, brt_corr);

    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        delete siImages[i - first_input];
    }
}

////////////////////////////////////////////////////////////////////////
// Handles float type buffer. No radiometric correction. If brt_corr model 
// exist, then brightness correction will be performed on input images.
//
// Note: New users should use the SimpleImage version. This is a legacy API 
// for mars_read_inputs. Left here for backward compatibility.
//
// DO NOT attempt to use this with multiple bands (band=0).  The allocations
// are not set up to support this.  Use the SimpleImage version instead.
//
// Example Usage:
//     float *fbuf_x[MAX_INPUTS];
//     float *fbuf_y[MAX_INPUTS];
//     float *fbuf_z[MAX_INPUTS];
//     mars_read_inputs(first_input, last_input, files, fbuf_x, MAX_NL,
//                 MAX_NS, 1, radiometric, brt_corr);
//     mars_read_inputs(first_input, last_input, files, fbuf_y, MAX_NL,
//                 MAX_NS, 2, radiometric, brt_corr);
//     mars_read_inputs(first_input, last_input, files, fbuf_z, MAX_NL,
//                 MAX_NS, 3, radiometric, brt_corr);
////////////////////////////////////////////////////////////////////////
void mars_read_inputs(int first_input, int last_input, PigFileModel *files[],
                 float *fbuf[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr[])
{

    int fileNL, fileNS;
    SimpleImage<float> *fImages[last_input - first_input + 1];

    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        fileNL = files[i]->getNL();
        fileNS = files[i]->getNS();
        fImages[i - first_input] = new SimpleImage<float>(fbuf[i - first_input],
                                                        fileNL, fileNS, fileNS);
    }

    mars_read_inputs(first_input, last_input, files, fImages, MAX_NL, MAX_NS, 
                     band, NULL, brt_corr);

    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        delete fImages[i - first_input];
    }
}

////////////////////////////////////////////////////////////////////////
// Handles float type buffer. If radiometric and brt_corr models exist, 
// then radiometirc and brightness correction will be performed on input 
// images.
//
// Note: New users should use the SimpleImage version. This is a legacy API 
// for mars_read_inputs. Left here for backward compatibility.
//
// DO NOT attempt to use this with multiple bands (band=0).  The allocations
// are not set up to support this.  Use the SimpleImage version instead.
//
// Example Usage:
//     float *fbuf_x[MAX_INPUTS];
//     float *fbuf_y[MAX_INPUTS];
//     float *fbuf_z[MAX_INPUTS];
//     mars_read_inputs_rad(first_input, last_input, files, fbuf_x, MAX_NL,
//                 MAX_NS, 1, radiometric, brt_corr);
//     mars_read_inputs_rad(first_input, last_input, files, fbuf_y, MAX_NL,
//                 MAX_NS, 2, radiometric, brt_corr);
//     mars_read_inputs_rad(first_input, last_input, files, fbuf_z, MAX_NL,
//                 MAX_NS, 3, radiometric, brt_corr);
////////////////////////////////////////////////////////////////////////
void mars_read_inputs_rad(int first_input,int last_input, PigFileModel *files[],
                 float *fbuf[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[], PigBrtCorrModel *brt_corr[])
{
    int fileNL, fileNS;
    SimpleImage<float> *fImages[last_input - first_input + 1];

    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        fileNL = files[i]->getNL();
        fileNS = files[i]->getNS();
        fImages[i - first_input] = new SimpleImage<float>(fbuf[i - first_input],
                                                        fileNL, fileNS, fileNS);
    }

    mars_read_inputs(first_input, last_input, files, fImages, MAX_NL, MAX_NS, 
                     band, radiometric, brt_corr);

    for (int i = first_input; i <= last_input; i++) {
        if (files[i] == NULL)
            continue;

        delete fImages[i - first_input];
    }
}


// Explicit instantiations of the template functions.  We instantiate for
// short int and float since that's likely all that's needed.

template
void mars_read_inputs<short int>(int first_input, int last_input, 
         PigFileModel *files[], SimpleImage<short int> *images[], int MAX_NL, 
         int MAX_NS, int band, RadiometryModel *radiometric[],
         PigBrtCorrModel *brt_corr[]);

template
void mars_read_inputs<float>(int first_input, int last_input, 
         PigFileModel *files[], SimpleImage<float> *images[], int MAX_NL, 
         int MAX_NS, int band, RadiometryModel *radiometric[],
         PigBrtCorrModel *brt_corr[]);

template
void mars_read_inputs_hsi<short int>(int first_input, int last_input, 
         PigFileModel *files[], SimpleImage<short int> *siImages[], int MAX_NL, 
         int MAX_NS, int band, RadiometryModel *radiometric[],
         PigBrtCorrModel *brt_corr[]);

template
void mars_read_inputs_hsi<float>(int first_input, int last_input, 
         PigFileModel *files[], SimpleImage<float> *siImages[], int MAX_NL, 
         int MAX_NS, int band, RadiometryModel *radiometric[],
         PigBrtCorrModel *brt_corr[]);

