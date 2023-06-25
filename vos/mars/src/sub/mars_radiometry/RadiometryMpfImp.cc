////////////////////////////////////////////////////////////////////////
// RadiometryMpfImp
//
// Subclass for MPF IMP Radiometry Model.  Responsible for maintaining
// calibration information for a camera and applying radiometric correction
// when requested.
//
// J. Maki 2 February 1999	original version
// R. Deen August 1999		significant revisions
//
////////////////////////////////////////////////////////////////////////

#include "RadiometryMpfImp.h"
#include "PigFileModel.h"

#include <unistd.h>	// for the read function
#include <fcntl.h>	// for the open function
#include <stdlib.h>

float RadiometryMpfImp::_flat[FLAT_NL_MPFIMP][FLAT_NS_MPFIMP];
char RadiometryMpfImp::_flat_tag[256];
int RadiometryMpfImp::_flat_valid = FALSE;


////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryMpfImp::RadiometryMpfImp(const char *mission,const char *instrument,
				int filter, float exptime, float temperature,
				int sl, int ss, int el, int es)
		: RadiometryModel(mission, instrument, sl, ss, el, es)

{
    _filter = filter;
    _exptime = exptime;
    _temperature = temperature;
}

RadiometryMpfImp::RadiometryMpfImp()
		: RadiometryModel()
{
    _filter = 0;
    _exptime = 1.0;
    _temperature = 0.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryMpfImp::~RadiometryMpfImp()
{
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
// It could be a constructor instead, but this allows us to return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryMpfImp *RadiometryMpfImp::create(PigFileModel *file)
{
    int sl, ss, el, es;

    sl = file->getFirstLine(0) - 1;		// convert to 0-based
    ss = file->getFirstLineSample(0) - 1;

    if (sl < 0) sl = 0; // clip if negative
    if (ss < 0) ss = 0;

    el = sl + file->getNL() - 1; // compute ending line/sample
    es = ss + file->getNS() - 1;

    // grab the observation time
    float exptime = file->getExposureDuration(1000) / 1000.;   // cvt to seconds

    // grab the CCD temperature
    float temperature = file->getInstrumentTemperature(0);

    // grab the filter number
    int filter;
    const char *filter_string = file->getFilterNumber();
    if (filter_string == NULL)
	filter = 5;			// default
    else
	filter = atoi(filter_string);

    // now do the actual creation by calling the constructor
    const char *image_id = file->getImageId();
    // Even image id == left camera (!)
    if (image_id && (image_id[strlen(image_id)-1] % 2) == 0)
        return new RadiometryMpfImp(file->getMissionName(), "IMP Left", filter,
			exptime, temperature, sl, ss, el, es);
    else
        return new RadiometryMpfImp(file->getMissionName(), "IMP Right", filter,
			exptime, temperature, sl, ss, el, es);

    return NULL;		// never reached
}

////////////////////////////////////////////////////////////////////////

void RadiometryMpfImp::print()

{
    char msg[256];

    RadiometryModel::print();

    sprintf(msg, "Filter: %d", _filter);
    printInfo(msg);
    sprintf(msg, "Exposure time: %f", _exptime);
    printInfo(msg);
    sprintf(msg, "Temperature: %f", _temperature);
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// Load a flat-field image.  The image is cached in a static class variable
// so we don't re-load it on each image.
////////////////////////////////////////////////////////////////////////

int RadiometryMpfImp::loadFlatField(float *&flat, int &width, int &height)
{
    int fd;
    int i, j, k;
    char flat_field_name[256], flat_field_path[256];
    double doubleflat[FLAT_NS_MPFIMP];
    char msg[256];

    flat = NULL;
    width = FLAT_NS_MPFIMP;
    height = FLAT_NL_MPFIMP;

    // Construct the filename

    sprintf(flat_field_name, "flat_field_%c.%d",
		(strcmp(_instrument, "FRAME_LEFT") == 0) ? 'l' : 'r',
		_filter);

    // check and see if we need to load the flat field, which is
    // stored in the static variable _flat

    if (!_flat_valid || strcmp(_flat_tag, flat_field_name) != 0) {

	// We need a new flat field

	_flat_valid = FALSE;		// until it loads properly

	// change the tag name to reflect the active flat field
	strcpy(_flat_tag, flat_field_name);

	// Find the file
	fd = -1;
	FILE *f = PigModelBase::openConfigFile(flat_field_name,flat_field_path);
	if (f != NULL) {
	    fclose(f);
	    sprintf(msg, "loading flat field file %s", flat_field_path);
	    printInfo(msg);
	    // re-open the flat field image using open instead of fopen
      	    fd = open(flat_field_path, O_RDONLY, 0);
	}

      	if (fd == -1) {
            sprintf(msg, "Unable to open flat field file %s", flat_field_name);
	    printWarning(msg);
            printWarning("No flat field correction applied");
	    return FALSE;
        }

    	for (i = 0; i < FLAT_NL_MPFIMP; i++) {

	    // The file is stored as doubles, so convert to float

      	    k = read(fd, doubleflat, sizeof(doubleflat));
      	    if (k < sizeof(doubleflat)) {
            	sprintf(msg,
		    "Unexpected short record read from flat field file %s", 
		    flat_field_path);
		printError(msg);
                printWarning("No flat field correction applied");
		close(fd);
		return FALSE;
      	    }
      	    for (j=0; j<FLAT_NS_MPFIMP; j++)
		_flat[i][j] = (float) doubleflat[j];
	}
        close(fd);
	_flat_valid = TRUE;
    }

    flat = (float *)_flat;
    return TRUE;
}

////////////////////////////////////////////////////////////////////////
// Return the responsivity coefficient given the temperature and exposure time
// responsivity = R0+R1*T+R2*T*T.  This coefficient is divided into the input
// DN to give a number with the units watts/(meter**2,steradian,micron).
////////////////////////////////////////////////////////////////////////

double RadiometryMpfImp::getResponsivityFactor(int band)
{
    double responsivity;

    // Coefficients for computing responsivity versus temperature, Left camera

    static double r_left[12][3] = {
	{ 126.86, -.4214,  -0.0006 },
	{ 500.0,   0.0,     0.0    },
	{ 500.0,   0.0,     0.0    },
	{ 500.0,   0.0,     0.0    },
	{ 500.0,   0.0,     0.0    },
	{ 565.9,  -0.5611, -0.0013 },
	{ 875.74,  0.2383, -0.0029 },
	{ 1224.4,  2.125,   0.003  },
	{ 1094.1,  2.9378,  0.0058 },
	{ 485.65,  1.956,   0.0051 },
	{ 219.1,   1.648,   0.0053 },
	{ 401.3,   2.055,   0.0051 }
    };
    
    // Coefficients for computing responsivity versus temperature, Right camera

     static double r_right[12][3] = {
	{ 132.86, -0.399,  -0.0007 },
	{ 500.0,   0.0,     0.0    },
	{ 500.0,   0.0,     0.0    },
	{ 500.0,   0.0,     0.0    },
	{ 500.0,   0.0,     0.0    },
	{ 557.84, -0.5751, -0.0014 },
	{ 803.26, -0.2517, -0.0019 },
	{ 7596.9,  9.057,  -0.0235 },
	{ 594.0,  -0.5998, -0.0013 },
	{ 592.78, -0.9146, -0.0021 },
	{ 361.0,  -0.6556, -0.0019 },
	{ 402.8,   2.237,   0.0066 }
    };
    
    // compute the responsivity

    if (strcmp(_instrument,"FRAME_LEFT") == 0)
      
	responsivity =  r_left[_filter][0] +
			r_left[_filter][1] * _temperature +
			r_left[_filter][2] * _temperature * _temperature;	

    else
        responsivity =  r_right[_filter][0] +
                        r_right[_filter][1] * _temperature +
                        r_right[_filter][2] * _temperature * _temperature;    

    // precompute the combined responsivity exposure factor

    return responsivity;
}

double RadiometryMpfImp::getResponsivity(int band)
{   
    return _exptime * getResponsivityFactor(band);
}


