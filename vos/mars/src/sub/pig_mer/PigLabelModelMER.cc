////////////////////////////////////////////////////////////////////////
// PigLabelModelMER
//
// MER-specific class for Label models.  Responsible for maintaining an output
// file's label information.
//
////////////////////////////////////////////////////////////////////////

#include "PigLabelModelMER.h"

#include "return_status.h"
#include "lbl_derived_image.h"

////////////////////////////////////////////////////////////////////////
// The constructor is protected because the create() function should be
// used instead.  The file passed in must be open.
////////////////////////////////////////////////////////////////////////

PigLabelModelMER::PigLabelModelMER(int unit, const char *mission)
              :PigLabelModel(unit, mission)
{
}

PigLabelModelMER::~PigLabelModelMER()
{
}

//////////////////////////////////////////////////////////////////////////
// Write marsreach label items.  Note that this is MER specific.
//////////////////////////////////////////////////////////////////////////

int PigLabelModelMER::setReach(PigFileModel *file_models[], int nids)
{    
    int result = PigLabelModel::setReach(file_models, nids);// basic items

    LblDerivedImage_typ  DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    char *inst_values[] = { "MI","MI","MI","MI",
			    "RAT","RAT","RAT","RAT",
			    "MB","MB","MB","MB",
			    "APXS","APXS","APXS","APXS" };
    
    int size = sizeof(inst_values) / sizeof(char*);
    
    for (int cnt = 0; cnt < size; cnt++) {
        strcpy(DerivedImage.InstrumentBandId[cnt].Value, inst_values[cnt]);
	DerivedImage.InstrumentBandId[cnt].Valid = 1;
    }

    
    char *config_values[] = { "ELBOW_UP_WRIST_UP", 
				      "ELBOW_UP_WRIST_DOWN",
				      "ELBOW_DOWN_WRIST_UP", 
				      "ELBOW_DOWN_WRIST_DOWN",
				      "ELBOW_UP_WRIST_UP", 
                                      "ELBOW_UP_WRIST_DOWN",
			              "ELBOW_DOWN_WRIST_UP", 
                                      "ELBOW_DOWN_WRIST_DOWN",
			              "ELBOW_UP_WRIST_UP", 
                                      "ELBOW_UP_WRIST_DOWN",
			              "ELBOW_DOWN_WRIST_UP", 
                                      "ELBOW_DOWN_WRIST_DOWN",
			              "ELBOW_UP_WRIST_UP", 
                                      "ELBOW_UP_WRIST_DOWN",
			              "ELBOW_DOWN_WRIST_UP", 
                                      "ELBOW_DOWN_WRIST_DOWN" };

    size = sizeof(config_values) / sizeof(char*);

    for (int cnt = 0; cnt < size; cnt++) {
      strcpy(DerivedImage.ConfigurationBandId[cnt].Value, config_values[cnt]);
      DerivedImage.ConfigurationBandId[cnt].Valid = 1;
    }

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
      printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
      result = 1;
    }

    return result;
}

//////////////////////////////////////////////////////////////////////////
// Write MER-specific marsrough label items.  Flag is ignored.
//////////////////////////////////////////////////////////////////////////

int PigLabelModelMER::setRough(PigFileModel *file_models[], int nids,
			PigCoordSystem *cs, float invalid_constant, int flag)
{ 
    int result = PigLabelModel::setRough(file_models, nids, 
		 	                 cs, invalid_constant, flag);

    LblDerivedImage_typ  DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    strcpy(DerivedImage.InstrumentBandId[0].Value, "RAT");
    DerivedImage.InstrumentBandId[0].Valid = 1;

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
      printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
      result = 1;
    }
    
    return result;
}

/////////////////////////////////////////////////////////////////////////////
// Returns Rover Motion Counter(RMC) index name.  For MER, we have 6 indexes,
// named (SITE, DRIVE, IDD, PMA, HGA, TWEAK).
/////////////////////////////////////////////////////////////////////////////
char* PigLabelModelMER::getCoordSysIndexName(int index)
{
    switch(index) {
        case 0: {
	  return "SITE";
	  break;
	}
        case 1: {
	  return "DRIVE";
	  break;
	}
        case 2: {
	  return "IDD";
	  break;
	}
        case 3: {
	  return "PMA";
	  break;
	}
        case 4: {
	  return "HGA";
	  break;
	}
        case 5: {
	  return "TWEAK";
	  break;
	}
	// Invalid index
	return NULL;
    }
    return NULL;
}
