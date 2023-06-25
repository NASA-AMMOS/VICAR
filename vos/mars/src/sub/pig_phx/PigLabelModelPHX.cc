////////////////////////////////////////////////////////////////////////
// PigLabelModelPHX
//
// PHX-specific class for Label models.  Responsible for maintaining an output
// file's label information.
//
////////////////////////////////////////////////////////////////////////

#include "PigLabelModelPHX.h"

#include "return_status.h"
#include "lbl_derived_image.h"

////////////////////////////////////////////////////////////////////////
// The constructor is protected because the create() function should be
// used instead.  The file passed in must be open.
////////////////////////////////////////////////////////////////////////

PigLabelModelPHX::PigLabelModelPHX(int unit, const char *mission)
              :PigLabelModel(unit, mission)
{
}

PigLabelModelPHX::~PigLabelModelPHX()
{
}

//////////////////////////////////////////////////////////////////////////
// Write marsreach label items.  Note that this is PHX specific.
//////////////////////////////////////////////////////////////////////////

int PigLabelModelPHX::setReach(PigFileModel *file_models[], int nids)
{    
    int result = PigLabelModel::setReach(file_models, nids);// basic items

    LblDerivedImage_typ  DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    char *inst_values[] = { "SCOOP", "SCOOP",
			    "SCOOP_BTM", "SCOOP_BTM",
			    "BLADE", "BLADE",
			    "ISAD1", "ISAD1",
			    "ISAD2", "ISAD2",
			    "TECP", "TECP" };
    
    int size = sizeof(inst_values) / sizeof(char*);
    
    for (int cnt = 0; cnt < size; cnt++) {
        strcpy(DerivedImage.InstrumentBandId[cnt].Value, inst_values[cnt]);
	DerivedImage.InstrumentBandId[cnt].Valid = 1;
    }

    
    char *config_values[] = { "ELBOW_UP", "ELBOW_DOWN",
			      "ELBOW_UP", "ELBOW_DOWN",
			      "ELBOW_UP", "ELBOW_DOWN",
			      "ELBOW_UP", "ELBOW_DOWN",
			      "ELBOW_UP", "ELBOW_DOWN",
			      "ELBOW_UP", "ELBOW_DOWN" };

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
// There are no PHX-specific marsrough items at this time.
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Returns Rover Motion Counter(RMC) index name.  For PHX, we have 2 indexes,
// named (SITE, DRIVE).
/////////////////////////////////////////////////////////////////////////////
char* PigLabelModelPHX::getCoordSysIndexName(int index)
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
	// Invalid index
	return NULL;
    }
    return NULL;
}
