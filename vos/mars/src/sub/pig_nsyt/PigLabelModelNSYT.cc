////////////////////////////////////////////////////////////////////////
// PigLabelModelNSYT
//
// NSYT-specific class for Label models.  Responsible for maintaining an output
// file's label information.
//
////////////////////////////////////////////////////////////////////////

#include "PigLabelModelNSYT.h"

#include "return_status.h"
#include "lbl_derived_image.h"

////////////////////////////////////////////////////////////////////////
// The constructor is protected because the create() function should be
// used instead.  The file passed in must be open.
////////////////////////////////////////////////////////////////////////

PigLabelModelNSYT::PigLabelModelNSYT(int unit, const char *mission)
              :PigLabelModel(unit, mission)
{
}

PigLabelModelNSYT::~PigLabelModelNSYT()
{
}


#if 0
//!!!! NEEDS TO BE REPLACED FOR INSIGHT
//////////////////////////////////////////////////////////////////////////
// Write mslreach label items.  Note that this is MSL specific.
//////////////////////////////////////////////////////////////////////////

int PigLabelModelMSL::setReach(PigFileModel *file_models[], int nids)
{    
    int result = PigLabelModel::setReach(file_models, nids); // basic items

    LblDerivedImage_typ  DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    char *inst_values[] = { "DRILL","DRT","MAHLI","APXS","SCOOP_TIP" };
    
    int size = sizeof(inst_values) / sizeof(char*);
    
    for (int cnt = 0; cnt < size; cnt++) {
        strcpy(DerivedImage.InstrumentBandId[cnt].Value, inst_values[cnt]);
	DerivedImage.InstrumentBandId[cnt].Valid = 1;
    }

    char *config_values[] = {	"ARM_SO_EU_WU", "ARM_SO_EU_WD",
				"ARM_SO_ED_WU", "ARM_SO_ED_WD",
				"ARM_SI_EU_WU", "ARM_SI_EU_WD",
				"ARM_SI_ED_WU", "ARM_SI_ED_WD" };

    size = sizeof(config_values) / sizeof(char*);

    for (int cnt = 0; cnt < size; cnt++) {
      strcpy(DerivedImage.ConfigurationBitId[cnt].Value, config_values[cnt]);
      DerivedImage.ConfigurationBitId[cnt].Valid = 1;
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
//!!!! NEEDS TO BE REPLACED FOR INSIGHT
#endif

#if 0
//!!!! NEEDS TO BE REPLACED FOR INSIGHT
//////////////////////////////////////////////////////////////////////////
// Write MSL-specific marsrough label items.  Flag indicates Drill or DRT
// (0 = DRT, 1 = Drill)
// In addition to the normal instrument ID, we also override Missing and
// Invalid constants from the base class.
//////////////////////////////////////////////////////////////////////////

int PigLabelModelMSL::setRough(PigFileModel *file_models[], int nids,
			PigCoordSystem *cs, float invalid_constant, int flag)
{ 
    int result = PigLabelModel::setRough(file_models, nids,
					cs, invalid_constant, flag);

    LblDerivedImage_typ  DerivedImage;
    LblImageData_typ ImageData;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;
    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    ImageData.MissingConstant[1].Value = invalid_constant;
    ImageData.MissingConstant[1].Valid = 1;
    ImageData.InvalidConstant[1].Value = invalid_constant;
    ImageData.InvalidConstant[1].Valid = 1;

    if (flag) {		// non-0 = Drill
	strcpy(DerivedImage.InstrumentBandId[0].Value, "DRILL_FLAG");
	DerivedImage.InstrumentBandId[0].Valid = 1;
	strcpy(DerivedImage.InstrumentBandId[1].Value, "DRILL_OVERALL");
	DerivedImage.InstrumentBandId[1].Valid = 1;
	strcpy(DerivedImage.InstrumentBandId[2].Value, "DRILL_RING");
	DerivedImage.InstrumentBandId[2].Valid = 1;

	ImageData.MissingConstant[2].Value = invalid_constant;
	ImageData.MissingConstant[2].Valid = 1;
	ImageData.InvalidConstant[2].Value = invalid_constant;
	ImageData.InvalidConstant[2].Valid = 1;
    }
    else {		// 0 = DRT
	strcpy(DerivedImage.InstrumentBandId[0].Value, "DRT_FLAG");
	DerivedImage.InstrumentBandId[0].Valid = 1;
	strcpy(DerivedImage.InstrumentBandId[1].Value, "DRT_OVERALL");
	DerivedImage.InstrumentBandId[1].Valid = 1;
    }

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
	result = 1;
    }
    
    // write out ImageData Property Group to the labe
    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
	result = 1;
    }

    return result;
}
//!!!! NEEDS TO BE REPLACED FOR INSIGHT
#endif

/////////////////////////////////////////////////////////////////////////////
// Returns Rover Motion Counter(RMC) index name.  For NSYT, we have 2 indices,
// named (SITE, DRIVE).
/////////////////////////////////////////////////////////////////////////////
char* PigLabelModelNSYT::getCoordSysIndexName(int index)
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
