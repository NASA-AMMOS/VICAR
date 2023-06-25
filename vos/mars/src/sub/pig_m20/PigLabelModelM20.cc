////////////////////////////////////////////////////////////////////////
// PigLabelModelM20
//
// M20-specific class for Label models.  Responsible for maintaining an output
// file's label information.
//
////////////////////////////////////////////////////////////////////////

#include "PigLabelModelM20.h"

#include "return_status.h"
#include "lbl_derived_image.h"


const char* PigLabelModelM20::inst_values[] = 
    { "DRILL","GDRT","SHERLOC_WATSON","SHERLOC","PIXL", "FCS" };

const char* PigLabelModelM20::config_values[] = 
    { "ARM_SO_EU_WU", "ARM_SO_EU_WD", "ARM_SO_ED_WU", "ARM_SO_ED_WD",
      "ARM_SI_EU_WU", "ARM_SI_EU_WD", "ARM_SI_ED_WU", "ARM_SI_ED_WD" };

////////////////////////////////////////////////////////////////////////
// The constructor is protected because the create() function should be
// used instead.  The file passed in must be open.
////////////////////////////////////////////////////////////////////////

PigLabelModelM20::PigLabelModelM20(int unit, const char *mission)
              :PigLabelModel(unit, mission)
{
}

PigLabelModelM20::~PigLabelModelM20()
{
}

//////////////////////////////////////////////////////////////////////////
// Write mslreach label items.  Note that this is M20 specific.
//////////////////////////////////////////////////////////////////////////

//!!!!!!!! NOT UPDATED FROM MSL !!!!!!!!

int PigLabelModelM20::setReach(PigFileModel *file_models[], int nids)
{    
    int result = PigLabelModel::setReach(file_models, nids); // basic items

    LblDerivedImage_typ  DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    int size = sizeof(inst_values) / sizeof(char*);
    
    for (int cnt = 0; cnt < size; cnt++) {
        strcpy(DerivedImage.InstrumentBandId[cnt].Value, inst_values[cnt]);
	DerivedImage.InstrumentBandId[cnt].Valid = 1;
    }

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

//////////////////////////////////////////////////////////////////////////
// Write goodness label items.  Note that this is M20 specific.
//////////////////////////////////////////////////////////////////////////

int PigLabelModelM20::setGReach(PigFileModel *file_models[], int nids,
                                int *bands, int nbands, bool include_configs)
{    
    int result = PigLabelModel::setGReach(file_models, nids, 
                              bands, nbands, include_configs); // basic items

    LblDerivedImage_typ  DerivedImage;
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    for (int cnt = 0; cnt < LBL_BAND_ID_ELEMENTS; cnt++)
	DerivedImage.InstrumentBandId[cnt].Valid = LBL_DELETE;

    if (!include_configs) {
        for (int cnt = 0; cnt < LBL_BIT_ID_ELEMENTS; cnt++)
	    DerivedImage.ConfigurationBitId[cnt].Valid = LBL_DELETE;
    } else {
        DerivedImage.ConfigurationBits.Value = 1;
        DerivedImage.ConfigurationBits.Valid = LBL_VALID;
        DerivedImage.ConfigurationBand.Value = 2;
        DerivedImage.ConfigurationBand.Valid = LBL_VALID;
    }

    int i_select=0;
    int size = sizeof(inst_values) / sizeof(char*);
    for (int cnt = 0; cnt < size; cnt++) {
        bool skip_this_band = true;
        for (int i = 0; (i < nbands) && skip_this_band; i++) {
            if (cnt+1 == bands[i])
                skip_this_band = false;
        }
        if (!skip_this_band && (i_select < LBL_BAND_ID_ELEMENTS)) {
            strncpy( DerivedImage.InstrumentListId[i_select].Value, 
                     inst_values[cnt],
                     sizeof(DerivedImage.InstrumentListId[i_select].Value) );
            DerivedImage.InstrumentListId[i_select].Valid = LBL_VALID;
            i_select++;
        }
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
// Write preload label items.  Note that this is M20 specific.  The third
// argument indicates the preload type: 2==2-band preload (min,max); 8==8-band
// preload (per config).
//////////////////////////////////////////////////////////////////////////

//!!!!!!!! NOT UPDATED FROM MSL !!!!!!!!

int PigLabelModelM20::setPreload(PigFileModel *file_models[], int nids,
				 int preload_type)
{    
    // basic items
    int result = PigLabelModel::setPreload(file_models, nids, preload_type);

    LblDerivedImage_typ  DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    char *inst_values[] = { "DRILL_MINIMUM", "DRILL_MAXIMUM" };
    
    if (preload_type == 2) {
        int size = sizeof(inst_values) / sizeof(char*);
	for (int cnt = 0; cnt < size; cnt++) {
	    strcpy(DerivedImage.InstrumentBandId[cnt].Value, inst_values[cnt]);
	    DerivedImage.InstrumentBandId[cnt].Valid = 1;
	}
    }
    else {			// default is 8-band now
        int size = sizeof(config_values) / sizeof(char*);
	for (int cnt = 0; cnt < size; cnt++) {
	    strcpy(DerivedImage.ConfigurationBandId[cnt].Value,
							config_values[cnt]);
	    DerivedImage.ConfigurationBandId[cnt].Valid = 1;
	}
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
// Write M20-specific marsrough label items.  Flag:
// 0: patch only (roughness) M20: unused
// 1: ring and patch (roughness) M20: FCS/ABRADE and CORING/STABILIZER
// 2: patch only (curvature) - M20 DRILL
// 3: ring and patch (curvature) - NOT SUPPORTED
// In addition to the normal instrument ID, we also override Missing and
// Invalid constants from the base class.
//////////////////////////////////////////////////////////////////////////

int PigLabelModelM20::setRough(PigFileModel *file_models[], int nids,
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

    if(flag >= 0 && flag < 2){

      //roughness product

      // flag: 0 = patch, 1 = patch and ring
      strcpy(DerivedImage.InstrumentBandId[0].Value, "ROUGH_FLAG");
      DerivedImage.InstrumentBandId[0].Valid = 1;
      strcpy(DerivedImage.InstrumentBandId[1].Value, "ROUGH_PATCH");
      DerivedImage.InstrumentBandId[1].Valid = 1;

      if (flag == 1) {	// 1: add ring
      
        strcpy(DerivedImage.InstrumentBandId[2].Value, "ROUGH_RING");
        DerivedImage.InstrumentBandId[2].Valid = 1;

        ImageData.MissingConstant[2].Value = invalid_constant;
        ImageData.MissingConstant[2].Valid = 1;
        ImageData.InvalidConstant[2].Value = invalid_constant;
        ImageData.InvalidConstant[2].Valid = 1;
        }
    }
    else if (flag == 2) {
       
        //curvature product, just patch
        strcpy(DerivedImage.DerivedImageType.Value, "CURVATURE_MAP"); 
        
        strcpy(DerivedImage.InstrumentBandId[0].Value, "CURVATURE_FLAG");
        DerivedImage.InstrumentBandId[0].Valid = 1;
        strcpy(DerivedImage.InstrumentBandId[1].Value, "CONCAVITY"); 
        DerivedImage.InstrumentBandId[1].Valid = 1;
        strcpy(DerivedImage.InstrumentBandId[2].Value, "CONVEXITY");
        DerivedImage.InstrumentBandId[2].Valid = 1;
    } 
    else
    {
      printMsg((char *)LblErrorMessage(), "BadFlags", PigMsgError);
      result = 1;
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

/////////////////////////////////////////////////////////////////////////////
// Returns Rover Motion Counter(RMC) index name.  For M20, we have 10 indices,
// named (SITE, DRIVE, POSE, ARM, SHA, DRILL, RSM, HGA, BITCAR, SEAL).  PIXL
// has 12, adding RTT and PMC.
/////////////////////////////////////////////////////////////////////////////
char* PigLabelModelM20::getCoordSysIndexName(int index)
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
	  return "POSE";
	  break;
	}
        case 3: {
	  return "ARM";
	  break;
	}
        case 4: {
	  return "SHA";
	  break;
	}
        case 5: {
	  return "DRILL";
	  break;
	}
        case 6: {
	  return "RSM";
	  break;
	}
        case 7: {
	  return "HGA";
	  break;
	}
        case 8: {
	  return "BITCAR";
	  break;
	}
        case 9: {
	  return "SEAL";
	  break;
	}
        case 10: {
	  return "RTT";
	  break;
	}
        case 11: {
	  return "PMC";
	  break;
	}
	// Invalid index
	return NULL;
    }
    return NULL;
}
