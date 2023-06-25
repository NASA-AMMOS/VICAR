////////////////////////////////////////////////////////////////////////
// PigLabelModel
//
// Base class for Label models.  Responsible for maintaining an output
// file's label information.
//
// Certain function calls return data in the multimission Label API (Lbl*)
// structures.  For missions which do not conform with this format (e.g.
// MPF), these function calls should read appropriate labels and fill in
// the important fields in the Label API structures.
//
// The base class is intended to be sufficient for most missions using the
// MM label structures.  Mission-specific subclasses can be created as
// needed.  The create() function should be modified to call a mission-
// specific create() function for this case.
//
////////////////////////////////////////////////////////////////////////

#include <time.h>

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"
#include "RadiometryModel.h"
#include "PigBrtCorrModel.h"
#include "PigColorModel.h"
#include "lbl_derived_image.h"
#include "lbl_surface_projection.h"
#include "lbl_image_map_projection.h"
#include "lbl_derived_geometry.h"
#include "return_status.h"
#include "zvproto.h"
#include "rts_time.h"
#include "del_prop_grp.h"

////////////////////////////////////////////////////////////////////////
// The constructor is protected because the create() function should be
// used instead.  The file passed in must be open.
////////////////////////////////////////////////////////////////////////

PigLabelModel::PigLabelModel(int unit, const char *mission)
              :PigModelBase()
{
    _mission = PigMission::getMissionObject(mission);
    _unit = unit;
    // populate label coord systems array
    getCoordSystems();
}

PigLabelModel::PigLabelModel(int unit, PigMission *mission)
              :PigModelBase()
{
    _mission = mission;
    _unit = unit;
    // populate label coord systems array
    getCoordSystems();
}

PigLabelModel::~PigLabelModel()
{
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of the
// proper mission-specific subclass for the given file.  Missions
// using the multimission label API may not need a mission-specific
// subclass.
////////////////////////////////////////////////////////////////////////

PigLabelModel *PigLabelModel::create(int unit, const char* mission)
{
    int status;

    PigMission *m = PigMission::getMissionObject(mission);
    return m->createLabelModel(unit);
}

PigLabelModel *PigLabelModel::create(int unit, PigMission *mission)
{
    int status;

    return mission->createLabelModel(unit);
}


/////////////////////////////////////////////////////////////////////////
//  Goes to the file label and reads all the coordinate systems.
//  The file should be open.  Coordinate systems stored in an array of
//  PigCSDefinition objects.
////////////////////////////////////////////////////////////////////////

void PigLabelModel::getCoordSystems()
{
    _num_all_coord_sys = MAX_CS_OBJ;
    PigFileModel::readAllCoordSystems(_mission, _unit,
				_num_all_coord_sys, _all_coord_sys);
}

/////////////////////////////////////////////////////////////////////////
// Check if given Coordinate System is already in the label, i.e. found
// in _all_coord_sys array.  Returns 1 if CS Found, 0 if not.
/////////////////////////////////////////////////////////////////////////
int PigLabelModel::findCS(PigCSReference *cs)
{
    for (int cnt = 0; cnt < _num_all_coord_sys; cnt++) {
	    PigCSReference *csRef = _all_coord_sys[cnt]->getIdentity();
		if (cs->isEqual(csRef))
		    return 1;
	}	 
	// didn't find the match
	return 0;
}

//////////////////////////////////////////////////////////////////////////
int PigLabelModel::findValue(char* key[], int& count, char* new_str)
{
    for (int i = 0; i < count; i++)
        if (!strcasecmp(key[i], new_str)) {
            // release the memory and return
            free(new_str);
            new_str = NULL;
            return 1;
        }

    // didn't find the match
    key[count++] = new_str;
    return 0;
}
//////////////////////////////////////////////////////////////////////////
//
// Writes the standard identification properties to the label.
//
// The output unit number must be open for write.
//
// This routine requires the following parameters be in the PDF:
//
// PARM DATA_SET_NAME TYPE=STRING DEFAULT="NULL"
// PARM DATA_SET_ID TYPE=STRING DEFAULT="NULL"
// PARM RELEASE_ID TYPE=STRING DEFAULT="NULL"
// PARM PRODUCER_ID TYPE=STRING DEFAULT="NULL"
// PARM PRODUCER_INST TYPE=STRING DEFAULT="NULL"
// PARM PRODUCT_ID TYPE=STRING DEFAULT="NULL"
// PARM TARGET_NAME TYPE=STRING COUNT=(0:1) DEFAULT=--
// PARM TARGET_TYPE TYPE=STRING COUNT=(0:1) DEFAULT=--
//
// The passed-in ident parameter can contain items which are written to
// the label along with these.  It can be NULL if there are no additional
// items to write.
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setIdentification(LblIdentification_typ *ident)
{
    LblIdentification_typ Ident;
    char filename[PIG_MAX_FILENAME_SIZE+1];

    if (ident)
	memcpy(&Ident, ident, sizeof(LblIdentification_typ));
    else
        memset(&Ident, 0, sizeof(LblIdentification_typ));
    
    // given the unit number, get the File's name
    int status = zvget(_unit, "name", filename, NULL);

    // Product Id is based on file's name basically we remove file's extension
    // and directory path and the remaining string is our product id.
    // Don't set Product ID if it's already been set in the structure.

    if (!Ident.ProductId.Valid) {
        // get rid of absolute directory path
        char *fn = strrchr(filename, '/');
 
	if (fn)
	    // remove '/'
	    fn++;
	else 
	    // filename doesn't contain absolute directory path
	    fn = filename;
	  
	// returns the string size before the dot
	int size = strcspn(fn, ".");
	// copies product id which is filename minus extension
	strncpy(Ident.ProductId.Value, fn, size);
	// NULL terminate the string
	Ident.ProductId.Value[size] = '\0';
	Ident.ProductId.Valid = 1;
    }
    
    // Check if keywords have been explicitly specified on
    // command line.  If they were, that value will override
    // whatever was specified in the input label.
    int count;

    zvp("DATA_SET_NAME", Ident.DataSetName.Value, &count);
    if (count == 1)
        Ident.DataSetName.Valid = 1;

    zvp("DATA_SET_ID", Ident.DataSetId.Value, &count);
    if (count == 1)
        Ident.DataSetId.Valid = 1;

    zvp("RELEASE_ID", Ident.ReleaseId.Value, &count);
    if (count == 1)
        Ident.ReleaseId.Valid = 1;

    // Don't clobber the ProductID created above unless it's specified.
    zvpcnt("PRODUCT_ID", &count);
    if (count != 0) {
        zvp("PRODUCT_ID", Ident.ProductId.Value, &count);
        Ident.ProductId.Valid = 1;
    }

    zvp("PRODUCER_ID", Ident.ProducerId.Value, &count);
    if (count == 1)
        Ident.ProducerId.Valid = 1;

    zvp("PRODUCER_INST", Ident.ProducerInstitutionName.Value, &count);
    if (count == 1)
        Ident.ProducerInstitutionName.Valid = 1;

    zvp("TARGET_NAME", Ident.TargetName.Value, &count);
    if (count == 1)
	Ident.TargetName.Valid = 1;

    zvp("TARGET_TYPE", Ident.TargetType.Value, &count);
    if (count == 1)
	Ident.TargetType.Valid = 1;

    // Finally, write the labels.  Note that we are using
    // LBL_AUGMENT mode which starts with copying an input label
    // structure then changes values or adds keyword-value
    // pairs for items in LblIdentification_typ Ident that
    // are valid(Ident.XXX.Valid=1).

    status = LblIdentification(_unit, LBL_AUGMENT, &Ident, 1);
    if (RTN_FAILURE(status))
        printMsg((char *)LblErrorMessage(), "Identification", PigMsgError);
    return;
}

//////////////////////////////////////////////////////////////////////
// Writes the standard multi-input properties to the label.  Currently
// these properties include IDENTIFICATION and radiometry labels in 
// DERIVED_IMAGE_PARMS.  Certain items (see the code) are added to these 
// structures.  If you want other items in there, you may pass
// in partially-filled structures with additional items.  Fields with
// the Valid flag set will *not* be overridden.  If these parameters 
// are NULL, empty structures will be created.
//
// The rad_models parameter may be NULL if no rad models are present.
// If the rad models do not all agree, the radiometric scaling factors
// will not be written (i.e. it is an uncalibrated image).
//////////////////////////////////////////////////////////////////////

void PigLabelModel::setRemosaic(PigFileModel *file_models[],
                                PigFileModel *idx_fm,
                                PigFileModel *icm_fm,
			        RadiometryModel *rad_models[],
			        PigBrtCorrModel *brt_models[],
 			        int nids,
			        PigCoordSystem *cs)
{
    PigFileModel **fm_all = NULL;
    fm_all = new PigFileModel *[nids+2];
    if (fm_all == NULL) {
       printFatal("Cannot allocate dynamic memory to get setRemosaic job done!");
       return;
    }

    setMosaic(file_models, rad_models, brt_models, NULL, NULL, nids, NULL,
								cs, NULL);

    for (int i=0; i<nids; i++)
        fm_all[i] = file_models[i];
    fm_all[nids] = idx_fm;
    fm_all[nids+1] = icm_fm;
    writeProductIds(fm_all, nids+2);

    delete fm_all;

}

//////////////////////////////////////////////////////////////////////////
// Write marsrad label items
// Note: this no longer sets the bit mask directly.  The app needs to
// call setBitMask() itself if it wants the bit mask to be set.
// If given a single rad model, it writes it.  If given an array of
// rad models, it writes the values only if they are consistent with each
// other (isEquivalent is true).  Flat and dark filenames are written as
// unique arrays.
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setRadiometric(PigFileModel *file_models[], int nids,
				   RadiometryModel *rad_models[],
				   int num_rads)
{    
    int status;
    char msg[150];
    LblDerivedImage_typ  DerivedImage;
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    if (file_models != NULL)
        writeProductIds(file_models, nids);

    if (rad_models == NULL || num_rads <= 0)
	return;

    if (rad_models[0] == NULL)
	return;

    // Check for consistent rad models

    int use_rad = TRUE;

    if (num_rads > 1) {
	for (int i=1; i < num_rads; i++) {
	    if (rad_models[i] == NULL ||
	        	!rad_models[0]->isEquivalent(rad_models[i]))
		use_rad = FALSE;
	}
    }

    if (use_rad) {

        // We read the property first so we can check for double updates.
        status = LblDerivedImage(_unit, LBL_READ, &DerivedImage, 1);
        if (RTN_FAILURE(status))
            printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);

        if (DerivedImage.RadianceScaleFactor.Valid ||
	    DerivedImage.RadianceOffset.Valid) {
	    sprintf(msg, "WARNING: file %d appears to have been radiometrically corrected previously.", _unit);
            printMsg(msg, "", PigMsgWarning);
	    printMsg("Correction may be applied twice.", "", PigMsgWarning);
        }

        DerivedImage.RadianceScaleFactor.Value =
					rad_models[0]->getDnScalingFactor();
        DerivedImage.RadianceScaleFactor.Valid = 1;
        strcpy(DerivedImage.RadianceScaleFactorUnit.Value,
					rad_models[0]->getUnitLabelName());
        DerivedImage.RadianceScaleFactorUnit.Valid = 1;
        DerivedImage.RadianceOffset.Value = rad_models[0]->getDnScalingOffset();
        DerivedImage.RadianceOffset.Valid = 1;
        strcpy(DerivedImage.RadianceOffsetUnit.Value,
					rad_models[0]->getUnitLabelName());
        DerivedImage.RadianceOffsetUnit.Valid = 1;
        strcpy(DerivedImage.RadiometricCorrectionType.Value,
					rad_models[0]->getModelLabelName());
        DerivedImage.RadiometricCorrectionType.Valid = 1;

        if (rad_models[0]->doScaledRad()) {
            DerivedImage.RadiometricZenithScalingFactor.Value =
                                        rad_models[0]->getZenithFactor();
            DerivedImage.RadiometricZenithScalingFactor.Valid = 1;
            DerivedImage.AtmosphericOpacity.Value = rad_models[0]->getTau();
            DerivedImage.AtmosphericOpacity.Valid = 1;
            DerivedImage.AtmosphericOpacityReference.Value =
                                        rad_models[0]->getTauReference();
            DerivedImage.AtmosphericOpacityReference.Valid = 1;
        }
        strcpy(DerivedImage.RadiometricType.Value,
                                        rad_models[0]->getRadiometricType());
        DerivedImage.RadiometricType.Valid = 1;

	// Write out the responsivity

	if (rad_models[0]->isColor()) {
	    DerivedImage.ResponsivityR.Value =
					rad_models[0]->getResponsivityFactor(0);
	    DerivedImage.ResponsivityR.Valid = 1;
	    DerivedImage.ResponsivityG.Value =
					rad_models[0]->getResponsivityFactor(1);
	    DerivedImage.ResponsivityG.Valid = 1;
	    DerivedImage.ResponsivityB.Value =
					rad_models[0]->getResponsivityFactor(2);
	    DerivedImage.ResponsivityB.Valid = 1;
	} else {
	    DerivedImage.ResponsivityPan.Value = rad_models[0]->
							getResponsivityFactor(0);
	    DerivedImage.ResponsivityPan.Valid = 1;
	}

        // write out Derived Image Property Group into the label

        LblSetDerivedImage("DERIVED_IMAGE_PARMS");
        status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
        if (RTN_FAILURE(status)) {
          printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
        }

    }

    // Do the rest regardless of whether the models are consistent

    // Set Identification fields
    setIdentification(NULL);

    // Write flat and dark model filenames.

    writeRadFlats(rad_models, num_rads);
    writeRadDarks(rad_models, num_rads);

    return;

}

//////////////////////////////////////////////////////////////////////////
// Write flat field filenames.  Only write unique values, however,
// These use the zl routines directly for convenience.
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::writeRadFlats(RadiometryModel *rad_models[], int num_rads)
{
    char *flats[PIG_MAX_INPUTS];
    int n_flats = 0;
    int i;
    int status;

    // Remove the keyword if it's been copied from the input label
    zldel(_unit, "PROPERTY", "FLAT_FIELD_FILE_NAME",
		"PROPERTY", "DERIVED_IMAGE_PARMS", "ERR_ACT", "", NULL);
    zldel(_unit, "PROPERTY", "FLAT_FIELD_FILE_DESC",
		"PROPERTY", "DERIVED_IMAGE_PARMS", "ERR_ACT", "", NULL);

    for (int i = 0; i < PIG_MAX_INPUTS; i++)
	flats[i] = NULL;

    n_flats = 0;
    for (i=0; i < num_rads; i++) {
      int nf = rad_models[i]->getNumFlatFieldFiles();
      for (int j=0; j < nf; j++) {
	char *ff = rad_models[i]->getFlatFieldFile(j);
	if (ff == NULL)
	    continue;
	char *fff = (char *)malloc(strlen(ff)+1);
	strcpy(fff, ff);

	// Add to list or deallocate memory if duplicate
	if (!findValue(flats, n_flats, fff)) {
	    status = zladd(_unit, "PROPERTY", "FLAT_FIELD_FILE_NAME",
			flats[n_flats-1],
			"NELEMENT", 1,
			"FORMAT", "STRING", "MODE", "INSERT",
			"PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
	    // Add corresponding description
	    if (rad_models[i]->getFlatFieldDesc(j) != NULL) {
	        status = zladd(_unit, "PROPERTY", "FLAT_FIELD_FILE_DESC",
			rad_models[i]->getFlatFieldDesc(j),
			"NELEMENT", 1,
			"FORMAT", "STRING", "MODE", "INSERT",
			"PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
	    }
	}
      }
    }

    // deallocate the memory
    for (i=0; i < n_flats; i++)
	free(flats[i]);
}

//////////////////////////////////////////////////////////////////////////
// Write dark current filenames.  Only write unique values, however,
// These use the zl routines directly for convenience.
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::writeRadDarks(RadiometryModel *rad_models[], int num_rads)
{
    char *darks[PIG_MAX_INPUTS];
    int n_darks = 0;
    int i;
    int status;

    // Remove the keyword if it's been copied from the input label
    zldel(_unit, "PROPERTY", "DARK_CURRENT_FILE_NAME",
		"PROPERTY", "DERIVED_IMAGE_PARMS", "ERR_ACT", "", NULL);
    zldel(_unit, "PROPERTY", "DARK_CURRENT_FILE_DESC",
		"PROPERTY", "DERIVED_IMAGE_PARMS", "ERR_ACT", "", NULL);

    for (int i = 0; i < PIG_MAX_INPUTS; i++)
	darks[i] = NULL;

    n_darks = 0;
    for (i=0; i < num_rads; i++) {
	char *dc = rad_models[i]->getDarkCurrentFile();
	if (dc == NULL)
	    continue;
	char *dcf = (char *)malloc(strlen(dc)+1);
	strcpy(dcf, dc);

	// Add to list or deallocate memory if duplicate
	if (!findValue(darks, n_darks, dcf)) {

	    status = zladd(_unit, "PROPERTY", "DARK_CURRENT_FILE_NAME",
			darks[n_darks-1],
			"NELEMENT", 1,
			"FORMAT", "STRING", "MODE", "INSERT",
			"PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
	    // Add corresponding description
	    if (rad_models[i]->getDarkCurrentDesc() != NULL) {
	        status = zladd(_unit, "PROPERTY", "DARK_CURRENT_FILE_DESC",
			rad_models[i]->getDarkCurrentDesc(),
			"NELEMENT", 1,
			"FORMAT", "STRING", "MODE", "INSERT",
			"PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
	    }
	}
    }

    // deallocate the memory
    for (i=0; i < n_darks; i++)
	free(darks[i]);
}

///////////////////////////////////////////////////////////////////////////
// Write instrument placement labels.  
///////////////////////////////////////////////////////////////////////////
void PigLabelModel::setInstPlacement(PigFileModel *file_models[], int nids, 
                                     const char *imageType, const char *instType,
                                     PigCoordSystem *cs)
{
    // First check if there is any good (successfully opened) file.

    PigFileModel *the_first_good_file = NULL;
    for (int i = 0; (i < nids) && !the_first_good_file; i++)
	the_first_good_file = file_models[i];
    if (the_first_good_file == NULL) {
        printError("Could not find a good file to read!");
        return;
    }

    LblImageData_typ ImageData;
    LblDerivedImage_typ  DerivedImage;
    int status;

    memset(&ImageData, 0, sizeof(LblImageData_typ));
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    writeProductIds(file_models, nids);
    setIdentification(NULL);

    // update missing and invalid constants
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    // update derived image type
    if (imageType)
        PigLabelModel::setDerivedImageType(imageType);

    // update target instrument
    if (instType) {
        PigLabelModel::setTargetInstrument(instType);
    } else {
        // set target instrument based on primary file model
        const LblDerivedImage_typ *primary_derived_image = 
                                             the_first_good_file->getLblDerivedImage();
        if (primary_derived_image->TargetInstrument.Valid) 
            setTargetInstrument(primary_derived_image->TargetInstrument.Value);
    }

    // update reference coord system name and reference coord system index 
    // in derived image parms group
    if (cs != NULL) {
        strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());
        DerivedImage.ReferenceCoordSystemName.Valid = 1;

        if (cs != NULL) {
            for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
                DerivedImage.ReferenceCoordSystemIndex[cnt].Value =
                    cs->getIndex(cnt);
                DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
            }

            // Write solution_id string only if it's not NULL or "telemetry"
            if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0
                	&& strcasecmp(cs->getSolutionId(), "telemetry")) {
                strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value,
                     cs->getSolutionId());
                DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
            }

            // check if this coordinate system already in the label, if not
            // write it in the label.  Then check if coordinate system's
            // reference coordinate system is in the label, if not write it
            // in the label.

            writeCSandItsReferences(cs);
        }
    }

    // write out Derived Image Property Group into the label
    status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    // write out Image Data Property Group into the label 
    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    return;
}

///////////////////////////////////////////////////////////////////////////
// Set the target instrument. Skip it if NULL.
///////////////////////////////////////////////////////////////////////////
void PigLabelModel::setTargetInstrument(const char *type)
{
    if (type == NULL)
        return; 

    LblDerivedImage_typ  DerivedImage;
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    strcpy(DerivedImage.TargetInstrument.Value, type);
    DerivedImage.TargetInstrument.Valid = 1;

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }   
}

//////////////////////////////////////////////////////////////////////////
// Write brightness correction and bias labels to the file.
//
// Generally this should come from the brt_models.  Direct use of
// radiance_factor and radiance_offset is more or less deprecated...
// they are used for BIAS which is the old method.  However, the
// radiance_factor and radiance_offset keywords, if specified, override
// what's in brt_models.
//
// Practically speaking, that means if you supply both BRTCORR and BIAS
// to a mosaic, the BIAS labels will win.  This is not recommended.
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setBrtCorr(PigBrtCorrModel *brt_models[],
		double radiance_factor[], double radiance_offset[],
		int nids)
{    
    int status = 0;
    int i;
    char msg[150];

    LblDerivedImage_typ  DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    // Unfortunately we have to call zladd directly, instead of going through
    // label routines like everything else.  This is due to the fact that
    // the label routines presently can't handle variable length arrays.

    if (radiance_factor != NULL) {   
        for (i= 0; i < nids; i++) {
            status = zladd(_unit, "PROPERTY", "IMAGE_RADIANCE_FACTOR",
			&radiance_factor[i], 
                        "FORMAT", "DOUB", "MODE", "INSERT",
			"PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
	}
    }
    if (radiance_offset != NULL) {
        for (i= 0; i < nids; i++) {
            status = zladd(_unit, "PROPERTY", "IMAGE_RADIANCE_OFFSET",
			&radiance_offset[i], 
                        "FORMAT", "DOUB", "MODE", "INSERT",
			"PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
        }
    }

    if (brt_models == NULL)
	return;					// nothing to write

    // Write brt corr models.  Each is responsible for writing its own slot.

    const char *type = NULL;
    for (i=0; i < nids; i++) {
	if (brt_models[i] != NULL) {
	    if (type == NULL)
		type = brt_models[i]->getModelLabelName();
	    else {
		if (strcmp(type, brt_models[i]->getModelLabelName()) != 0) {
		    type = "MIXED";
		}
	    }
	    status = brt_models[i]->writeToLabel(this, i);
	}
    }

    if (type != NULL) {
        strcpy(DerivedImage.BrightnessCorrectionType.Value, type);
	DerivedImage.BrightnessCorrectionType.Valid = 1;

	// write out Derived Image Property Group into the label
	LblSetDerivedImage("DERIVED_IMAGE_PARMS");
	status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
	if (RTN_FAILURE(status)) {
	    printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
	}
    }

    return;
}

//////////////////////////////////////////////////////////////////////////

// These should be called only by the specific brt corr models...

int PigLabelModel::writeBrtCorrLinearLabel(int index,
						double mult, double add)
{
    int status;

    status = zladd(_unit, "PROPERTY", "IMAGE_RADIANCE_FACTOR",
		&mult, "FORMAT", "DOUB", "MODE", "REPLACE",
		"ELEMENT", index+1, "NELEMENT", 1,
		"PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
    status = zladd(_unit, "PROPERTY", "IMAGE_RADIANCE_OFFSET",
		&add, "FORMAT", "DOUB", "MODE", "REPLACE",
		"ELEMENT", index+1, "NELEMENT", 1,
		"PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
    return status;
}

//////////////////////////////////////////////////////////////////////////
// Write marscahv label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setLinearized(PigFileModel *file_models[], int nids, 
                                  char *stereo_partner_type,
                                  char *cahv_fov_type,
                                  PigFileModel *partner_fm)
{    
    writeProductIds(file_models, nids);

    LblIdentification_typ Ident;
    LblDerivedImage_typ DerivedImage;

    memset(&Ident, 0, sizeof(LblIdentification_typ));
    strcpy(Ident.GeometryProjectionType.Value, "LINEARIZED");
    Ident.GeometryProjectionType.Valid = 1;
    
    setIdentification(&Ident);

    //add linearization_mode and linearization_product_id
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    strcpy(DerivedImage.LinearizationMode[0].Value, stereo_partner_type);
    strcpy(DerivedImage.LinearizationMode[1].Value, cahv_fov_type);
    DerivedImage.LinearizationMode[0].Valid = 1;
    DerivedImage.LinearizationMode[1].Valid = 1;

    if (partner_fm != NULL) {
        const char *product_id = partner_fm->getProductId();
        strcpy(DerivedImage.LinearizationProductId.Value, product_id);
        DerivedImage.LinearizationProductId.Valid = 1;
    }

    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) 
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);

    return;
}

//////////////////////////////////////////////////////////////////////////
// Write marsunlinearize label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setUnlinearized(PigFileModel *file_models[], int nids)
{    
    writeProductIds(file_models, nids);

    LblIdentification_typ Ident;
    LblDerivedImage_typ DerivedImage;

    memset(&Ident, 0, sizeof(LblIdentification_typ));
    strcpy(Ident.GeometryProjectionType.Value, "RAW");
    Ident.GeometryProjectionType.Valid = 1;

    setIdentification(&Ident);

    //remove LinearizationMode and LinearizationProductId
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    DerivedImage.LinearizationMode[0].Valid = LBL_DELETE;
    DerivedImage.LinearizationMode[1].Valid = LBL_DELETE;
    DerivedImage.LinearizationProductId.Valid = LBL_DELETE;

    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status))
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);

    return;
}
////////////////////////////////////////////////////////////////////////
// Write disparity(marsjplstereo, marscorr, etc.) output labels.
////////////////////////////////////////////////////////////////////////
void PigLabelModel::setDisparity(PigFileModel *file_models[], 
                                 PigFileModel *corr_parnter_fm,
                                 int nids, const char *imageType)
{    
    writeProductIds(file_models, nids);

    LblImageData_typ     ImageData;
    int status;

    memset(&ImageData, 0, sizeof(LblImageData_typ));

    if (imageType)
	setDerivedImageType(imageType);
    else
	setDerivedImageType("DISPARITY_MAP");

    // write out the missing and invalid constants
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    // if we have two-banded image (i.e. both line and samle disparity)
    if (imageType == NULL || (!strcmp(imageType, "DISPARITY_MAP"))) {
	ImageData.MissingConstant[1].Value = 0.0;
	ImageData.MissingConstant[1].Valid = 1;

	ImageData.InvalidConstant[1].Value = 0.0;
	ImageData.InvalidConstant[1].Valid = 1;
    }

    // Set Identification fields
    setIdentification(NULL);

    // write out Image Data Property Group into the label
    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }

    //add stereo_product_id
    if (corr_parnter_fm != NULL) {
        LblDerivedImage_typ  DerivedImage;
        memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
        const char *product_id = corr_parnter_fm->getProductId();
        strcpy(DerivedImage.StereoProductId.Value, product_id);
        DerivedImage.StereoProductId.Valid = 1;

        status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
        if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
        }
    }

    return;
}

////////////////////////////////////////////////////////////////////////
// Write extra (optional) info for disparity.  Count is always
// written; scale is written only if it is non-0; overlap is written only
// if it is non negative
////////////////////////////////////////////////////////////////////////
void PigLabelModel::setDisparityExtra(int count, double scale, double overlap,
                                      int pyramid_level)
{    
    int status;

    LblDerivedImage_typ DerivedImage;
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    DerivedImage.CorrelationPixelCount.Value = count;
    DerivedImage.CorrelationPixelCount.Valid = 1;

    if (scale != 0.0) {
        DerivedImage.CorrelationAverageScale.Value = scale;
        DerivedImage.CorrelationAverageScale.Valid = 1;
    }

    if (overlap >= 0.0) {
        DerivedImage.CorrelationOverlapPercentage.Value = overlap;
        DerivedImage.CorrelationOverlapPercentage.Valid = 1;
    }

    if (pyramid_level >= 0) {
        DerivedImage.CorrelationPyramidLevel.Value = pyramid_level;
        DerivedImage.CorrelationPyramidLevel.Valid = 1;
    }

    status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    return;
}

////////////////////////////////////////////////////////////////////////
// Write mask(marsdispcmp, marsfilter, etc.) output labels.
// Note that filename2 is special, if set it also goes to
// SUPPLEMENTAL_MASK_FILE so it can be included in the PDS4
// supplemental area.
////////////////////////////////////////////////////////////////////////
void PigLabelModel::setMask(PigFileModel *file_models[], int nids,
				const char *imageType,
				const char *filename1, const char *filename2,
				int use_horizon, double horizon)
{    
    writeProductIds(file_models, nids);

    LblImageData_typ     ImageData;
    LblDerivedImage_typ  DerivedImage;
    int status;

    memset(&ImageData, 0, sizeof(LblImageData_typ));
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    if (imageType)
	setDerivedImageType(imageType);
    else
	setDerivedImageType("MASK");

    int second_index = 0;
    if (filename1 != NULL && strlen(filename1) > 0) {
	second_index = 1;
        // get rid of absolute directory path
        const char *fn = strrchr(filename1, '/');
	if (fn)
	    fn++;			// remove '/'
	else 
	    fn = filename1;  // filename doesn't contain abs dir path

	int maxlen = sizeof(DerivedImage.MaskDescFileName[0].Value);
	strncpy(DerivedImage.MaskDescFileName[0].Value, fn, maxlen);
	DerivedImage.MaskDescFileName[0].Value[maxlen] = '\0';	// just in case
	DerivedImage.MaskDescFileName[0].Valid = 1;
    }
    if (filename2 != NULL && strlen(filename2) > 0) {
        // get rid of absolute directory path
        const char *fn = strrchr(filename2, '/');
	if (fn)
	    fn++;			// remove '/'
	else 
	    fn = filename2;  // filename doesn't contain abs dir path

	int maxlen = sizeof(DerivedImage.MaskDescFileName[second_index].Value);
	strncpy(DerivedImage.MaskDescFileName[second_index].Value, fn, maxlen);
	DerivedImage.MaskDescFileName[second_index].Value[maxlen] = '\0';
	DerivedImage.MaskDescFileName[second_index].Valid = 1; // just in case

	second_index++;		// flag that we added something for later test

        // Write it also to the supplemental label

	maxlen = sizeof(DerivedImage.SupplementalMaskFile.Value);
	strncpy(DerivedImage.SupplementalMaskFile.Value, fn, maxlen);
	DerivedImage.SupplementalMaskFile.Value[maxlen] = '\0';
	DerivedImage.SupplementalMaskFile.Valid = 1; // just in case

    }

    if (use_horizon) {
	DerivedImage.HorizonMaskElevation.Value = horizon;
	DerivedImage.HorizonMaskElevation.Valid = 1;
    }

  
    // Set SampleBitMask to 8-bits
    setBitMask(8);

    // write out the missing and invalid constants
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    // Set Identification fields
    setIdentification(NULL);

    // write out Image Data Property Group into the label
    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    // write out Derived Image Property Group into the label
    if (second_index != 0 || use_horizon) {
        LblSetDerivedImage("DERIVED_IMAGE_PARMS");
        int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
        if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
        }
    }
    return;
}

////////////////////////////////////////////////////////////////////////
// Write masked product labels.  Since marsmask typically uses the label
// from the mask image, this transfers the DERIVED_IMAGE_TYPE and
// MISSING/INVALID_CONSTANTs from the first input (which should be the
// actual image) to the output.
////////////////////////////////////////////////////////////////////////
void PigLabelModel::setMasked(PigFileModel *file_models[], int nids)
{    
    // First check if there is any good (successfully opened) file.

    PigFileModel *a_good_file = NULL;
    for (int i = 0; (i < nids) && !a_good_file; i++)
	a_good_file = file_models[i];
    if (a_good_file == NULL) {
        printError("Could not find a good file to read!");
        return;
    }

    LblImageData_typ     ImageData;
    int status;

    memset(&ImageData, 0, sizeof(LblImageData_typ));

    writeProductIds(file_models, nids);
    // Set Identification fields
    setIdentification(NULL);

    // Oddly, neither DerivedImageType nor InvalidConstant/MissingConstant
    // are available via the FileModel API.  Because it's unlikely we will
    // need them anywhere else, we get them the hard way here.

    const LblImageData_typ *input_image_data = a_good_file->getLblImageData();
    const LblDerivedImage_typ *input_derived_image =
					       a_good_file->getLblDerivedImage();

    // Reset DerivedImageType, if it's there

    if (input_derived_image->DerivedImageType.Valid)
	setDerivedImageType(input_derived_image->DerivedImageType.Value);

    // Reset the constants.  We only have to worry about [0] because
    // that's all that setMask() clobbers.

    int write_image_data = FALSE;
    if (input_image_data->MissingConstant[0].Valid) {
	write_image_data = TRUE;
	ImageData.MissingConstant[0].Value =
				input_image_data->MissingConstant[0].Value;
	ImageData.MissingConstant[0].Valid = 1;
    }
    if (input_image_data->InvalidConstant[0].Valid) {
	write_image_data = TRUE;
	ImageData.InvalidConstant[0].Value =
				input_image_data->InvalidConstant[0].Value;
	ImageData.InvalidConstant[0].Valid = 1;
    }

    if (write_image_data) {
        // write out Image Data Property Group into the label
        status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
        if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
        }
    }
    return;
}

//////////////////////////////////////////////////////////////////////////
// Write marsxyz label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setXYZ(PigFileModel *file_models[], int nids,
                           PigCoordSystem *cs, char *imageType, double baseline,
			   const char *stereo_product_id)
{    


    LblDerivedImage_typ  DerivedImage;
    LblImageData_typ     ImageData;

    writeProductIds(file_models, nids);

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    if (imageType)
        strcpy(DerivedImage.DerivedImageType.Value, imageType);
    else
        strcpy(DerivedImage.DerivedImageType.Value, "XYZ_MAP");

    DerivedImage.DerivedImageType.Valid = 1;

    // write out the missing and invalid constants
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    // if we have three-banded image
    if (!strcmp(DerivedImage.DerivedImageType.Value, "XYZ_MAP")) {
	ImageData.MissingConstant[1].Value = 0.0;
	ImageData.MissingConstant[1].Valid = 1;
	ImageData.MissingConstant[2].Value = 0.0;
	ImageData.MissingConstant[2].Valid = 1;	

	ImageData.InvalidConstant[1].Value = 0.0;
	ImageData.InvalidConstant[1].Valid = 1;
	ImageData.InvalidConstant[2].Value = 0.0;
	ImageData.InvalidConstant[2].Valid = 1;	
    }

    strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());   
    DerivedImage.ReferenceCoordSystemName.Valid = 1;
    
    if (cs != NULL) {
      for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
	  DerivedImage.ReferenceCoordSystemIndex[cnt].Value = 
	    cs->getIndex(cnt);
	  DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
      }

      // Write solution_id string only if it's not NULL or "telemetry" 
      if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	  		strcasecmp(cs->getSolutionId(), "telemetry")) {
	  strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value, 
		 cs->getSolutionId());
	  DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
      }

      writeCSandItsReferences(cs);

    }

    // Set Identity fields
    setIdentification(NULL);

    if (baseline != 0.0) {
        DerivedImage.StereoBaseline.Value = baseline;
        DerivedImage.StereoBaseline.Valid = 1;
    }

    if (stereo_product_id != NULL) {
	strcpy(DerivedImage.StereoProductId.Value, stereo_product_id);
	DerivedImage.StereoProductId.Valid = 1;
    }

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }

    return;
}

//////////////////////////////////////////////////////////////////////////
// Write marsrfilt label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setRFilt(PigFileModel *file_models[], int nids,
                           PigCoordSystem *cs, double average_window,
			   double min_window, double max_window)
{    


    LblDerivedImage_typ  DerivedImage;
    LblImageData_typ     ImageData;

    writeProductIds(file_models, nids);

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    strcpy(DerivedImage.DerivedImageType.Value, "XYZ_FILTER_MAP");
    DerivedImage.DerivedImageType.Valid = 1;

    // write out the missing and invalid constants
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;
    ImageData.MissingConstant[1].Value = 0.0;
    ImageData.MissingConstant[1].Valid = 1;
    ImageData.MissingConstant[2].Value = 0.0;
    ImageData.MissingConstant[2].Valid = 1;	

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;
    ImageData.InvalidConstant[1].Value = 0.0;
    ImageData.InvalidConstant[1].Valid = 1;
    ImageData.InvalidConstant[2].Value = 0.0;
    ImageData.InvalidConstant[2].Valid = 1;	

    strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());   
    DerivedImage.ReferenceCoordSystemName.Valid = 1;
    
    if (cs != NULL) {
        for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Value = 
	    						cs->getIndex(cnt);
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
        }

        // Write solution_id string only if it's not NULL or "telemetry" 
        if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	  		strcasecmp(cs->getSolutionId(), "telemetry")) {
	    strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value, 
		 					cs->getSolutionId());
	    DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
        }

        writeCSandItsReferences(cs);

    }

    // Set Identity fields
    setIdentification(NULL);

    DerivedImage.AverageFilterWindow.Value = average_window;
    DerivedImage.AverageFilterWindow.Valid = 1;
    DerivedImage.MinFilterWindow.Value = min_window;
    DerivedImage.MinFilterWindow.Valid = 1;
    DerivedImage.MaxFilterWindow.Value = max_window;
    DerivedImage.MaxFilterWindow.Valid = 1;

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }

    return;
}

//////////////////////////////////////////////////////////////////////////
// Write patch label items
//////////////////////////////////////////////////////////////////////////                                                 

void PigLabelModel::setPatch(PigFileModel *file_models[], int nids,
			     PigCoordSystem *cs)
{


  LblDerivedImage_typ  DerivedImage;
  LblImageData_typ     ImageData;

  writeProductIds(file_models, nids);

  memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
  memset(&ImageData, 0, sizeof(LblImageData_typ));

  strcpy(DerivedImage.DerivedImageType.Value, "XYZ_FILTER_MAP");
  DerivedImage.DerivedImageType.Valid = 1;

  // write out the missing and invalid constants                                                                          
  ImageData.MissingConstant[0].Value = 0.0;
  ImageData.MissingConstant[0].Valid = 1;
  ImageData.MissingConstant[1].Value = 0.0;
  ImageData.MissingConstant[1].Valid = 1;
  ImageData.MissingConstant[2].Value = 0.0;
  ImageData.MissingConstant[2].Valid = 1;

  ImageData.InvalidConstant[0].Value = 0.0;
  ImageData.InvalidConstant[0].Valid = 1;
  ImageData.InvalidConstant[1].Value = 0.0;
  ImageData.InvalidConstant[1].Valid = 1;
  ImageData.InvalidConstant[2].Value = 0.0;
  ImageData.InvalidConstant[2].Valid = 1;

  strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());
  DerivedImage.ReferenceCoordSystemName.Valid = 1;

  if (cs != NULL) {
    for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
            DerivedImage.ReferenceCoordSystemIndex[cnt].Value =
	      cs->getIndex(cnt);
            DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
    }

    // Write solution_id string only if it's not NULL or "telemetry"                                                    
    if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	strcasecmp(cs->getSolutionId(), "telemetry")) {
      strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value,
	     cs->getSolutionId());
      DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
    }

    writeCSandItsReferences(cs);

  }

  // Set Identity fields                                                                                                  
  setIdentification(NULL);

  // write out Derived Image Property Group into the label                                                                
  LblSetDerivedImage("DERIVED_IMAGE_PARMS");
  int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
  if (RTN_FAILURE(status)) {
    printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
  }

  status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
  if (RTN_FAILURE(status)) {
    printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
  }

  return;
}

//////////////////////////////////////////////////////////////////////////
// Write marsuvw label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setUVW(PigFileModel *file_models[], int nids,
		           PigCoordSystem *cs, char *imageType)
{
    int result = 0;  // return value
    LblDerivedImage_typ  DerivedImage;
    LblImageData_typ     ImageData;

    writeProductIds(file_models, nids);

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    if (imageType)
        strcpy(DerivedImage.DerivedImageType.Value, imageType);
    else
        strcpy(DerivedImage.DerivedImageType.Value, "UVW_MAP");

    DerivedImage.DerivedImageType.Valid = 1;

    // write out the missing constant
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    // if we have three-banded image
    if (!strcmp(DerivedImage.DerivedImageType.Value, "UVW_MAP")) {
	ImageData.MissingConstant[1].Value = 0.0;
	ImageData.MissingConstant[1].Valid = 1;
	ImageData.MissingConstant[2].Value = 0.0;
	ImageData.MissingConstant[2].Valid = 1;

	ImageData.InvalidConstant[1].Value = 0.0;
	ImageData.InvalidConstant[1].Valid = 1;
	ImageData.InvalidConstant[2].Value = 0.0;
	ImageData.InvalidConstant[2].Valid = 1;	
    }

    strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());   
    DerivedImage.ReferenceCoordSystemName.Valid = 1;
    
    if (cs != NULL) {
        for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Value = 
	            cs->getIndex(cnt);
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
        }

        // Write solution_id string only if it's not NULL or "telemetry" 
        if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	  		strcasecmp(cs->getSolutionId(), "telemetry")) {
	    strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value, 
		 				cs->getSolutionId());
	    DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
        }

        writeCSandItsReferences(cs);

    }

    // Set Identity fields
    setIdentification(NULL);

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
        result = 1;
    }

    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
	result = 1;
    }

    return;
}

//////////////////////////////////////////////////////////////////////////
// Write marserror label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setError(PigFileModel *file_models[], int nids,
		             PigCoordSystem *cs, 
                             double error_parms[], int error_parms_cnt,
                             char *imageType)
{
   LblDerivedImage_typ  DerivedImage;
    LblImageData_typ     ImageData;

    writeProductIds(file_models, nids);

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    if (imageType) {
        strcpy(DerivedImage.DerivedImageType.Value, imageType);
        DerivedImage.DerivedImageType.Valid = 1;
    }

    strcpy(DerivedImage.ErrorModelName.Value, "MIPL_CONST_DISPARITY_PROJECTED_V1");
    DerivedImage.ErrorModelName.Valid = 1;
    strcpy(DerivedImage.ErrorModelDesc.Value, "MIPL_ERROR_METHODS.TXT");
    DerivedImage.ErrorModelDesc.Valid = 1;

    strcpy(DerivedImage.ErrorModelParmsName[0].Value, "DELTA_REF_LINE");
    strcpy(DerivedImage.ErrorModelParmsName[1].Value, "DELTA_REF_SAMPLE");
    strcpy(DerivedImage.ErrorModelParmsName[2].Value, "DELTA_DISP_LINE");
    strcpy(DerivedImage.ErrorModelParmsName[3].Value, "DELTA_DISP_SAMPLE");

    for (int cnt = 0; cnt < error_parms_cnt; cnt++) {
        DerivedImage.ErrorModelParms[cnt].Value = error_parms[cnt];
        DerivedImage.ErrorModelParms[cnt].Valid = 1;
        DerivedImage.ErrorModelParmsName[cnt].Valid = 1;
    }

    // write out the missing and invalid constants
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    // if we have three-banded image
    if ((!strcmp(DerivedImage.DerivedImageType.Value, "XYZ_ERROR_MAP")) ||
        (!strcmp(DerivedImage.DerivedImageType.Value, "RANGE_ERROR_MAP"))) {
        ImageData.MissingConstant[1].Value = 0.0;
        ImageData.MissingConstant[1].Valid = 1;
        ImageData.MissingConstant[2].Value = 0.0;
        ImageData.MissingConstant[2].Valid = 1;

        ImageData.InvalidConstant[1].Value = 0.0;
        ImageData.InvalidConstant[1].Valid = 1;
        ImageData.InvalidConstant[2].Value = 0.0;
        ImageData.InvalidConstant[2].Valid = 1;
    }

    strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());
    DerivedImage.ReferenceCoordSystemName.Valid = 1;

    if (cs != NULL) {
        for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
            DerivedImage.ReferenceCoordSystemIndex[cnt].Value =
            						cs->getIndex(cnt);
            DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
        }

        // Write solution_id string only if it's not NULL or "telemetry" 
        if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
            		strcasecmp(cs->getSolutionId(), "telemetry")) {
            strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value,
                 	cs->getSolutionId());
            DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
        }

        writeCSandItsReferences(cs);

    }

    // Set Identity fields
    setIdentification(NULL);

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }

    return;

}

//////////////////////////////////////////////////////////////////////////
// Write marsrange label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setRange(PigFileModel *file_models[], int nids,
                             const PigPoint &origin, PigCoordSystem *cs)
{   
    LblDerivedImage_typ  DerivedImage;
    LblImageData_typ     ImageData;

    writeProductIds(file_models, nids);

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    // write out the missing constant
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    strcpy(DerivedImage.DerivedImageType.Value, "RANGE_MAP");
    DerivedImage.DerivedImageType.Valid = 1;

    DerivedImage.RangeOriginVector.Value[0] = origin.getX();
    DerivedImage.RangeOriginVector.Value[1] = origin.getY();
    DerivedImage.RangeOriginVector.Value[2] = origin.getZ();
    DerivedImage.RangeOriginVector.Valid = 1;

    // Deal with the coord sys

    strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());   
    DerivedImage.ReferenceCoordSystemName.Valid = 1;
    
    if (cs != NULL) {
        for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Value = 
	    					cs->getIndex(cnt);
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
        }

        // Write solution_id string only if it's not NULL or "telemetry" 
        if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	  			strcasecmp(cs->getSolutionId(), "telemetry")) {
	    strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value, 
		 		cs->getSolutionId());
	    DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
        }

        writeCSandItsReferences(cs);

    }

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
      printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }
    // Now set Identity fields
    setIdentification(NULL);

    return;

}

//////////////////////////////////////////////////////////////////////////
// Write marsslope label items
// Origin can be NULL, in which case it is not written (it is only
// relevant for slope-rover-direction).
// SA is only written out if the type is "SOLAR_ENERGY_MAP".  It's a bit
// of a kludge, but works for now.
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setSlope(PigFileModel *file_models[], int nids,
                             PigCoordSystem *cs,
                             const PigPoint *origin, 
                             float sa, 
                             const char* slopeFunctionType )
{   
    LblDerivedImage_typ  DerivedImage;
    LblImageData_typ     ImageData;

    writeProductIds(file_models, nids);

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    if ( slopeFunctionType )
        strcpy(DerivedImage.DerivedImageType.Value, slopeFunctionType);
    else
        strcpy(DerivedImage.DerivedImageType.Value, "SLOPE_MAP");

    DerivedImage.DerivedImageType.Valid = 1;

    // write out the missing constant
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    if (origin != NULL) {
        DerivedImage.RangeOriginVector.Value[0] = origin->getX();
        DerivedImage.RangeOriginVector.Value[1] = origin->getY();
        DerivedImage.RangeOriginVector.Value[2] = origin->getZ();
        DerivedImage.RangeOriginVector.Valid = 1;
    }

    if (slopeFunctionType != NULL &&
	strcmp(slopeFunctionType, "SOLAR_ENERGY_MAP") == 0) {

        DerivedImage.SolarEnergyElevation.Value = sa;
        DerivedImage.SolarEnergyElevation.Valid = 1;
        strcpy(DerivedImage.SolarEnergyElevationUnit.Value, "DEGREES");
        DerivedImage.SolarEnergyElevationUnit.Valid = 1;
    }

    strcpy( DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName() );
    DerivedImage.ReferenceCoordSystemName.Valid = 1;

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }
    // Now set Identity fields
    setIdentification(NULL);

    return;

}

//////////////////////////////////////////////////////////////////////////
// Write reachability label items.  Note that this is basic; mission-specific
// subclasses need to augment.
//////////////////////////////////////////////////////////////////////////

int PigLabelModel::setReach(PigFileModel *file_models[], int nids)
{    
    writeProductIds(file_models, nids);

    setDerivedImageType("REACHABILITY_MAP");

    // Now set Identity fields
    setIdentification(NULL);

    return 0;

}

//////////////////////////////////////////////////////////////////////////
// Write goodness label items.  Note that this is basic; mission-specific
// subclasses need to augment.
//////////////////////////////////////////////////////////////////////////

int PigLabelModel::setGReach(PigFileModel *file_models[], int nids,
                             int *bands, int nbands, bool include_configs)
{    
    writeProductIds(file_models, nids);

    setDerivedImageType("GOODNESS_MAP");

    // Now set Identity fields
    setIdentification(NULL);

    return 0;

}

//////////////////////////////////////////////////////////////////////////
// Write preload label items.  Note that this is basic; mission-specific
// subclasses need to augment.  The third argument specifies the type of
// preload map in a mission-specific manner.
//////////////////////////////////////////////////////////////////////////

int PigLabelModel::setPreload(PigFileModel *file_models[], int nids,
			      int preload_type)
{    
    writeProductIds(file_models, nids);

    setDerivedImageType("PRELOAD_MAP");

    // Now set Identity fields
    setIdentification(NULL);

    return 0;

}

//////////////////////////////////////////////////////////////////////////
// Write basic marsrough label items.  Mission subclasses should augment.
// Flag is a mission-specific value (for MSL, indicates drill or DRT).
// Set to 0 if not needed.
//////////////////////////////////////////////////////////////////////////

int PigLabelModel::setRough(PigFileModel *file_models[], int nids,
                            PigCoordSystem *cs, float invalid_constant,int flag)
{ 
    int result = 0;
    LblDerivedImage_typ  DerivedImage;
    LblImageData_typ     ImageData;

    writeProductIds(file_models, nids);

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    // write out the missing constant
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    // write out the invalid constant
    ImageData.InvalidConstant[0].Value = invalid_constant;
    ImageData.InvalidConstant[0].Valid = 1;

    strcpy(DerivedImage.DerivedImageType.Value, "ROUGHNESS_MAP");
    DerivedImage.DerivedImageType.Valid = 1;

    strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());   
    DerivedImage.ReferenceCoordSystemName.Valid = 1;
    
    if (cs != NULL) {
        for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Value = 
	    				cs->getIndex(cnt);
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
        }

        // Write solution_id string only if it's not NULL or "telemetry" 
        if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	  		strcasecmp(cs->getSolutionId(), "telemetry")) {
	    strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value, 
		 	cs->getSolutionId());
	    DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
        }

        writeCSandItsReferences(cs);

    }

    // Now set Identity fields
    setIdentification(NULL);

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
	result = 1;
    }
    
    // write out ImageData Property Group into the label
    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
	result = 1;
    }

    return result;
}

//////////////////////////////////////////////////////////////////////////
// Write basic marsdepth label items.  Mission subclasses should augment.
//////////////////////////////////////////////////////////////////////////

int PigLabelModel::setDepth(PigFileModel *file_models[], int nids,
                            PigCoordSystem *cs, float invalid_constant)
{ 
    int result = 0;
    LblDerivedImage_typ  DerivedImage;
    LblImageData_typ     ImageData;

    writeProductIds(file_models, nids);

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    // write out the missing constant
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    // write out the invalid constant
    ImageData.InvalidConstant[0].Value = invalid_constant;
    ImageData.InvalidConstant[0].Valid = 1;

    strcpy(DerivedImage.DerivedImageType.Value, "DEPTH_MAP");
    DerivedImage.DerivedImageType.Valid = 1;

    strcpy(DerivedImage.ReferenceCoordSystemName.Value, cs->getFrameName());   
    DerivedImage.ReferenceCoordSystemName.Valid = 1;
    
    if (cs != NULL) {
        for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Value = 
	    				cs->getIndex(cnt);
	    DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
        }

        // Write solution_id string only if it's not NULL or "telemetry" 
        if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	  		strcasecmp(cs->getSolutionId(), "telemetry")) {
	    strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value, 
		 	cs->getSolutionId());
	    DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
        }

        writeCSandItsReferences(cs);

    }

    // Now set Identity fields
    setIdentification(NULL);

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
	result = 1;
    }
    
    // write out ImageData Property Group into the label
    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
	result = 1;
    }

    return result;
}

//////////////////////////////////////////////////////////////////////////
// Write marsmap Cylindrical projection label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setMapCyl(PigFileModel *file_models[],
			      RadiometryModel *rad_models[],
			      PigBrtCorrModel *brt_models[],
                              double radiance_factor[],
                              double radiance_offset[],
			      int nids,
			      PigSurfaceModel *surface_model,
			      double scale,       // radians/pixel
			      PigPoint proj_origin,
			      double line_zero_el,
			      double az_first_sample,
			      double az_last_sample,
			      double min_elev, 
			      double max_elev,
			      PigCoordSystem *cs) // projection coord system
{
    LblSurfaceProjection_typ SurfProj;

    memset(&SurfProj, 0, sizeof(LblSurfaceProjection_typ));

    // Fill in the surface projection fields

    strcpy(SurfProj.MapProjectionType.Value, "CYLINDRICAL");
    SurfProj.MapProjectionType.Valid = 1;
    
    // Line, Sample order
    SurfProj.MapResolution.Value[0] = 1.0 / PigRad2Deg(scale);
    SurfProj.MapResolution.Value[1] = 1.0 / PigRad2Deg(scale);
    SurfProj.MapResolution.Valid = 1;
    strcpy(SurfProj.MapResolutionUnit[0].Value, "PIXELS/DEGREE");
    strcpy(SurfProj.MapResolutionUnit[1].Value, "PIXELS/DEGREE");
    SurfProj.MapResolutionUnit[0].Valid = 1;
    SurfProj.MapResolutionUnit[1].Valid = 1;

    SurfProj.MaximumElevation.Value = PigRad2Deg(max_elev);
    SurfProj.MaximumElevation.Valid = 1;
    SurfProj.MinimumElevation.Value = PigRad2Deg(min_elev);
    SurfProj.MinimumElevation.Valid = 1;
    strcpy(SurfProj.MaximumElevationUnit.Value, "DEGREES");
    SurfProj.MaximumElevationUnit.Valid = 1;
    strcpy(SurfProj.MinimumElevationUnit.Value, "DEGREES");
    SurfProj.MinimumElevationUnit.Valid = 1;
    
    SurfProj.ProjectionOriginVector.Value[0] = proj_origin.getX();
    SurfProj.ProjectionOriginVector.Value[1] = proj_origin.getY();
    SurfProj.ProjectionOriginVector.Value[2] = proj_origin.getZ();
    SurfProj.ProjectionOriginVector.Valid = 1;

    az_first_sample = PigRad2Deg(az_first_sample);
    if (az_first_sample < 0.0) {
        az_first_sample += 360.0;
    }
    if (az_first_sample > 360.0) {
        az_first_sample -= 360.0;
    }
    SurfProj.StartAzimuth.Value = az_first_sample;
    SurfProj.StartAzimuth.Valid = 1;

    az_last_sample = PigRad2Deg(az_last_sample);
    if (az_last_sample < 0.0) {
        az_last_sample += 360.0;
    }
    if (az_last_sample > 360.0) {
        az_last_sample -= 360.0;
    }
    SurfProj.StopAzimuth.Value = az_last_sample;
    SurfProj.StopAzimuth.Valid = 1;
    strcpy(SurfProj.StartAzimuthUnit.Value, "DEGREES");
    SurfProj.StartAzimuthUnit.Valid = 1;
    strcpy(SurfProj.StopAzimuthUnit.Value, "DEGREES");
    SurfProj.StopAzimuthUnit.Valid = 1;

    SurfProj.ZeroElevationLine.Value = line_zero_el + 1;
    SurfProj.ZeroElevationLine.Valid = 1;

    setMosaic(file_models,
		rad_models, brt_models, radiance_factor, radiance_offset,
		nids, surface_model, cs, &SurfProj);
}

//////////////////////////////////////////////////////////////////////////
// Write marsmap Polar projection label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setMapPolar(PigFileModel *file_models[],
				RadiometryModel *rad_models[],
				PigBrtCorrModel *brt_models[],
                                double radiance_factor[],
                                double radiance_offset[],
				int nids,
				PigSurfaceModel *surface_model,
				double scale,       // radians/pixel
				PigPoint proj_origin,
				double up_azimuth,
				int nadir_line, 
				int nadir_samp,
				double max_elev,
				PigCoordSystem *cs) // projection coord system
{
    int status;
    LblSurfaceProjection_typ SurfProj;

    memset(&SurfProj, 0, sizeof(LblSurfaceProjection_typ));

    // Fill in the surface projection fields

    strcpy(SurfProj.MapProjectionType.Value, "POLAR");
    SurfProj.MapProjectionType.Valid = 1;
    
    SurfProj.LineProjectionOffset.Value = nadir_line + 1;
    SurfProj.LineProjectionOffset.Valid = 1;
    SurfProj.SampleProjectionOffset.Value = nadir_samp + 1;
    SurfProj.SampleProjectionOffset.Valid = 1;
    
    // Line, Sample order
    SurfProj.MapResolution.Value[0] = 1.0 / PigRad2Deg(scale);
    SurfProj.MapResolution.Value[1] = 1.0 / PigRad2Deg(scale);
    SurfProj.MapResolution.Valid = 1;
    strcpy(SurfProj.MapResolutionUnit[0].Value, "PIXELS/DEGREE");
    strcpy(SurfProj.MapResolutionUnit[1].Value, "PIXELS/DEGREE");
    SurfProj.MapResolutionUnit[0].Valid = 1;
    SurfProj.MapResolutionUnit[1].Valid = 1;

    SurfProj.ProjectionOriginVector.Value[0] = proj_origin.getX();
    SurfProj.ProjectionOriginVector.Value[1] = proj_origin.getY();
    SurfProj.ProjectionOriginVector.Value[2] = proj_origin.getZ();
    SurfProj.ProjectionOriginVector.Valid = 1;
    
    SurfProj.ReferenceAzimuth.Value = PigRad2Deg(up_azimuth);
    SurfProj.ReferenceAzimuth.Valid = 1;
    strcpy(SurfProj.ReferenceAzimuthUnit.Value, "DEGREES");
    SurfProj.ReferenceAzimuthUnit.Valid = 1;

    SurfProj.MaximumElevation.Value = PigRad2Deg(max_elev);
    SurfProj.MaximumElevation.Valid = 1;
    strcpy(SurfProj.MaximumElevationUnit.Value, "DEGREES");
    SurfProj.MaximumElevationUnit.Valid = 1;

    setMosaic(file_models,
		rad_models, brt_models, radiance_factor, radiance_offset,
		nids, surface_model, cs, &SurfProj);

}

//////////////////////////////////////////////////////////////////////////
// Write marsmap Vertical projection label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setMapVert(PigFileModel *file_models[],
			       RadiometryModel *rad_models[],
			       PigBrtCorrModel *brt_models[],
                               double radiance_factor[],
                               double radiance_offset[],
			       int nids,
			       PigSurfaceModel *surface_model,
			       int nlo, 
			       int nso,
			       double vert_scale,
			       double maxx, 
			       double maxy,
			       PigCoordSystem *cs) // projection coord system
{
    int status;
    LblSurfaceProjection_typ SurfProj;

    memset(&SurfProj, 0, sizeof(LblSurfaceProjection_typ));

    // Fill in the surface projection fields

    strcpy(SurfProj.MapProjectionType.Value, "VERTICAL");
    SurfProj.MapProjectionType.Valid = 1;
    
    // Originally this was computed as nlo/2, nso/2... which was wrong.
    // It was correct iff we were at the origin, and the mosaic was centered
    // on the image.  These values are supposed to be the location in line/samp
    // space of the origin of the coordinate system (which can be off the
    // image).  We can compute that from max_x/y and the scale.  Note we have
    // to add 1 to make them 1-based coords.

    // Note: vert_scale is pixel/m not m/pixel, thus the * rather than /
    SurfProj.LineProjectionOffset.Value = maxx*vert_scale + 1.0;
    SurfProj.LineProjectionOffset.Valid = 1;
    SurfProj.SampleProjectionOffset.Value = (nso - maxy*vert_scale) + 1.0;
    SurfProj.SampleProjectionOffset.Valid = 1;
    
    // Line, Sample order
    SurfProj.MapScale[0].Value = 1.0 / vert_scale;
    SurfProj.MapScale[0].Valid = 1;
    SurfProj.MapScale[1].Value = 1.0 / vert_scale;
    SurfProj.MapScale[1].Valid = 1;
    strcpy(SurfProj.MapScaleUnit[0].Value, "M/PIXEL");
    SurfProj.MapScaleUnit[0].Valid = 1;
    strcpy(SurfProj.MapScaleUnit[1].Value, "M/PIXEL");
    SurfProj.MapScaleUnit[1].Valid = 1;

    // Probably should set these directly via parameters, but this
    // accomplishes the same thing.

    SurfProj.XAxisMaximum.Value = maxx;
    SurfProj.XAxisMaximum.Valid = 1;
    SurfProj.XAxisMinimum.Value = maxx - nlo / vert_scale;
    SurfProj.XAxisMinimum.Valid = 1;
    SurfProj.YAxisMaximum.Value = maxy;
    SurfProj.YAxisMaximum.Valid = 1;
    SurfProj.YAxisMinimum.Value = maxy - nso / vert_scale;
    SurfProj.YAxisMinimum.Valid = 1;

    strcpy(SurfProj.XAxisMaximumUnit.Value, "M");
    SurfProj.XAxisMaximumUnit.Valid = 1;
    strcpy(SurfProj.XAxisMinimumUnit.Value, "M");
    SurfProj.XAxisMinimumUnit.Valid = 1;
    strcpy(SurfProj.YAxisMaximumUnit.Value, "M");
    SurfProj.YAxisMaximumUnit.Valid = 1;
    strcpy(SurfProj.YAxisMinimumUnit.Value, "M");
    SurfProj.YAxisMinimumUnit.Valid = 1;

    setMosaic(file_models,
		rad_models, brt_models, radiance_factor, radiance_offset,
		nids, surface_model, cs, &SurfProj);
}


//////////////////////////////////////////////////////////////////////////
// Write marsortho projection label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setMosOrtho(PigFileModel *file_models[],
			       RadiometryModel *rad_models[],
			       PigBrtCorrModel *brt_models[],
			       PigCoordSystem *cs,   // projection coord system
                               PigCoordSystem *z_cs, // derived image coord system
			       PigCoordSystem *write_cs, // extra CS to write
                               double radiance_factor[],
                               double radiance_offset[],
			       double ortho_scale,
			       double maxx, double maxy, 
                               double missing_constant, 
			       int nids, int nlo, int nso, int bands, 
                               int is_dem, int is_zup)
{
    int status;
    LblSurfaceProjection_typ SurfProj;
    LblImageData_typ ImageData;
    LblDerivedImage_typ DerivedImage;

    memset(&SurfProj, 0, sizeof(LblSurfaceProjection_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    // Fill in the surface projection fields

    strcpy(SurfProj.MapProjectionType.Value, "ORTHORECTIFIED");
    SurfProj.MapProjectionType.Valid = 1;
    
    // Originally this was computed as nlo/2, nso/2... which was wrong.
    // It was correct iff we were at the origin, and the mosaic was centered
    // on the image.  These values are supposed to be the location in line/samp
    // space of the origin of the coordinate system (which can be off the
    // image).  We can compute that from max_x/y and the scale.  Note we have
    // to add 1 to make them 1-based coords.

    // Note: ortho_scale is pixel/m not m/pixel, thus the * rather than /
    SurfProj.LineProjectionOffset.Value = maxx*ortho_scale + 1.0;
    SurfProj.LineProjectionOffset.Valid = 1;
    SurfProj.SampleProjectionOffset.Value = (nso - maxy*ortho_scale) + 1.0;
    SurfProj.SampleProjectionOffset.Valid = 1;
    
    // Line, Sample order
    SurfProj.MapScale[0].Value = 1.0 / ortho_scale;
    SurfProj.MapScale[0].Valid = 1;
    SurfProj.MapScale[1].Value = 1.0 / ortho_scale;
    SurfProj.MapScale[1].Valid = 1;
    strcpy(SurfProj.MapScaleUnit[0].Value, "M/PIXEL");
    SurfProj.MapScaleUnit[0].Valid = 1;
    strcpy(SurfProj.MapScaleUnit[1].Value, "M/PIXEL");
    SurfProj.MapScaleUnit[1].Valid = 1;

    // Probably should set these directly via parameters, but this
    // accomplishes the same thing.

    SurfProj.XAxisMaximum.Value = maxx;
    SurfProj.XAxisMaximum.Valid = 1;
    SurfProj.XAxisMinimum.Value = maxx - nlo / ortho_scale;
    SurfProj.XAxisMinimum.Valid = 1;
    SurfProj.YAxisMaximum.Value = maxy;
    SurfProj.YAxisMaximum.Valid = 1;
    SurfProj.YAxisMinimum.Value = maxy - nso / ortho_scale;
    SurfProj.YAxisMinimum.Valid = 1;

    strcpy(SurfProj.XAxisMaximumUnit.Value, "M");
    SurfProj.XAxisMaximumUnit.Valid = 1;
    strcpy(SurfProj.XAxisMinimumUnit.Value, "M");
    SurfProj.XAxisMinimumUnit.Valid = 1;
    strcpy(SurfProj.YAxisMaximumUnit.Value, "M");
    SurfProj.YAxisMaximumUnit.Valid = 1;
    strcpy(SurfProj.YAxisMinimumUnit.Value, "M");
    SurfProj.YAxisMinimumUnit.Valid = 1;

    setMosaic(file_models,
		rad_models, brt_models, radiance_factor, radiance_offset,
		nids, NULL, cs, &SurfProj);

    // write out the missing and invalid constants
    for (int b = 0; b < bands; b++) {
        ImageData.MissingConstant[b].Value = missing_constant;
        ImageData.MissingConstant[b].Valid = 1;

        ImageData.InvalidConstant[b].Value = 0.0;
        ImageData.InvalidConstant[b].Valid = 1;
    }

    // update REFERENCE_COORD_SYSTEM_NAME and REFERENCE_COORD_SYSTEM_INDEX
    // in DERIVED_IMAGE_PARMS group
    if (is_dem && z_cs != NULL) {
        strcpy(DerivedImage.ReferenceCoordSystemName.Value, z_cs->getFrameName());
        DerivedImage.ReferenceCoordSystemName.Valid = 1;

        if (z_cs != NULL) {
            for (int cnt = 0; cnt < z_cs->getNumIndices(); cnt++) {
	        DerivedImage.ReferenceCoordSystemIndex[cnt].Value = 
	            			z_cs->getIndex(cnt);
	        DerivedImage.ReferenceCoordSystemIndex[cnt].Valid = 1;
            }

            // Write solution_id string only if it's not NULL or "telemetry" 
            if (z_cs->getSolutionId() !=NULL && strlen(z_cs->getSolutionId())!=0
                	&& strcasecmp(z_cs->getSolutionId(), "telemetry")) {
                strcpy(DerivedImage.ReferenceCoordSystemSolnId.Value, 
                    	z_cs->getSolutionId());
                DerivedImage.ReferenceCoordSystemSolnId.Valid = 1;
            }

            writeCSandItsReferences(z_cs);

        }
    }
  
    if (is_dem && !is_zup) {
        setDerivedImageType("Z_MAP");
    }

    if (is_dem && is_zup) {
        setDerivedImageType("ELEVATION_MAP");
    }

    if (write_cs != NULL) {
	writeCSandItsReferences(write_cs);
    }

    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }

    status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }
}

//////////////////////////////////////////////////////////////////////////
// Write marsmap Sinusoidal projection label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setMapSin(PigFileModel *file_models[],
			      RadiometryModel *rad_models[],
			      PigBrtCorrModel *brt_models[],
                              double radiance_factor[],
                              double radiance_offset[],
			      int nids,
			      PigSurfaceModel *surface_model,
			      double scale,       // radians/pixel
			      PigPoint proj_origin,
			      double line_zero_el,
			      double az_first_sample,
			      double az_last_sample,
			      double min_elev, 
			      double max_elev,
			      double center_az,
			      double center_el,
			      PigCoordSystem *cs) // projection coord system
{
    LblSurfaceProjection_typ SurfProj;

    memset(&SurfProj, 0, sizeof(LblSurfaceProjection_typ));

    // Fill in the surface projection fields

    strcpy(SurfProj.MapProjectionType.Value, "SINUSOIDAL");
    SurfProj.MapProjectionType.Valid = 1;
    
    // Line, Sample order
    SurfProj.MapResolution.Value[0] = 1.0 / PigRad2Deg(scale);
    SurfProj.MapResolution.Value[1] = 1.0 / PigRad2Deg(scale);
    SurfProj.MapResolution.Valid = 1;
    strcpy(SurfProj.MapResolutionUnit[0].Value, "PIXELS/DEGREE");
    strcpy(SurfProj.MapResolutionUnit[1].Value, "PIXELS/DEGREE");
    SurfProj.MapResolutionUnit[0].Valid = 1;
    SurfProj.MapResolutionUnit[1].Valid = 1;

    SurfProj.MaximumElevation.Value = PigRad2Deg(max_elev);
    SurfProj.MaximumElevation.Valid = 1;
    SurfProj.MinimumElevation.Value = PigRad2Deg(min_elev);
    SurfProj.MinimumElevation.Valid = 1;
    strcpy(SurfProj.MaximumElevationUnit.Value, "DEGREES");
    SurfProj.MaximumElevationUnit.Valid = 1;
    strcpy(SurfProj.MinimumElevationUnit.Value, "DEGREES");
    SurfProj.MinimumElevationUnit.Valid = 1;
    
    SurfProj.ProjectionOriginVector.Value[0] = proj_origin.getX();
    SurfProj.ProjectionOriginVector.Value[1] = proj_origin.getY();
    SurfProj.ProjectionOriginVector.Value[2] = proj_origin.getZ();
    SurfProj.ProjectionOriginVector.Valid = 1;

    az_first_sample = PigRad2Deg(az_first_sample);
    if (az_first_sample < 0.0) {
        az_first_sample += 360.0;
    }
    if (az_first_sample > 360.0) {
        az_first_sample -= 360.0;
    }
    SurfProj.StartAzimuth.Value = az_first_sample;
    SurfProj.StartAzimuth.Valid = 1;

    az_last_sample = PigRad2Deg(az_last_sample);
    if (az_last_sample < 0.0) {
        az_last_sample += 360.0;
    }
    if (az_last_sample > 360.0) {
        az_last_sample -= 360.0;
    }
    SurfProj.StopAzimuth.Value = az_last_sample;
    SurfProj.StopAzimuth.Valid = 1;
    strcpy(SurfProj.StartAzimuthUnit.Value, "DEGREES");
    SurfProj.StartAzimuthUnit.Valid = 1;
    strcpy(SurfProj.StopAzimuthUnit.Value, "DEGREES");
    SurfProj.StopAzimuthUnit.Valid = 1;

    SurfProj.ZeroElevationLine.Value = line_zero_el + 1;
    SurfProj.ZeroElevationLine.Valid = 1;

    SurfProj.ProjectionElevation.Value = PigRad2Deg(center_el);
    SurfProj.ProjectionElevation.Valid = 1;
    strcpy(SurfProj.ProjectionElevationUnit.Value, "DEGREES");
    SurfProj.ProjectionElevationUnit.Valid = 1;
    SurfProj.ProjectionAzimuth.Value = PigRad2Deg(center_az);
    SurfProj.ProjectionAzimuth.Valid = 1;
    strcpy(SurfProj.ProjectionAzimuthUnit.Value, "DEGREES");
    SurfProj.ProjectionAzimuthUnit.Valid = 1;

    setMosaic(file_models,
		rad_models, brt_models, radiance_factor, radiance_offset,
		nids, surface_model, cs, &SurfProj);
}
//////////////////////////////////////////////////////////////////////////
// Write marsmos label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setMos(PigFileModel *file_models[],
			   RadiometryModel *rad_models[],
			   PigBrtCorrModel *brt_models[],
                           double radiance_factor[],
                           double radiance_offset[],
			   int nids,
			   PigSurfaceModel *surface_model,
			   PigCameraModel *camera_out,
			   double x_offset,
			   double y_offset,
			   PigCoordSystem *cs,
			   PigVector output_direction)
{    
    int status;
    LblSurfaceProjection_typ SurfProj;

    memset(&SurfProj, 0, sizeof(LblSurfaceProjection_typ));

    // Fill in the surface projection fields

    strcpy(SurfProj.MapProjectionType.Value, "PERSPECTIVE");
    SurfProj.MapProjectionType.Valid = 1;

    SurfProj.LineCameraModelOffset.Value = y_offset;
    SurfProj.LineCameraModelOffset.Valid = 1;
    SurfProj.SampleCameraModelOffset.Value = x_offset;
    SurfProj.SampleCameraModelOffset.Valid = 1;

    // Line, Sample order
    SurfProj.MapResolution.Value[0] = 1.0 /
				PigRad2Deg(camera_out->getPixelAngle(0));
    SurfProj.MapResolution.Value[1] = 1.0 /
				PigRad2Deg(camera_out->getPixelAngle(1));
    SurfProj.MapResolution.Valid = 1;
    strcpy(SurfProj.MapResolutionUnit[0].Value, "PIXELS/DEGREE");
    strcpy(SurfProj.MapResolutionUnit[1].Value, "PIXELS/DEGREE");
    SurfProj.MapResolutionUnit[0].Valid = 1;
    SurfProj.MapResolutionUnit[1].Valid = 1;

    // Azout, elout
    SurfProj.ProjectionAzimuth.Value = PigRad2Deg(cs->getAz(
						output_direction));
    SurfProj.ProjectionAzimuth.Valid = 1;
    SurfProj.ProjectionElevation.Value = PigRad2Deg(cs->getEl(
						output_direction));
    SurfProj.ProjectionElevation.Valid = 1;
    strcpy(SurfProj.ProjectionAzimuthUnit.Value, "DEGREES");
    SurfProj.ProjectionAzimuthUnit.Valid = 1;
    strcpy(SurfProj.ProjectionElevationUnit.Value, "DEGREES");
    SurfProj.ProjectionElevationUnit.Valid = 1;

    setMosaic(file_models,
		rad_models, brt_models, radiance_factor, radiance_offset,
		nids, surface_model, cs, &SurfProj);

    // Write the camera model.  Errors are printed by the routine
    writeCM(camera_out, cs);
}

//////////////////////////////////////////////////////////////////////////
// Write marsmcauley label items
//////////////////////////////////////////////////////////////////////////

void PigLabelModel::setMcauley(PigFileModel *file_models[],
			       RadiometryModel *rad_models[],
			       PigBrtCorrModel *brt_models[],
                               double radiance_factor[],
                               double radiance_offset[],
			       int nids,
			       PigSurfaceModel *surface_model,
			       PigCameraModel *camera_out,
			       double start_az,
			       double stop_az,		      
			       double proj_el, 
			       double proj_line,
			       PigCoordSystem *cs,
			       int write_ring,
			       PigPoint ring_center,
			       PigVector ring_axis,
			       PigVector ring_axis_new,
			       double ring_radius,
			       double start_az_site,
			       double stop_az_site,
			       PigCoordSystem *site_cs)
{
    int status;
    LblSurfaceProjection_typ SurfProj;
    LblDerivedGeometry_typ DerivedGeom;

    memset(&SurfProj, 0, sizeof(LblSurfaceProjection_typ));
    memset(&DerivedGeom, 0, sizeof(LblDerivedGeometry_typ));

    // Fill in the surface projection fields

    strcpy(SurfProj.MapProjectionType.Value, "CYLINDRICAL-PERSPECTIVE");
    SurfProj.MapProjectionType.Valid = 1;

    SurfProj.ProjectionElevation.Value = PigRad2Deg(proj_el);
    SurfProj.ProjectionElevation.Valid = 1;
    strcpy(SurfProj.ProjectionElevationUnit.Value, "DEGREES");
    SurfProj.ProjectionElevationUnit.Valid = 1;
    SurfProj.ProjectionElevationLine.Value = proj_line + 1;
    SurfProj.ProjectionElevationLine.Valid = 1;

    start_az = PigRad2Deg(start_az);
    if (start_az < 0.0) {
        start_az += 360.0;
    }
    if (start_az > 360.0) {
        start_az -= 360.0;
    }
    SurfProj.StartAzimuth.Value = start_az;
    SurfProj.StartAzimuth.Valid = 1;

    stop_az = PigRad2Deg(stop_az);
    if (stop_az < 0.0) {
        stop_az += 360.0;
    }
    if (stop_az > 360.0) {
        stop_az -= 360.0;
    }
    SurfProj.StopAzimuth.Value = stop_az;
    SurfProj.StopAzimuth.Valid = 1;
    strcpy(SurfProj.StartAzimuthUnit.Value, "DEGREES");
    SurfProj.StartAzimuthUnit.Valid = 1;
    strcpy(SurfProj.StopAzimuthUnit.Value, "DEGREES");
    SurfProj.StopAzimuthUnit.Valid = 1;

    // Line, Sample order
    SurfProj.MapResolution.Value[0] = 1.0 / PigRad2Deg(
				camera_out->getPixelAngle(0));
    SurfProj.MapResolution.Value[1] = 1.0 / PigRad2Deg(
				camera_out->getPixelAngle(1));
    SurfProj.MapResolution.Valid = 1;
    strcpy(SurfProj.MapResolutionUnit[0].Value, "PIXELS/DEGREE");
    strcpy(SurfProj.MapResolutionUnit[1].Value, "PIXELS/DEGREE");
    SurfProj.MapResolutionUnit[0].Valid = 1;
    SurfProj.MapResolutionUnit[1].Valid = 1;

    if (write_ring) {
	SurfProj.ProjectionOriginVector.Value[0] = ring_center.getX();
	SurfProj.ProjectionOriginVector.Value[1] = ring_center.getY();
	SurfProj.ProjectionOriginVector.Value[2] = ring_center.getZ();
	SurfProj.ProjectionOriginVector.Valid = 1;

	SurfProj.CameraRotationAxisVector.Value[0] = ring_axis.getX();
	SurfProj.CameraRotationAxisVector.Value[1] = ring_axis.getY();
	SurfProj.CameraRotationAxisVector.Value[2] = ring_axis.getZ();
	SurfProj.CameraRotationAxisVector.Valid = 1;

	SurfProj.ProjectionZAxisVector.Value[0] = ring_axis_new.getX();
	SurfProj.ProjectionZAxisVector.Value[1] = ring_axis_new.getY();
	SurfProj.ProjectionZAxisVector.Value[2] = ring_axis_new.getZ();
	SurfProj.ProjectionZAxisVector.Valid = 1;

	SurfProj.ProjectionAxisOffset.Value = ring_radius;
	SurfProj.ProjectionAxisOffset.Valid = 1;
	strcpy(SurfProj.ProjectionAxisOffsetUnit.Value, "M");
	SurfProj.ProjectionAxisOffsetUnit.Valid = 1;
    }

    setMosaic(file_models,
		rad_models, brt_models, radiance_factor, radiance_offset,
		nids, surface_model, cs, &SurfProj);

    // Write the camera model.  Errors are printed by the routine
    writeCM(camera_out, cs);

    // Write the SiteDerivedGeometry

    // CS first

    strcpy(DerivedGeom.ReferenceCoordSystemName.Value, site_cs->getFrameName());
    DerivedGeom.ReferenceCoordSystemName.Valid = 1;

    if (site_cs != NULL) {
	for (int cnt = 0; cnt < site_cs->getNumIndices(); cnt++) {
	    DerivedGeom.ReferenceCoordSystemIndex[cnt].Value = 
	      				site_cs->getIndex(cnt);
	    DerivedGeom.ReferenceCoordSystemIndex[cnt].Valid = 1;
	}

	// Write solution_id string only if it's not NULL or "telemetry" 
	if (site_cs->getSolutionId() != NULL &&
			strlen(site_cs->getSolutionId()) != 0 &&
	    		strcasecmp(site_cs->getSolutionId(), "telemetry")) {
	    strcpy(DerivedGeom.ReferenceCoordSystemSolnId.Value, 
		 	site_cs->getSolutionId());
	    DerivedGeom.ReferenceCoordSystemSolnId.Valid = 1;
	}
    }

    // Now the actual values

    start_az_site = PigRad2Deg(start_az_site);
    if (start_az_site < 0.0) {
        start_az_site += 360.0;
    }
    if (start_az_site > 360.0){
        start_az_site -= 360.0;
    }
    DerivedGeom.StartAzimuth.Value = start_az_site;
    DerivedGeom.StartAzimuth.Valid = 1;

    stop_az_site = PigRad2Deg(stop_az_site);
    if (stop_az_site < 0.0) {
        stop_az_site += 360.0;
    }
    if (stop_az_site > 360.0){
        stop_az_site -= 360.0;
    }
    DerivedGeom.StopAzimuth.Value = stop_az_site;
    DerivedGeom.StopAzimuth.Valid = 1;

    strcpy(DerivedGeom.StartAzimuthUnit.Value, "DEGREES");
    DerivedGeom.StartAzimuthUnit.Valid = 1;
    strcpy(DerivedGeom.StopAzimuthUnit.Value, "DEGREES");
    DerivedGeom.StopAzimuthUnit.Valid = 1;

    status = LblDerivedGeometryApi(_unit, LBL_WRITE, &DerivedGeom, 0,
		"SITE_DERIVED_GEOMETRY_PARMS");
    if (RTN_FAILURE(status))
        printMsg((char *)LblErrorMessage(), "SiteDerivedGeometryParms", PigMsgError);

}

////////////////////////////////////////////////////////////////////////
// Writes out given CameraModel to the _unit file.  If mission doesn't
// use LblCameraModel_typ structure for label read/write, then subclass 
// should overload this function.
////////////////////////////////////////////////////////////////////////
int PigLabelModel::writeCM(PigCameraModel* cm, PigCoordSystem *cs)
{
    int status;

    LblCameraModel_typ GeometricCameraModel;

    memset(&GeometricCameraModel, 0, sizeof(LblCameraModel_typ));

    // delete GEOMETRIC_CAMERA_MODEL, if there is any in the label
    del_prop_grp(_unit, "GEOMETRIC_CAMERA_MODEL", 1);

    if (cm->getCameraCalibration()) {
        strcpy(GeometricCameraModel.CalibrationSourceId.Value, 
	       cm->getCameraCalibration());
	GeometricCameraModel.CalibrationSourceId.Valid = 1;
    }

    strcpy(GeometricCameraModel.ReferenceCoordSystemName.Value, 
	   cs->getFrameName());
    GeometricCameraModel.ReferenceCoordSystemName.Valid = 1;

    if (cs != NULL) {
	for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
	    GeometricCameraModel.ReferenceCoordSystemIndex[cnt].Value = 
	      			cs->getIndex(cnt);
	    GeometricCameraModel.ReferenceCoordSystemIndex[cnt].Valid = 1;
	}

	// Write solution_id string only if it's not NULL or "telemetry" 
	if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	    		strcasecmp(cs->getSolutionId(), "telemetry")) {
	    strcpy(GeometricCameraModel.ReferenceCoordSystemSolnId.Value, 
		   	cs->getSolutionId());
	    GeometricCameraModel.ReferenceCoordSystemSolnId.Valid = 1;
	}
    }

    // Setup label fields the camera model
    cm->setupWriteToLabel(GeometricCameraModel, cs);

    status = LblGeometricCameraModel(_unit, LBL_AUGMENT, 
				     &GeometricCameraModel, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "GeometricCameraModel", 
		 PigMsgError);
	return 1;
    }
    else 
	// success, now write out Coord System that is referenced
	// by this Camera Model
        if (cs != NULL) {
	    writeCSandItsReferences(cs);
	}

    return 0;

}

////////////////////////////////////////////////////////////////////////
// Writes out given PointingModel to the _unit file.  If mission doesn't
// use LblDerivedImage_typ structure for label read/write, then subclass 
// should overload this function.
////////////////////////////////////////////////////////////////////////
int PigLabelModel::writePointingModel(PigPointingModel* pm)
{
    int status;
    int num_params = PIG_MAX_PARAMS;
    double params[PIG_MAX_PARAMS];

    LblDerivedImage_typ DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));


    strcpy(DerivedImage.PointingModelName.Value, pm->getModelName()); 
    DerivedImage.PointingModelName.Valid = 1;

    if (pm->getPointingParamCount() < PIG_MAX_PARAMS)
        num_params = pm->getPointingParamCount();

    pm->getPointingParameters(params, num_params);

    for (int cnt = 0; cnt < num_params; cnt++) {
	DerivedImage.PointingModelParams[cnt].Value = 
	      params[cnt];
	DerivedImage.PointingModelParams[cnt].Valid = 1;
        strcpy(DerivedImage.PointingModelParamsName[cnt].Value, 
	       pm->getPointingParamName(cnt));
        DerivedImage.PointingModelParamsName[cnt].Valid = 1;
    }

    //write out Derived Image Property Group into the label
    status = LblDerivedImageParms(_unit, LBL_AUGMENT, &DerivedImage, 1); 
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", 
		 PigMsgError);
	return 1;
    }

    return 0;

}
//////////////////////////////////////////////////////////////////////
// Given a list of FileModels, creates appropriate SOURCE_PRODUCT_ID  
//////////////////////////////////////////////////////////////////////
int PigLabelModel::writeSourceProductId(PigFileModel* file_models[], int nids)
{
    char* source_product_id;
    char *src_prod_ids[PIG_MAX_INPUTS];
    int num = 0; 
    int status = 0;

    // remove the keyword if it's been copied from input label
    zldel(_unit, "PROPERTY", "SOURCE_PRODUCT_ID", 
          "PROPERTY", "IDENTIFICATION", "ERR_ACT", "", NULL);
    
    for (int k = 0; k < PIG_MAX_INPUTS; k++)
        src_prod_ids[k] = NULL;

    int maxLength = 0;
    for (int i= 0; i < nids; i++) {
        if (file_models[i] == NULL)
            continue;

        int nElements;
        char format[24];
        // find out the number of elements to process
        file_models[i]->openFile(); //make sure the file is open
        status = zlinfo(file_models[i]->getUnit(), "PROPERTY", "SOURCE_PRODUCT_ID", 
                        format, &maxLength, &nElements, "PROPERTY", "IDENTIFICATION", 
                        "ERR_ACT", "", NULL);
        if (status != 1)
            continue;
        
        for (int cnt = 1; cnt <= nElements; cnt++) {
            source_product_id = file_models[i]->getSourceProductId(cnt);

            if (source_product_id == NULL)
                continue;
            
            // either add it to the list 
            // or dealocate memory for source_product_id if duplicate

            if (!findValue(src_prod_ids, num, source_product_id))
                   status = zladd(_unit, "PROPERTY",
                       "SOURCE_PRODUCT_ID", src_prod_ids[num-1],
                       "NELEMENT", 1, //"ULEN", maxLength,
                       "FORMAT", "STRING", "MODE", "INSERT",
                       "PROPERTY", "IDENTIFICATION", NULL);
        }
        
    }
           
    //dealocate the memory
    for (int k = 0; k < num; k++) {
        free(src_prod_ids[k]);
        src_prod_ids[k] = NULL;
    }
    
    if (RTN_FAILURE(status))
        return 0;
    return 1;
}

//////////////////////////////////////////////////////////////////////
// Given a list of FileModels, creates appropriate INPUT_PRODUCT_ID
// by pulling PRODUCT_IDs from each FileModel
//////////////////////////////////////////////////////////////////////
int PigLabelModel::writeInputProductId(PigFileModel* file_models[], int nids)
{
    char* product_id;
    char *prod_ids[PIG_MAX_INPUTS];
    int num = 0;
    int status = 0;

    // remove the keyword if it's been copied from input label
    zldel(_unit, "PROPERTY", "INPUT_PRODUCT_ID",
          "PROPERTY", "DERIVED_IMAGE_PARMS", "ERR_ACT", "", NULL);
   
    for (int k = 0; k < PIG_MAX_INPUTS; k++)
        prod_ids[k] = NULL;

    int maxLength = 0;
    for (int i= 0; i < nids; i++) {
        if (file_models[i] == NULL)
            continue;

        int nElements;
        char format[24];

        //product_id = file_models[i]->getProductId();
        const char* prod_str = file_models[i]->getProductId();
        if (prod_str == NULL)
            continue;
        product_id = (char* )malloc(strlen(prod_str)+1);
        strcpy(product_id, prod_str);

        // either add it to the list
        // or dealocate memory for product_id if duplicate
        if (!findValue(prod_ids, num, product_id)) {

            status = zladd(_unit, "PROPERTY",
                       "INPUT_PRODUCT_ID", prod_ids[num-1],
                       "NELEMENT", 1, //"ULEN", maxLength,
                       "FORMAT", "STRING", "MODE", "INSERT",
                       "PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
	}
       
    }

    //dealocate the memory
    for (int k = 0; k < num; k++) {
        free(prod_ids[k]);
        prod_ids[k] = NULL;
    }

    if (RTN_FAILURE(status))
        return 0;
    return 1;

}

//////////////////////////////////////////////////////////////////////
// Given a list of FileModels, calls the above two routines and creates
// SOURCE_PRODUCT_ID and INPUT_PRODUCT_ID
//////////////////////////////////////////////////////////////////////
void PigLabelModel::writeProductIds(PigFileModel* file_models[], int nids)
{
    writeSourceProductId(file_models, nids);
    writeInputProductId(file_models, nids);
}
////////////////////////////////////////////////////////////////////////
// Writes out given PigCoordSystem to the _unit file.  If mission doesn't
// use LblCoordinate structure for label read/write, then subclass should
// overload this function.
////////////////////////////////////////////////////////////////////////
int PigLabelModel::writeCS(PigCoordSystem* cs)
{
    LblCoordinate_typ lblCoordinate;
    LblCoordinate_typ lblCoordinate_read;
    int status;

    memset(&lblCoordinate, 0, sizeof(LblCoordinate_typ));
    memset(&lblCoordinate_read, 0, sizeof(LblCoordinate_typ));

    if (cs->getReference() == NULL)
	return 0;	// Can't write if no reference...but not really an error

    char prop_name[32];
    sprintf(prop_name, "%s_COORDINATE_SYSTEM", cs->getFrameShortName());
    LblSetCoordinate(prop_name);
    // Current missions like MER allow only single instance of given
    // coordinate system like ROVER_COORDINATE_SYSTEM, so if there is
    // one in the label different from what we want to write, we delete
    // it. 
    status = LblCoordinateApi(_unit, LBL_READ, &lblCoordinate_read, 1,(const char*)NULL);
    if (!RTN_FAILURE(status))
        del_prop_grp(_unit, prop_name, 1);

    setupWriteCS(cs, &lblCoordinate);

    status = LblCoordinateApi(_unit, LBL_AUGMENT, &lblCoordinate, 1,(const char*)NULL);
    if (RTN_FAILURE(status)) {
      printMsg((char *)LblErrorMessage(), "Coordinate", PigMsgError);
      return 1;
    }
    return 0;
}

//////////////////////////////////////////////////////////////////////
// check if this coordinate system already in the label, if not 
// write it in the label.  Then check if coordinate system's 
// reference coordinate system is in the label, if not write it 
// in the label.  Follow this chain until the fixed site is reached.
////////////////////////////////////////////////////////////////////// 
void PigLabelModel::writeCSandItsReferences(PigCoordSystem *cs)
{
    do {
        PigCSReference *self = cs->getIdentity();

	// Check to see if the CS is already there.  Don't write it again
	// if it is.

	if (!findCS(self)) {
	    // Not found, so write the CS
	    writeCS(cs);
	}
	else {
	    // Coord System already in the label.  Assume that all
	    // it's references also in there.
	    return;
	}

	if (cs == _mission->getFixedCS())
	    return;			// Don't need to go any farther

	// get the parent
	cs = cs->getReferenceCS();

    } while (cs != NULL);
}

//////////////////////////////////////////////////////////////////////
// Copy items from the CS to the label structure.  Will not override
// existing items.
//////////////////////////////////////////////////////////////////////

void PigLabelModel::setupWriteCS(PigCoordSystem* cs, LblCoordinate_typ *aux)
{
    int cnt;
  
    if (!aux->SolutionId.Valid) {
        if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	        (strcasecmp(cs->getSolutionId(), "telemetry"))) {
            strcpy(aux->SolutionId.Value, cs->getSolutionId());
            aux->SolutionId.Valid = 1;
        }
    }
  
    for (cnt = 0; cnt < cs->getNumIndices(); cnt++) {
        if (!aux->CoordinateSystemIndex[cnt].Valid) {
            aux->CoordinateSystemIndex[cnt].Value = cs->getIndex(cnt);
            aux->CoordinateSystemIndex[cnt].Valid = 1;
        }
    
        if (!aux->CoordinateSystemIndexName[cnt].Valid) {
            strcpy(aux->CoordinateSystemIndexName[cnt].Value,
						cs->getIndexName(cnt));
            aux->CoordinateSystemIndexName[cnt].Valid = 1;
        }		
    }

    if (!aux->CoordinateSystemName.Valid) {
        strcpy(aux->CoordinateSystemName.Value, cs->getFrameName());
        aux->CoordinateSystemName.Valid = 1;
    }
  
    if (!aux->OriginOffsetVector.Valid) {
        PigVector offsetVector = cs->getOffset();
        aux->OriginOffsetVector.Value[0] = offsetVector.getX();
        aux->OriginOffsetVector.Value[1] = offsetVector.getY();
        aux->OriginOffsetVector.Value[2] = offsetVector.getZ();
        aux->OriginOffsetVector.Valid = 1;
    }
  
    if (!aux->OriginRotationQuaternion.Valid) {
        double v[4];
        cs->getQuaternion().getComponents(v);
        aux->OriginRotationQuaternion.Value[0] = v[0];
        aux->OriginRotationQuaternion.Value[1] = v[1];
        aux->OriginRotationQuaternion.Value[2] = v[2];
        aux->OriginRotationQuaternion.Value[3] = v[3];
        aux->OriginRotationQuaternion.Valid = 1;
    }
  
    // Now deal with Reference Coord System

    PigCSReference *ref = cs->getReference();

    if (!aux->ReferenceCoordSystemName.Valid) {
        strcpy(aux->ReferenceCoordSystemName.Value, ref->getFrameName());   
        aux->ReferenceCoordSystemName.Valid = 1;
    }

    for (cnt = 0; cnt < ref->getNumIndices(); cnt++) {
	if (!aux->ReferenceCoordSystemIndex[cnt].Valid) {
	    aux->ReferenceCoordSystemIndex[cnt].Value = ref->getIndex(cnt);
	    aux->ReferenceCoordSystemIndex[cnt].Valid = 1;
	}
    }

    // Write solution_id string only if it's not NULL or "telemetry" 
    if (ref->getSolutionId() != NULL && strlen(ref->getSolutionId()) != 0 &&
	  strcasecmp(ref->getSolutionId(), "telemetry") != 0) {
	if (!aux->ReferenceCoordSystemSolnId.Valid) {
	    strcpy(aux->ReferenceCoordSystemSolnId.Value, ref->getSolutionId());
	    aux->ReferenceCoordSystemSolnId.Valid = 1;
        }
    }
  
    // Set the positive az/el directions
    if (!aux->PositiveAzimuthDirection.Valid) {
      strcpy(aux->PositiveAzimuthDirection.Value,
	   cs->getAzimuthDirection() > 0 ? "CLOCKWISE" : "COUNTERCLOCKWISE");
      aux->PositiveAzimuthDirection.Valid = TRUE;
    }
    // This is weird because cs->getElevationDirection doesn't define up/down,
    // it defines toward +z or -z.  So we infer up/down by the combination
    // of the az and el directions (by the Right-Hand-Rule, CW Az (1) implies
    // Z down while CCW Az (-1) implies Z up.
    if (!aux->PositiveElevationDirection.Valid) {
        strcpy(aux->PositiveElevationDirection.Value,
	   cs->getAzimuthDirection() * cs->getElevationDirection() > 0 ?
							"DOWN":"UP");
        aux->PositiveElevationDirection.Valid = TRUE;
    }	

}

//////////////////////////////////////////////////////////////////////

#define uniqueLabel(Set, inSet, Item)					      \
    if (inSet.Item[0].Valid) {						      \
	int max = sizeof(Set.Item) / sizeof(Set.Item[0]);		      \
	for (int i=0; i <= max; i++) {					      \
	    if (i < max && Set.Item[i].Valid) {				      \
		if (strcasecmp(Set.Item[i].Value, inSet.Item[0].Value) == 0)  \
		    break;	/* found a match, not unique */		      \
	    }								      \
	    else {							      \
		if (i < max) {	/* unique, and there's room; add it */	      \
		    strcpy(Set.Item[i].Value, inSet.Item[0].Value);	      \
		    Set.Item[i].Valid = 1;				      \
		    break;						      \
		}							      \
		else {		/* unique but no room, trash last value */    \
		    strcpy(Set.Item[max-1].Value, "ETC");		      \
		    break;						      \
		}							      \
	    }								      \
	}								      \
    }

//////////////////////////////////////////////////////////////////////
// Writes the standard multi-input properties to the label.  Currently
// these properties include IDENTIFICATION, SURFACE_MODEL_PARMS, 
// SURFACE_PROJECTION_PARMS as well as radiometry labels in 
// DERIVED_IMAGE_PARMS.  Certain items (see the code) are added to these 
// structures.  If you want other items in there, you may
// pass in partially-filled structures with additional items.  Fields with
// the Valid flag set will *not* be overridden.  If these parameters 
// are NULL, empty structures will be created.
//
// The rad_models parameter may be NULL if no rad models are present.
// If the rad models do not all agree, the radiometric scaling factors
// will not be written (i.e. it is an uncalibrated image).
//////////////////////////////////////////////////////////////////////
void PigLabelModel::setMosaic(PigFileModel *file_models[],
			      RadiometryModel *rad_models[],
			      PigBrtCorrModel *brt_models[],
                              double radiance_factor[],
                              double radiance_offset[],
			      int nids,
			      PigSurfaceModel *surface_model,
			      PigCoordSystem *cs,
			      LblSurfaceProjection_typ *aux_surfProj)
{
    // First check if there is any good (successfully opened) file.

    PigFileModel *a_good_file = NULL;
    for (int i = 0; (i < nids) && !a_good_file; i++)
	a_good_file = file_models[i];
    if (a_good_file == NULL) {
        printError("Could not find a good file to read!");
        return;
    }

    int status;
    int count;
    LblIdentification_typ    Ident, inIdent;
    LblSurfaceProjection_typ SurfProj;
    LblSurfaceModel_typ      SurfModel;
    LblImageData_typ         ImageData;
    LblDerivedImage_typ      DerImg, inDerImg;

    if (aux_surfProj)
	memcpy(&SurfProj, aux_surfProj, sizeof(LblSurfaceProjection_typ));
    else
	memset(&SurfProj, 0, sizeof(LblSurfaceProjection_typ));

    memset(&Ident, 0, sizeof(LblIdentification_typ));
    memset(&SurfModel, 0, sizeof(LblSurfaceModel_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));
    memset(&DerImg, 0, sizeof(LblDerivedImage_typ));

    // write out the missing and invalid constants
    ImageData.MissingConstant[0].Value = 0.0;
    ImageData.MissingConstant[0].Valid = 1;

    ImageData.InvalidConstant[0].Value = 0.0;
    ImageData.InvalidConstant[0].Valid = 1;

    setIdentification(&Ident);

    // Populate SpacecraftClock(Start/Stop)Count
    // For mosaics, the min of all ...StartCount goes into the label
    // as the SpacecraftClockStartCount value.
    // The max of all ....StopCount goes into the label as the 
    // SpacecraftClockStopCount value.
    char start_sclk[100];
    char stop_sclk[100];

    // initialize strings
    const char *s = a_good_file->getSpacecraftClockStartCount();
    strcpy(start_sclk, (s==NULL) ? "" : s);
    s = a_good_file->getSpacecraftClockStopCount();
    strcpy(stop_sclk, (s==NULL) ? "" : s);
    for (int cnt = 0; cnt < nids; cnt++) {
        if ((file_models[cnt] == NULL) || (file_models[cnt] == a_good_file))
            continue;

        // check if current is smaller than current min
	s = file_models[cnt]->getSpacecraftClockStartCount();
        if (s != NULL && strcmp(s, start_sclk) < 0)
	  strcpy(start_sclk, s);

	// check if current is greater than current max
	s = file_models[cnt]->getSpacecraftClockStopCount();
        if (s != NULL && strcmp(s, stop_sclk) > 0)
	  strcpy(stop_sclk, s);
	  
    }
    if (!Ident.SpacecraftClockStartCount.Valid) {
	strcpy(Ident.SpacecraftClockStartCount.Value, start_sclk);
	Ident.SpacecraftClockStartCount.Valid = 1;
    }
    if (!Ident.SpacecraftClockStopCount.Valid) {
	strcpy(Ident.SpacecraftClockStopCount.Value, stop_sclk);
	Ident.SpacecraftClockStopCount.Valid = 1;
    }

    // Get the current date/time

    time_t clock;
    time(&clock);
    strftime(Ident.ProductCreationTime.Value,
	     sizeof(Ident.ProductCreationTime.Value),
	     "%Y-%m-%dT%H:%M:%S.000Z",
	     gmtime(&clock));
    Ident.ProductCreationTime.Valid = 1;

    strcpy(SurfProj.ReferenceCoordSystemName.Value, cs->getFrameName());
    SurfProj.ReferenceCoordSystemName.Valid = 1;

    if (surface_model) {
    	strcpy(SurfModel.ReferenceCoordSystemName.Value,
	   	surface_model->getCoordSystem()->getFrameName());
    	SurfModel.ReferenceCoordSystemName.Valid = 1;
    }

    if (cs != NULL) {
	for (int cnt = 0; cnt < cs->getNumIndices(); cnt++) {
	    SurfProj.ReferenceCoordSystemIndex[cnt].Value = 
	      				cs->getIndex(cnt);
	    SurfProj.ReferenceCoordSystemIndex[cnt].Valid = 1;
	}

	// Write solution_id string only if it's not NULL or "telemetry" 
	if (cs->getSolutionId() != NULL && strlen(cs->getSolutionId()) != 0 &&
	    		strcasecmp(cs->getSolutionId(), "telemetry")) {
	    strcpy(SurfProj.ReferenceCoordSystemSolnId.Value, 
		 	cs->getSolutionId());
	    SurfProj.ReferenceCoordSystemSolnId.Valid = 1;
	}
    }

    if (surface_model) {
	PigCoordSystem *surf_cs = surface_model->getCoordSystem();
	if (surf_cs != NULL) {
	    for (int cnt = 0; cnt < surf_cs->getNumIndices(); cnt++) {
	        SurfModel.ReferenceCoordSystemIndex[cnt].Value = 
	      				surf_cs->getIndex(cnt);
	        SurfModel.ReferenceCoordSystemIndex[cnt].Valid = 1;
	    }

	    // Write solution_id string only if it's not NULL or "telemetry" 
	    if (surf_cs->getSolutionId() != NULL &&
			strlen(surf_cs->getSolutionId()) != 0 &&
	    		strcasecmp(surf_cs->getSolutionId(), "telemetry")) {	  
	  	strcpy(SurfModel.ReferenceCoordSystemSolnId.Value, 
		 			surf_cs->getSolutionId());
	  	SurfModel.ReferenceCoordSystemSolnId.Valid = 1;
	    }
	}
    }

    writeProductIds(file_models, nids);

    // Now loop through all the inputs and gather stuff from there.
    // For each interesting label item, we gather just the unique values
    // from all the inputs in the output array.

    for (int file=0; file < nids; file++) {
        if (file_models[file] == NULL)
            continue;

	const LblIdentification_typ *inIdentP =
				file_models[file]->getLblIdentification();
	if (inIdentP == NULL)				// no label
	    memset(&inIdent, 0, sizeof(LblIdentification_typ));
	else
	    memcpy(&inIdent, inIdentP, sizeof(LblIdentification_typ));

 	const LblDerivedImage_typ *inDerImgP =
				file_models[file]->getLblDerivedImage();
	if (inDerImgP == NULL)
	    memset(&inDerImg, 0, sizeof(LblDerivedImage_typ));
	else
	    memcpy(&inDerImg, inDerImgP, sizeof(LblDerivedImage_typ));

	file_models[file]->closeFile();		// avoid too many open

	// Search the inputs for unique labels.  This is a macro rather than
	// a subroutine because the data types are different inside the
	// label structures, but the access syntax is the same.

	uniqueLabel(Ident, inIdent, InstrumentHostId);
	uniqueLabel(Ident, inIdent, InstrumentHostName);
	uniqueLabel(Ident, inIdent, InstrumentId);
	uniqueLabel(Ident, inIdent, InstrumentName);
	uniqueLabel(Ident, inIdent, InstrumentType);
	uniqueLabel(Ident, inIdent, MissionName);
	uniqueLabel(Ident, inIdent, FrameId);

	// Calculate the minimum start time

	if (inIdent.StartTime.Valid) {
	    if (!Ident.StartTime.Valid) {	// nothing yet, just copy
		strcpy(Ident.StartTime.Value, inIdent.StartTime.Value);
		Ident.StartTime.Valid = 1;
	    }
	    else {				// compare
		SfocTime_typ old_time = *AsciiToSfocTime(
				Ident.StartTime.Value, 0);
		SfocTime_typ new_time = *AsciiToSfocTime(
				inIdent.StartTime.Value, 0);
		MinSfocTime(&old_time, &new_time);	// min goes in old_time
		strcpy(Ident.StartTime.Value, SfocTimeToAscii(&old_time, 0));
		strcat(Ident.StartTime.Value, "Z");
		Ident.StartTime.Valid = 1;
	    }
	}

	// Calculate the maximum stop time

	if (inIdent.StopTime.Valid) {
	    if (!Ident.StopTime.Valid) {	// nothing yet, just copy
		strcpy(Ident.StopTime.Value, inIdent.StopTime.Value);
		Ident.StopTime.Valid = 1;
	    }
	    else {				// compare
		SfocTime_typ old_time = *AsciiToSfocTime(
				Ident.StopTime.Value, 0);
		SfocTime_typ new_time = *AsciiToSfocTime(
				inIdent.StopTime.Value, 0);
		MaxSfocTime(&old_time, &new_time);	// max goes in old_time
		strcpy(Ident.StopTime.Value, SfocTimeToAscii(&old_time, 0));
		strcat(Ident.StopTime.Value, "Z");
		Ident.StopTime.Valid = 1;
	    }
	}

	// Transfer over the derived image type and related fields
	// Note, we transfer radiometric_correction_type here even if
	// use_rad is 0 because we might be using already rad'd inputs
	// and we want to preserve the type of correction in the label.
	// The offset/factor won't be there because they vary in the inputs,
	// but hopefully that's been taken care of via iof->iff processing.

	if (file == 0) {
	    if (inDerImg.DerivedImageType.Valid) {
		strcpy(DerImg.DerivedImageType.Value,
				inDerImg.DerivedImageType.Value);
		DerImg.DerivedImageType.Valid = 1;
	    }

	    if (inDerImg.RangeOriginVector.Valid) {
		memcpy(DerImg.RangeOriginVector.Value,
				inDerImg.RangeOriginVector.Value,
				sizeof(inDerImg.RangeOriginVector.Value));
		DerImg.RangeOriginVector.Valid = 1;
	    }

	    if (inDerImg.RadiometricCorrectionType.Valid) {
		strcpy(DerImg.RadiometricCorrectionType.Value,
				inDerImg.RadiometricCorrectionType.Value);
		DerImg.RadiometricCorrectionType.Valid = 1;
	    }

	    if (inDerImg.ReferenceCoordSystemName.Valid) {
		strcpy(DerImg.ReferenceCoordSystemName.Value,
				inDerImg.ReferenceCoordSystemName.Value);
		DerImg.ReferenceCoordSystemName.Valid = 1;
	    }

	    if (inDerImg.ReferenceCoordSystemSolnId.Valid) {
		strcpy(DerImg.ReferenceCoordSystemSolnId.Value,
				inDerImg.ReferenceCoordSystemSolnId.Value);
		DerImg.ReferenceCoordSystemSolnId.Valid = 1;
	    }

	    for (int i=0; i < LBL_COORD_SYS_INDEX; i++) {
		if (inDerImg.ReferenceCoordSystemIndex[i].Valid) {
		    DerImg.ReferenceCoordSystemIndex[i].Value =
			inDerImg.ReferenceCoordSystemIndex[i].Value;
		    DerImg.ReferenceCoordSystemIndex[i].Valid = 1;
		}
	    }
	}

	// For other than index 0, just check.  Set to Invalid if they
	// differ (since there's no way to indicate a variance).

	else {
	    if (inDerImg.DerivedImageType.Valid) {
		if (strcmp(DerImg.DerivedImageType.Value,
				inDerImg.DerivedImageType.Value) != 0) {
		    DerImg.DerivedImageType.Valid = 0;
		}
	    }

	    if (inDerImg.RangeOriginVector.Valid) {
		if ((DerImg.RangeOriginVector.Value[0] !=
				inDerImg.RangeOriginVector.Value[0]) ||
		    (DerImg.RangeOriginVector.Value[1] !=
				inDerImg.RangeOriginVector.Value[1]) ||
		    (DerImg.RangeOriginVector.Value[2] !=
				inDerImg.RangeOriginVector.Value[2])) {

		    DerImg.RangeOriginVector.Valid = 0;
		}
	    }

	    if (inDerImg.RadiometricCorrectionType.Valid) {
		if (strcmp(DerImg.RadiometricCorrectionType.Value,
			inDerImg.RadiometricCorrectionType.Value) != 0) {
		    DerImg.RadiometricCorrectionType.Valid = 0;
		}
	    }

	    if (inDerImg.ReferenceCoordSystemName.Valid) {
		if (strcmp(DerImg.ReferenceCoordSystemName.Value,
				inDerImg.ReferenceCoordSystemName.Value) != 0) {
		    DerImg.ReferenceCoordSystemName.Valid = 0;
		}
	    }

	    if (inDerImg.ReferenceCoordSystemSolnId.Valid) {
		if (strcmp(DerImg.ReferenceCoordSystemSolnId.Value,
			inDerImg.ReferenceCoordSystemSolnId.Value) != 0) {
		    DerImg.ReferenceCoordSystemSolnId.Valid = 0;
		}
	    }

	    for (int i=0; i < LBL_COORD_SYS_INDEX; i++) {
		if (inDerImg.ReferenceCoordSystemIndex[i].Valid) {
		    if (DerImg.ReferenceCoordSystemIndex[i].Value !=
			inDerImg.ReferenceCoordSystemIndex[i].Value) {

		        DerImg.ReferenceCoordSystemIndex[i].Valid = 0;
		    }
		}
	    }
	}		// end index != 0

    }			// end file loop

    // Write Identification labels
    status = LblIdentification(_unit, LBL_AUGMENT, &Ident, 1);
    if (RTN_FAILURE(status))
        printMsg((char *)LblErrorMessage(), "Identification", PigMsgError);

    // Write Derived Image labels
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerImg, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    // Write out the coord system(s)
    if (cs != NULL)
	writeCSandItsReferences(cs);

    // Write out the derived image coord system... only if it's valid above
    // (as in, it matches across all inputs).  We don't match solution id
    // since it's not usually there.  We use the last inDerImg to tell which
    // indices are valid, which is good enough since they all have to match.

    if (DerImg.ReferenceCoordSystemName.Valid) {
	int do_it = TRUE;
	for (int i=0; i < LBL_COORD_SYS_INDEX; i++) {
	    if (inDerImg.ReferenceCoordSystemIndex[i].Valid &&
		!DerImg.ReferenceCoordSystemIndex[i].Valid) {
		do_it = FALSE;
	    }
	}
	if (do_it) {
	    // Okay to write.  Get the DerImg CSRef from the first file
	    // then get its CS.  Only write if we really got a CSRef.
	    PigCSReference *di_ref;
	    int status = a_good_file->getDerivedImageCS(di_ref);
	    if (status) {
		PigMission *m = PigMission::getMissionObject(
					a_good_file->getMissionName());
		PigCoordSystem *di_cs = m->getCoordSystem(di_ref);
		if (di_cs != NULL) {
		    writeCSandItsReferences(di_cs);
		}
	    }
	}
    }

    // Write radiometric scaling labels (but not product ID's)
    setRadiometric(NULL, nids, rad_models, nids);

    setBrtCorr(brt_models, radiance_factor, radiance_offset, nids);

    // write out Image Data Property Group into the label
    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }

    // Write the surface projection if provided.  Otherwise just ignore it.
    if (aux_surfProj != NULL) {
        status = LblSurfaceProjectionParms(_unit, LBL_WRITE, &SurfProj, 0);
        if (RTN_FAILURE(status))
            printMsg((char *)LblErrorMessage(), "SurfaceProjectionParms",
								PigMsgError);
    }

    // Write SurfaceModel
    if (surface_model)
    	surface_model->writeToLabel(_unit, 1, &SurfModel);
}

////////////////////////////////////////////////////////////////////////
// Write the inverted status to the label (for marsinverter).
// The base class implementation is appropriate for MER and MSL
// at least.  Other missions could override this if necessary.
// However, in most cases, PigFileModel::isIlutNeeded() will be
// called first, and this routine will only be called if that returns
// true.  So at the present time, no mission overrides are needed.
// Even if called, it should be benign, as the label will either not
// exist or not have the proper values to be changed.
//
// If the existing mode is "SOFTWARE" or "HARDWARE" and the value is
// TRUE, then change the mode to "SOFTWARE_INVERTED" or "HARDWARE_INVERTED".
// If the value is FALSE, the reverse happens (there is no current use
// case for FALSE).  If the value is anything else, it remins unchanged.
//
// Returns TRUE if the value was actually changed, FALSE if not.  Most
// callers can ignore the return value.
//
// If used_file and flag are both TRUE, then the provided LUT filename
// is also written out.
////////////////////////////////////////////////////////////////////////
int PigLabelModel::setInverted(PigFileModel *file_models[], int nids,
                               int flag, int used_file, char *filename)
{
    LblInstrumentState_typ InstrumentState;
    LblInstrumentState_typ InstrumentState_out;
    LblDerivedImage_typ  DerivedImage;
    int status;
    int rtn_status = TRUE;

    writeProductIds(file_models, nids);

    setIdentification(NULL);

    memset(&InstrumentState_out, 0, sizeof(LblInstrumentState_typ));

    // Read the property

    LblSetInstrumentState("INSTRUMENT_STATE_PARMS");
    status = LblInstrumentStateApi(_unit, LBL_READ, &InstrumentState, 1);
    if (RTN_FAILURE(status))
        return FALSE;				// quiet return if not found

    if (!InstrumentState.SampleBitMethod.Valid)
	return FALSE;				// label item not there

    if (flag) {					// Change to INVERTED
	if (strcasecmp(InstrumentState.SampleBitMethod.Value,
						"SOFTWARE") == 0) {
	    strcpy(InstrumentState_out.SampleBitMethod.Value,
						"SOFTWARE_INVERTED");
	}
	else if (strcasecmp(InstrumentState.SampleBitMethod.Value,
						"HARDWARE") == 0) {
	    strcpy(InstrumentState_out.SampleBitMethod.Value,
						"HARDWARE_INVERTED");
	}
	else
	    rtn_status = FALSE;			// not the right value to change
    } else {					// Take away INVERTED
	if (strcasecmp(InstrumentState.SampleBitMethod.Value,
						"SOFTWARE_INVERTED") == 0) {
	    strcpy(InstrumentState_out.SampleBitMethod.Value, "SOFTWARE");
	}
	else if (strcasecmp(InstrumentState.SampleBitMethod.Value,
						"HARDWARE_INVERTED") == 0) {
	    strcpy(InstrumentState_out.SampleBitMethod.Value, "HARDWARE");
	}
	else
	    rtn_status = FALSE;			// not the right value to change
    }

    // Now write out the modified label

    if (rtn_status == TRUE) {
        InstrumentState_out.SampleBitMethod.Valid = 1;
        LblSetInstrumentState("INSTRUMENT_STATE_PARMS");
        status=LblInstrumentStateApi(_unit,LBL_AUGMENT,&InstrumentState_out,1);
        if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "InstrumentState", PigMsgError);
            rtn_status = FALSE;			// error writing
	}
    }

    // Now do the filename if needed

    if (flag && used_file) {

        memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

	int maxlen = sizeof(DerivedImage.InverseLutFileName.Value);
	strncpy(DerivedImage.InverseLutFileName.Value, filename, maxlen);
	DerivedImage.InverseLutFileName.Value[maxlen] = '\0';	// just in case
	DerivedImage.InverseLutFileName.Valid = 1;

        // write out Derived Image Property Group into the label
        LblSetDerivedImage("DERIVED_IMAGE_PARMS");
        int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
        if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
	}
    }

    return rtn_status;			// changed successfully
}

////////////////////////////////////////////////////////////////////////
// Sets the SAMPLE_BIT_MASK field.
//
// The supplied value indicates how many bits should be active.  That
// many low-order bits are turned on in the mask.  If the value is 0,
// the label is removed (as being no longer relevant).  If the value is
// -1, all bits are set (indicating the entire value is usable).
//
// The data type of the file is also important.  The size of the file
// pixels determines how many bits are present (so a BYTE file will
// always have 8 bits, HALF will always have 16, etc).  Since the bit
// mask is only valid for integral types, it will be quietly removed
// (regardless of the supplied value) for floating-point types.
//
// If the supplied value is larger than the available data size, a warning
// is printed and all available bits are set.
//
// The returned value indicates the number of bits actually set, which
// may differ from the request in the warning or -1 cases.  A value of
// 0 means the label item was removed.  Most callers can ignore the return.
////////////////////////////////////////////////////////////////////////

int PigLabelModel::setBitMask(int setbits)
{
    char format[64];
    int delete_lbl = FALSE;
    int totalbits = 32;
    int rtn = 0;
    int status, i;

    LblImageData_typ     ImageData;

    memset(&ImageData, 0, sizeof(LblImageData_typ));

    // Get format from file

    zvget(_unit, "FORMAT", format, NULL);
    if (strcasecmp(format, "BYTE") == 0)
	totalbits = 8;
    if (strcasecmp(format, "HALF") == 0)
	totalbits = 16;
    if (strcasecmp(format, "WORD") == 0)	// obsolete value
	totalbits = 16;
    if (strcasecmp(format, "FULL") == 0)
	totalbits = 32;
    if (strcasecmp(format, "LONG") == 0)	// obsolete value
	totalbits = 32;
    if (strcasecmp(format, "REAL") == 0)
	delete_lbl = TRUE;
    if (strcasecmp(format, "DOUB") == 0)
	delete_lbl = TRUE;
    if (strcasecmp(format, "COMP") == 0)
	delete_lbl = TRUE;
    if (strcasecmp(format, "COMPLEX") == 0)	// obsolete value
	delete_lbl = TRUE;

    if (setbits == 0)
	delete_lbl = TRUE;
    if (setbits < 0)
	setbits = totalbits;
    if (setbits > totalbits)
	setbits = totalbits;

    if (delete_lbl) {
	ImageData.SampleBitMask.Valid = LBL_DELETE;
	rtn = 0;
    } else {

	// Format is:  2#11111111#

	char value[100];
	strcpy(value, "2#");

	// Start with the 0's
	for (i=0; i < (totalbits - setbits); i++)
	    strcat(value, "0");

	// Now the 1's
	for (i=0; i < setbits; i++)
	    strcat(value, "1");

	strcat(value, "#");

	strcpy(ImageData.SampleBitMask.Value, value);
	ImageData.SampleBitMask.Valid = 1;
	rtn = setbits;
    }

    // write out the label
    status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
    }

    return rtn;
}

////////////////////////////////////////////////////////////////////////
// Set the derived image type.  NULL means set to "IMAGE".
////////////////////////////////////////////////////////////////////////

void PigLabelModel::setDerivedImageType(const char *type)
{
    LblDerivedImage_typ  DerivedImage;

    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    if (type)
	strcpy(DerivedImage.DerivedImageType.Value, type);
    else
	strcpy(DerivedImage.DerivedImageType.Value, "IMAGE");

    DerivedImage.DerivedImageType.Valid = 1;

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }
}

/////////////////////////////////////////////////////////////////////
// (Re-)sets the geometry if the zoom or subframe change.  This
// updates the camera model as well as the subframe/downsample params
// in the label, so future kinematics pointing will get the model right.
// Note: sl/ss are applied *before* the zoom, and are 0-based.  Both
// the zoom and subframe are deltas applied to the current image
// (specified in file_model); if the image is already zoomed or subframed,
// these params add to it in the *output* label.
//
// Returns TRUE on success.
/////////////////////////////////////////////////////////////////////

int PigLabelModel::setGeometry(
		PigFileModel *file_model, PigCameraModel *cmod,
		int zoom_line, int zoom_samp,
                int sl, int ss)
{
    int status;
    int rtn_status = TRUE;

    // Get current zoom/subframe.

    int cur_zoom_l = (int)file_model->getDownsampleYFactor(1.0);
    if (cur_zoom_l == 0)	// whoops!
	cur_zoom_l = 1;
    int cur_zoom_s = (int)file_model->getDownsampleXFactor(1.0);
    if (cur_zoom_s == 0)	// whoops!
	cur_zoom_s = 1;
    int cur_sl = file_model->getFirstLine(1) - 1;	// make 0-based
    if (cur_sl == -1)		// whoops!
	cur_sl = 1;
    int cur_ss = file_model->getFirstLineSample(1) - 1;	// make 0-based
    if (cur_ss == -1)		// whoops!
	cur_ss = 1;

    int new_sl = cur_sl;
    int new_ss = cur_ss;
    int new_zoom_l = cur_zoom_l;
    int new_zoom_s = cur_zoom_s;
    int update = FALSE;

    // Subframe start in the label applies to the original, full-size image
    // before any zooming was done.  The parameters are a subframe applied
    // to the input image.  So if the input image was already zoomed, we must
    // adjust the subframes for that zoom, so they apply again to the original
    // image.

    if (sl != 0) {		// subframe different than before
	update = TRUE;
	new_sl = cur_sl + (sl*cur_zoom_l);
    }

    if (ss != 0) {		// subframe different than before
	update = TRUE;
	new_ss = cur_ss + (ss*cur_zoom_s);
    }

    if (zoom_line != 1) {
	update = TRUE;
	new_zoom_l = cur_zoom_l * zoom_line;
    }
    if (zoom_samp != 1) {
	update = TRUE;
	new_zoom_s = cur_zoom_s * zoom_samp;
    }

    if (!update)
	return TRUE;			// nothing to change!

    // Update the camera model.  This could get messy if the existing model
    // was already zoomed.  Using shiftCamera in this case on the existing
    // model with the given subframe params should be equivalent to recomputing
    // from scratch (the true original model) with the combined subframe start
    // computed above.  The latter is what would happen on a kinematics rerun.
    // Because they are equivalent (within roundoff error), we can get by with
    // using only the current camera model and don't have to re-derive from
    // scratch.  In other words: use the delta shift in sl/ss rather than
    // the absolute shift new_sl/new_ss computed above.

    if (new_sl != cur_sl || new_ss != cur_ss) {
	cmod->shiftCamera(ss, sl);
    }

    if (new_zoom_l != cur_zoom_l || new_zoom_s != cur_zoom_s) {
	cmod->scaleCamera(1.0/zoom_samp, 1.0/zoom_line);
    }

    // Write the updated camera model.

    status = writeCM(cmod, cmod->getCoordSystem());
    if (status)
	rtn_status = FALSE;		// return error, but keep trying

    // Write the updated downsample/subframe params

    LblInstrumentState_typ InstrumentState;
    LblImageData_typ ImageData;

    memset(&InstrumentState, 0, sizeof(LblInstrumentState_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    int write_inst = FALSE;
    int write_img = FALSE;

    if (new_sl != cur_sl) {
	ImageData.FirstLine.Value = new_sl + 1;	// 1-based
	ImageData.FirstLine.Valid = 1;
	write_img = TRUE;
    }

    if (new_ss != cur_ss) {
	ImageData.FirstLineSample.Value = new_ss + 1;	// 1-based
	ImageData.FirstLineSample.Valid = 1;
	write_img = TRUE;
    }

    if (new_zoom_l != cur_zoom_l) {
	InstrumentState.PixelAveragingHeight.Value = new_zoom_l;
	InstrumentState.PixelAveragingHeight.Valid = 1;
	write_inst = TRUE;
    }

    if (new_zoom_s != cur_zoom_s) {
	InstrumentState.PixelAveragingWidth.Value = new_zoom_s;
	InstrumentState.PixelAveragingWidth.Valid = 1;
	write_inst = TRUE;
    }

    if (write_img) {
	status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
	if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
            rtn_status = FALSE;			// error writing
	}
    }
    if (write_inst) {
	LblSetInstrumentState("INSTRUMENT_STATE_PARMS");
	status=LblInstrumentStateApi(_unit,LBL_AUGMENT,&InstrumentState,1);
	if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "InstrumentState", PigMsgError);
            rtn_status = FALSE;			// error writing
	}
    }

    return rtn_status;

}

/////////////////////////////////////////////////////////////////////
// Sets the geometry of an image (tile) larger to match the desired 
// output geometry of a particular image. This function updates the
// subframe/downsample parameters in the label to match the camera
// model of the reconstructed, tiled image.
// Note: sl/ss are full-resolution coordinates, with no downsampling.
// If the subframe or zoom changed, they are updated in the output label.
//
// Returns TRUE on success.
/////////////////////////////////////////////////////////////////////

int PigLabelModel::setGeometryLarger(PigFileModel *model_in,
	        PigCameraModel *cmod_in, int zoom_out_samp,
                int zoom_out_line, int slo, int sso)
{
    bool rtn_status = true;
    int status;
    char msg[255];

    int zoom_s = model_in->getDownsampleXFactor(1);
    int zoom_l = model_in->getDownsampleYFactor(1);
    int fs = model_in->getFirstLineSample(1) - 1;
    int fl = model_in->getFirstLine(1) - 1;
    int shift_s = sso - fs;
    int shift_l = slo - fl;

    // First, zoom the model to full size

    cmod_in->scaleCamera(double(zoom_s), double(zoom_l));

    // Now apply the shift, in full-size pixels

    if (shift_s != 0 || shift_l != 0) {
        cmod_in->shiftCamera(shift_s, shift_l);
    }

    // Now zoom the model down to the desired size

    cmod_in->scaleCamera(1.0 / zoom_out_samp, 1.0 / zoom_out_line);

    // Write the updated camera model.
    status = writeCM(cmod_in, cmod_in->getCoordSystem());
    if (status)
        rtn_status = false;

    // Write the updated downsample/subframe params
    LblInstrumentState_typ InstrumentState;
    LblImageData_typ ImageData;

    memset(&InstrumentState, 0, sizeof(LblInstrumentState_typ));
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    bool write_inst = false;
    bool write_img = false;

    if (fl != slo) {
        ImageData.FirstLine.Value = slo + 1;
        ImageData.FirstLine.Valid = 1;
        write_img = true;
    }

    if (fs != sso) {
        ImageData.FirstLineSample.Value = sso + 1;
        ImageData.FirstLineSample.Valid = 1;
        write_img = true;
    }

    if (zoom_out_line != zoom_l) {
        InstrumentState.PixelAveragingHeight.Value = zoom_out_line;
        InstrumentState.PixelAveragingHeight.Valid = 1;
        write_inst = true;
    }

    if (zoom_out_samp != zoom_s) {
        InstrumentState.PixelAveragingWidth.Value = zoom_out_samp;
        InstrumentState.PixelAveragingWidth.Valid = 1;
        write_inst = true;
    }

    if (write_img) {
        status = LblImageData(_unit, LBL_AUGMENT, &ImageData, 1);
        if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "ImageData", PigMsgError);
            rtn_status = FALSE;			// error writing
	}
    }

    if (write_inst) {
        LblSetInstrumentState("INSTRUMENT_STATE_PARMS");
        status = LblInstrumentStateApi(_unit, LBL_AUGMENT, &InstrumentState, 1);
        if (RTN_FAILURE(status)) {
            printMsg((char *)LblErrorMessage(), "InstrumentState", PigMsgError);
            rtn_status = FALSE;			// error writing
	}
    }

    return rtn_status;
}


/////////////////////////////////////////////////////////////////////
// Sets the labels for deBayering.  Since there really should be only
// one file model, we assume the first is the appropriate one to use
// for setGeometry().  "mode" is used for the deprecated BAYER_MODE
// label; "mode_new" is used for the new BAYER_METHOD label.
// Returns TRUE on success.
/////////////////////////////////////////////////////////////////////

int PigLabelModel::setBayer(PigFileModel *file_models[], int nids,
		char *mode, char *mode_new,
		PigCameraModel *cmod, int zoom_l, int zoom_s,
                int sl, int ss)
{
    // First check if there is any good (successfully opened) file.

    PigFileModel *a_good_file = NULL;
    for (int i = 0; (i < nids) && !a_good_file; i++)
	a_good_file = file_models[i];
    if (a_good_file == NULL) {
        printError("Could not find a good file to read!");
        return FALSE;
    }

    int rtn_status = TRUE;
    int status;

    writeProductIds(file_models, nids);
    setIdentification(NULL);

    rtn_status = setGeometry(a_good_file, cmod, zoom_l, zoom_s, sl, ss);

    // set the Mode string.  First check to see if the old BAYER_MODE exists.
    // If so, set it to the old mode.  If not, use the new labels.

    int old_mode = FALSE;

    LblInstrumentState_typ InstrumentState;
    memset(&InstrumentState, 0, sizeof(LblInstrumentState_typ));

    LblSetInstrumentState("INSTRUMENT_STATE_PARMS");
    status=LblInstrumentStateApi(_unit,LBL_READ,&InstrumentState,1);
    if (!RTN_FAILURE(status)) {		// it read okay, check for BayerMode
	if (InstrumentState.BayerMode.Valid)
	    old_mode = TRUE;		// got the old mode
    }

    memset(&InstrumentState, 0, sizeof(LblInstrumentState_typ));

    if (old_mode && mode != NULL) {
	strcpy(InstrumentState.BayerMode.Value, mode);
	InstrumentState.BayerMode.Valid = 1;
    }
    if (!old_mode && mode_new != NULL) {
	strcpy(InstrumentState.BayerMethod.Value, mode_new);
	InstrumentState.BayerMethod.Valid = 1;

	strcpy(InstrumentState.CfaVenue.Value, "GROUND");
	InstrumentState.CfaVenue.Valid = 1;
    }

    LblSetInstrumentState("INSTRUMENT_STATE_PARMS");
    status=LblInstrumentStateApi(_unit,LBL_AUGMENT,&InstrumentState,1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "InstrumentState", PigMsgError);
	rtn_status = FALSE;			// error writing
    }

    return rtn_status;
}

/////////////////////////////////////////////////////////////////////
// Sets the labels for color processing.
/////////////////////////////////////////////////////////////////////
int PigLabelModel::setColor(const char *color_space, char *illuminant, int bits,
                            PigFileModel *file_models[], 
                            PigColorModel *color_model, int is_float)
{
    char modified_color_space[256];
    LblDerivedImage_typ DerivedImage;
    memset(&DerivedImage, 0, sizeof(LblDerivedImage_typ));

    // set identification-related labels
    setIdentification(NULL);

    // set SAMPLE_BIT_MASK
    setBitMask(bits);

    // set INPUT_PRODUCT_ID
    writeProductIds(file_models, 1);

    // set COLOR_DN_SCALING_FACTOR and COLOR_DN_SCALING_METHOD
    if (color_space) {
        if (strcasecmp(color_space, "XYZ") == 0 ||
            strcasecmp(color_space, "xyY") == 0 ||
            strcasecmp(color_space, "wRGB") == 0) {
            DerivedImage.ColorDnScalingFactor.Value =
                                    color_model->getColorDnScalingFactor();
            DerivedImage.ColorDnScalingFactor.Valid = 1;
            strcpy(DerivedImage.ColorDnScalingMethod.Value,
                                    color_model->getColorDnScalingMethod());
            DerivedImage.ColorDnScalingMethod.Valid = 1;
        }
    }

    // set COLOR_SPACE.
    if (color_space) {
        // color space string is case insensitive, whereas the values for the 
        // label are case sensitive. We do a mapping here to convert case 
        // insensitive strings to case sensitive strings. In addition,
        // we want to convert the string XYZ to CIE_XYZ, and xyY to CIE_xyY. 
        if (strcasecmp(color_space, "iRGB") == 0)
            strcpy(modified_color_space, "iRGB");
        if (strcasecmp(color_space, "XYZ") == 0)
            strcpy(modified_color_space, "CIE_XYZ");
        if (strcasecmp(color_space, "wRGB") == 0) 
            strcpy(modified_color_space, "wRGB");
        if (strcasecmp(color_space, "xyY") == 0)
            strcpy(modified_color_space, "CIE_xyY");
        if (strcasecmp(color_space, "sRGB") == 0)
            strcpy(modified_color_space, "sRGB");
        if (strcasecmp(color_space, "pRGB") == 0)
            strcpy(modified_color_space, "pRGB");

        strcpy(DerivedImage.ColorSpace.Value, modified_color_space);
        DerivedImage.ColorSpace.Valid = 1;
    }

    // set ILLUMINANT
    if (illuminant) {
        strcpy(DerivedImage.Illuminant.Value, illuminant);
    } else {
        strcpy(DerivedImage.Illuminant.Value, "NONE"); 
    }
    DerivedImage.Illuminant.Valid = 1;

    // Update RADIANCE_OFFSET, RADIANCE_SCALING_FACTOR, 
    // RADIANCE_OFFSET__UNIT, and RADIANCE_SCALING_FACTOR__UNIT.
    if (is_float) {
        DerivedImage.RadianceOffset.Valid = LBL_DELETE;
        DerivedImage.RadianceOffsetUnit.Valid = LBL_DELETE;
        DerivedImage.RadianceScaleFactor.Valid = LBL_DELETE;
        DerivedImage.RadianceScaleFactorUnit.Valid = LBL_DELETE;
    } else {
        DerivedImage.RadianceOffset.Value = color_model->getDnScalingOffset();
        DerivedImage.RadianceOffset.Valid = 1;
        DerivedImage.RadianceScaleFactor.Value = color_model->getDnScalingFactor();
        DerivedImage.RadianceScaleFactor.Valid = 1;
    }

    // write out Derived Image Property Group into the label
    LblSetDerivedImage("DERIVED_IMAGE_PARMS");
    int status = LblDerivedImageApi(_unit, LBL_AUGMENT, &DerivedImage, 1);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "DerivedImage", PigMsgError);
    }

    return TRUE;
}
