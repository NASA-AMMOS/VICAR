////////////////////////////////////////////////////////////////////////
// PigLabelModel
//
// Base class for Label models.  Responsible for managing label information 
// for the output file.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGLABELMODEL_H
#define PIGLABELMODEL_H

#include <string.h>
#include "PigModelBase.h"
#include "PigCoordSystem.h"
#include "lbl_identification.h"
#include "lbl_camera_model.h"
#include "lbl_surface_model.h"
#include "lbl_surface_projection.h"
#include "lbl_image_map_projection.h"
#include "PigFileModel.h"	// for MAX_CS_OBJ

// We should use this include... but it does nasty things like #define I
// #include "defines.h"		// for MAX_LABEL_KEY_SIZE
#define MAX_LABEL_KEY_SIZE      32
#define PIG_MAX_INPUTS  2000

// Note that this parameter also defined in PigAdjustable.  If you change
// the value here, update other place as well. 


class PigCSDefinition;
class RadiometryModel;
class PigCameraModel;
class PigPointingModel;
class PigSurfaceModel;
class PigBrtCorrModel;
class PigColorModel;

class PigLabelModel : public PigModelBase {

 protected:
    PigMission *_mission;
    int _unit;
    int _num_all_coord_sys;
    PigCSDefinition *_all_coord_sys[MAX_CS_OBJ];
    
    //////////////////////////////////////////////////////////////////////////
    // Reads all Coordinate Systems from the label and puts them
    // into _all_coord_sys array
    /////////////////////////////////////////////////////////////////////////
    virtual void getCoordSystems();

    /////////////////////////////////////////////////////////////////////////
    // Check if given Coordinate System is already in the label, i.e. found
    // in _all_coord_sys array.
    /////////////////////////////////////////////////////////////////////////
    virtual int findCS(PigCSReference *cs);

    //////////////////////////////////////////////////////////////////////////
    // check if given string already in the array
    /////////////////////////////////////////////////////////////////////////
    virtual int findValue(char* key[], int& count, char* new_str);

    //////////////////////////////////////////////////////////////////////////
    // LblCoordinate's specific label write function.  Used by public function
    // writeCS(cs)
    /////////////////////////////////////////////////////////////////////////
    virtual void setupWriteCS(PigCoordSystem *cs, LblCoordinate_typ *aux);

    /////////////////////////////////////////////////////////////////////////
    // Writes the standard multi-input properties to the label.  Currently
    // these properties include IDENTIFICATION, SURFACE_MODEL_PARMS, 
    // SURFACE_PROJECTION_PARMS as well as radiometry labels in 
    // DERIVED_IMAGE_PARMS.  Certain items (see the code) are added to these 
    // structures.  If you want other items in there, you may
    // pass in partially-filled structures with additional items.  Fields with
    // the Valid flag set will *not* be overridden.  If these parameters 
    // are NULL, empty structures will be created.
    // This routine requires the following parameters be in the PDF:
    //
    // PARM DATA_SET_NAME TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM DATA_SET_ID TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM RELEASE_ID TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM PRODUCT_ID TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM PRODUCER_ID TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM PRODUCER_INST TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM TARGET_NAME TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM TARGET_TYPE TYPE=STRING COUNT=(0:1) DEFAULT=--
    //
    // The rad_models parameter may be NULL if no rad models are present.
    // If the rad models do not all agree, the radiometric scaling factors
    // will not be written (i.e. it is an uncalibrated image).
    /////////////////////////////////////////////////////////////////////////
    virtual void setMosaic(PigFileModel *file_models[],
			   RadiometryModel *rad_models[],
			   PigBrtCorrModel *brt_models[],
                           double radiance_factor[],
                           double radiance_offset[],
			   int nids,
			   PigSurfaceModel *surface,
			   PigCoordSystem *cs,
			   LblSurfaceProjection_typ *surfProj);

    ///////////////////////////////////////////////////////////////////////////
    // Write flat field and dark current filenames.  Only write unique values.
    ///////////////////////////////////////////////////////////////////////////
    virtual void writeRadFlats(RadiometryModel *rad_models[], int num_rads);
    virtual void writeRadDarks(RadiometryModel *rad_models[], int num_rads);

 public:

    // Constructor should only be called via PigMission's create functions!
    PigLabelModel(int unit, const char *mission);
    PigLabelModel(int unit, PigMission *mission);

    virtual ~PigLabelModel();
    
    ////////////////////////////////////////////////////////////////////
    // This factory method creates and returns an instance of the
    // proper mission-specific subclass for the given file.  Missions
    // using the multimission label API may not need a mission-specific
    // subclass.
    
    static PigLabelModel *create(int unit, const char* mission);
    static PigLabelModel *create(int unit, PigMission *mission);
    
    ////////////////////////////////////////////////////////////////////
    // Label management functions.
    ////////////////////////////////////////////////////////////////////
    
    // Return the VICAR unit number, filename, or mission name.
    int getUnit() { return _unit; }
    const char *getMissionName() { return _mission->getMissionName(); }
    
    // Writes out fields in Identity Label Group
    // which are applicable to all mars programs
    virtual void setIdentification(LblIdentification_typ *ident);

    /////////////////////////////////////////////////////////////////////////
    // Writes the standard multi-input properties to the label.  Currently
    // these properties include IDENTIFICATION, SURFACE_MODEL_PARMS, 
    // SURFACE_PROJECTION_PARMS as well as radiometry labels in 
    // DERIVED_IMAGE_PARMS.  Certain items (see the code) are added to these 
    // structures.  If you want other items in there, you may
    // pass in partially-filled structures with additional items.  Fields with
    // the Valid flag set will *not* be overridden.  If these parameters 
    // are NULL, empty structures will be created.
    // This routine requires the following parameters be in the PDF:
    //
    // PARM DATA_SET_NAME TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM DATA_SET_ID TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM RELEASE_ID TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM PRODUCT_ID TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM PRODUCER_ID TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM PRODUCER_INST TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM TARGET_NAME TYPE=STRING COUNT=(0:1) DEFAULT=--
    // PARM TARGET_TYPE TYPE=STRING COUNT=(0:1) DEFAULT=--
    //
    // The rad_models parameter may be NULL if no rad models are present.
    // If the rad models do not all agree, the radiometric scaling factors
    // will not be written (i.e. it is an uncalibrated image).
    /////////////////////////////////////////////////////////////////////////
    virtual void setRemosaic(PigFileModel *file_models[],
                             PigFileModel *idx_fm,
                             PigFileModel *icm_fm,
			     RadiometryModel *rad_models[],
			     PigBrtCorrModel *brt_models[],
 			     int nids,
			     PigCoordSystem *cs);

    ////////////////////////////////////////////////////////////////////////
    // Write marsrad's output labels to the file.
    // Note: this no longer sets the bit mask directly.  The app needs to 
    // call setBitMask() itself if it wants the bit mask to be set.
    ////////////////////////////////////////////////////////////////////////
    virtual void setRadiometric(PigFileModel *file_models[], int nids, 
				RadiometryModel *rad_models[],
				int num_rads);
    
    ///////////////////////////////////////////////////////////////////////////
    // Write instrument placement labels.  
    ///////////////////////////////////////////////////////////////////////////
    virtual void setInstPlacement(PigFileModel *file_models[], int nids, 
                                  const char *imageType, const char *instType, 
                                  PigCoordSystem *cs);

    ///////////////////////////////////////////////////////////////////////////
    // Set the target instrument. Skip it if NULL. 
    ///////////////////////////////////////////////////////////////////////////
    virtual void setTargetInstrument(const char *type);

    ////////////////////////////////////////////////////////////////////////
    // Write brightness correction and bias labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setBrtCorr(PigBrtCorrModel *brt_models[],
		double radiance_factor[], double radiance_offset[],
		int nids);

    // These should only be called by the specific brt corr models...

    virtual int writeBrtCorrLinearLabel(int index, double mult, double add);

    ////////////////////////////////////////////////////////////////////////
    // Write marscahv's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setLinearized(PigFileModel *file_models[], int nids, 
                               char *stereo_partner_type,
                               char *cahv_fov_type,
                               PigFileModel *partner_fm); 

    ////////////////////////////////////////////////////////////////////////
    // Write marsunlinearize's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setUnlinearized(PigFileModel *file_models[], int nids); 

    ////////////////////////////////////////////////////////////////////////
    // Write disparity(marsjplstereo, marscorr, etc.) output labels.
    ////////////////////////////////////////////////////////////////////////
    virtual void setDisparity(PigFileModel *file_models[],
                              PigFileModel *corr_parnter_fm,
                              int nids, const char *imageType);

    ////////////////////////////////////////////////////////////////////////
    // Write extra (optional) information for disparity.
    ////////////////////////////////////////////////////////////////////////
    virtual void setDisparityExtra(int count, 
                                   double scale, 
                                   double overlap,
                                   int pyramid_level);

    ////////////////////////////////////////////////////////////////////////
    // Write mask(marsdispcmp, marsfilter, etc.) output labels.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMask(PigFileModel *file_models[], int nids,
			const char *imageType,
			const char *filename1, const char *filename2,
			int use_horizon, double horizon);

    ////////////////////////////////////////////////////////////////////////
    // Write masked product labels.  Since marsmask typically uses the label
    // from the mask image, this transfers the DERIVED_IMAGE_TYPE from the
    // first input (which should be the actual image) to the output.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMasked(PigFileModel *file_models[], int nids);

    ////////////////////////////////////////////////////////////////////////
    // Write marsxyz's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setXYZ(PigFileModel *file_models[], int nids,
    			PigCoordSystem *cs, char *imageType, double baseline,
			const char *stereo_partner_id);

    ////////////////////////////////////////////////////////////////////////
    // Write marsrfilt's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setRFilt(PigFileModel *file_models[], int nids,
    			PigCoordSystem *cs, double average_window,
			double min_window, double max_window);

    ////////////////////////////////////////////////////////////////////////
    // Write patch's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setPatch(PigFileModel *file_models[], int nids,
			  PigCoordSystem *cs);

    ////////////////////////////////////////////////////////////////////////
    // Write marsuvw's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setUVW(PigFileModel *file_models[], int nids,
                        PigCoordSystem *cs, char *imageType);

    ////////////////////////////////////////////////////////////////////////
    // Write marserror's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setError(PigFileModel *file_models[], int nids,
			  PigCoordSystem *cs,
                          double error_parms[], int error_parms_cnt,
                          char *imageType);

    ////////////////////////////////////////////////////////////////////////
    // Write marsrange's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setRange(PigFileModel *file_models[], int nids,
                          const PigPoint &origin, PigCoordSystem *cs);

    ////////////////////////////////////////////////////////////////////////
    // Write marsslope's output labels to the file.
    ////////////////////////////////////////////////////////////////////////
    virtual void setSlope(PigFileModel *file_models[], int nids,
                          PigCoordSystem *cs,
			  const PigPoint *origin, float sa,
			  const char *slopeFunctionType);

    ////////////////////////////////////////////////////////////////////////
    // Write reachability product labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setReach(PigFileModel *file_models[], int nids);

    ////////////////////////////////////////////////////////////////////////
    // Write goodness product labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setGReach(PigFileModel *file_models[], int nids,
                          int *bands, int nbands, bool include_configs);

    ////////////////////////////////////////////////////////////////////////
    // Write preload product labels to the output file.  The third argument
    // indicates which variety of preload it is in a mission-specific manner.
    ////////////////////////////////////////////////////////////////////////
    virtual int setPreload(PigFileModel *file_models[], int nids,
			   int preload_type);

    ////////////////////////////////////////////////////////////////////////
    // Write marsrough's labels to the output file.  Flag is a mission-specific
    // value (for MSL, indicates drill or DRT).  Set to 0 if not needed.
    ////////////////////////////////////////////////////////////////////////
    virtual int setRough(PigFileModel *file_models[], int nids, 
                         PigCoordSystem *cs, float invalid_constant, int flag);

    ////////////////////////////////////////////////////////////////////////
    // Write marsdepth's labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setDepth(PigFileModel *file_models[], int nids, 
                         PigCoordSystem *cs, float invalid_constant);

    ////////////////////////////////////////////////////////////////////////
    // Write marsmap's output labels to the file.  Errors are reported to the
    // screen but are otherwise ignored.  This method is specific to 
    // the cylindrical Projection mode.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMapCyl(PigFileModel *file_models[],
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
			   PigCoordSystem *cs); // projection coord system

    ////////////////////////////////////////////////////////////////////////
    // Write marsmap's output labels to the file.  Errors are reported to the
    // screen but are otherwise ignored.  This method is specific to 
    // the Polar Projection mode.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMapPolar(PigFileModel *file_models[],
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
			     PigCoordSystem *cs); // projection coord system

    ////////////////////////////////////////////////////////////////////////
    // Write marsmap's output labels to the file.  Errors are reported to the
    // screen but are otherwise ignored.  This method is specific to 
    // the Vertical Projection mode.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMapVert(PigFileModel *file_models[],
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
			    PigCoordSystem *cs); // projection coord system

    ////////////////////////////////////////////////////////////////////////
    // Write marsmap's output labels to the file.  Errors are reported to the
    // screen but are otherwise ignored.  This method is specific to 
    // the sinusoidal Projection mode.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMapSin(PigFileModel *file_models[],
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
			   PigCoordSystem *cs); // projection coord system

    ////////////////////////////////////////////////////////////////////////
    // Write marsmos's output labels to the file.  Errors are reported to the
    // screen but are otherwise ignored.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMos(PigFileModel *file_models[],
			RadiometryModel *rad_models[],
			PigBrtCorrModel *brt_models[],
                        double radiance_factor[],
                        double radiance_offset[],
			int nids,
			PigSurfaceModel *surface_model,
			PigCameraModel *camera_out,
			double x_offset,
			double y_offset,
			PigCoordSystem *fixed_cs,
			PigVector output_direction);

    ////////////////////////////////////////////////////////////////////////
    // Write marsmcauley's output labels to the file.  Errors are reported 
    // to the screen but are otherwise ignored.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMcauley(PigFileModel *file_models[],
			    RadiometryModel *rad_models[],
			    PigBrtCorrModel *brt_models[],
                            double radiance_factor[],
                            double radiance_offset[],
			    int nids,
			    PigSurfaceModel *surface_model,
			    PigCameraModel *camera_out_copy,
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
			    PigCoordSystem *site_cs);

    ////////////////////////////////////////////////////////////////////////
    // Write marsortho output labels to the file.  Errors are reported to the
    // screen but are otherwise ignored.
    ////////////////////////////////////////////////////////////////////////
    virtual void setMosOrtho(PigFileModel *file_models[],
			    RadiometryModel *rad_models[],
			    PigBrtCorrModel *brt_models[],
			    PigCoordSystem *cs,    // projection coord system
                            PigCoordSystem *z_cs,  // derived image coord system
			    PigCoordSystem *write_cs, // extra cs to write
                            double radiance_factor[],
                            double radiance_offset[],
			    double ortho_scale,
			    double maxx, double maxy, 
                            double missing_constant,
			    int nids, int nlo, int nso, int bands, 
                            int is_dem, int is_zup);

    ///////////////////////////////////////////////////////////////////////
    // Writes out given PigCoordSystem to the _unit file
    ///////////////////////////////////////////////////////////////////////
    virtual int writeCS(PigCoordSystem *cs);

    //////////////////////////////////////////////////////////////////////
    // check if this coordinate system already in the label, if not 
    // write it in the label.  Then check if coordinate system's 
    // reference coordinate system is in the label, if not write it 
    // in the label.  Follow this chain until MAJOR_SITE or FIXED_SITE
    // is reached.
    ///////////////////////////////////////////////////////////////////// 
    virtual void writeCSandItsReferences(PigCoordSystem *cs);

    /////////////////////////////////////////////////////////////////////
    // Writes out given PigCameraModel to the _unit file
    /////////////////////////////////////////////////////////////////////
    virtual int writeCM(PigCameraModel *cm, PigCoordSystem *cs);

    /////////////////////////////////////////////////////////////////////
    // Writes out given PigPointingModel to the _unit file
    /////////////////////////////////////////////////////////////////////
    virtual int writePointingModel(PigPointingModel *pm);


    virtual void writeProductIds(PigFileModel *file_models[], int nids);
    virtual int writeSourceProductId(PigFileModel *file_models[], int nids);
    virtual int writeInputProductId(PigFileModel *file_models[], int nids);

    ///////////////////////////////////////////////////////////////////// 
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
    ///////////////////////////////////////////////////////////////////// 
    virtual int setInverted(PigFileModel *file_models[], int nids, 
                            int flag, int used_file, char *filename);

    ///////////////////////////////////////////////////////////////////// 
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
    ///////////////////////////////////////////////////////////////////// 
    virtual int setBitMask(int setbits);

    ///////////////////////////////////////////////////////////////////// 
    // Set the derived image type.  NULL means set to "IMAGE".
    /////////////////////////////////////////////////////////////////////
    virtual void setDerivedImageType(const char *type);

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
    virtual int setGeometry(PigFileModel *file_model, PigCameraModel *cmod,
		int zoom_line, int zoom_samp,
		int sl, int ss);

    /////////////////////////////////////////////////////////////////////
    // Sets the geometry of an image (tile) larger to match the desired 
    // output geometry of a particular image. This function updates the
    // subframe/downsample parameters in the label to match the camera
    // model of the reconstructed, tiled image.
    // Note: Unlike setGeometry, sl/ss are applied after zooming. Both the
    // zoom and subframe are deltas applied to the current image. If the
    // image is already zoomed or subframed, these params add to it in the
    // *output* label.
    //
    // Returns TRUE on success.
    /////////////////////////////////////////////////////////////////////
    virtual int setGeometryLarger(PigFileModel *model_in,
	        PigCameraModel *cmod_in, int zoom_out_samp,
                int zoom_out_line, int slo, int sso);
				  

    ///////////////////////////////////////////////////////////////////// 
    // Sets the labels for deBayering.  Since there really should be only
    // one file model, we assume the first is the appropriate one to use
    // for setGeometry().  "Mode" is used for the deprecated BAYER_MODE
    // label; "mode_new" is used for the new BAYER_METHOD label.
    ///////////////////////////////////////////////////////////////////// 
    virtual int setBayer(PigFileModel *file_models[], int nids,
		char *mode, char *mode_new,
		PigCameraModel *cmod, int zoom_l, int zoom_s,
		int sl, int ss);
    /////////////////////////////////////////////////////////////////////
    // Sets the labels for color processing. 
    /////////////////////////////////////////////////////////////////////
    virtual int setColor(const char *color_space, char *illuminant, int bits, 
                         PigFileModel *file_models[], 
                         PigColorModel *color_model, int is_float);

    virtual const char *const getModelName() { return "LabelModel"; }

};

#endif
