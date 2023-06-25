// This classe defines the "objective" function for a Bundle Adjustment. 
// It stores and computes the residual between observed pixel and reprojected 
// pixel. For each observation (i.e., each pixel) an instance is created and 
// passed to //the ceres "Problem" object.
class traditionalReprojectionError
{ 
   private:
     double observed_line, observed_samp;
     int nbParams;
     PigCameraModel * camera;
     mutable PigCoordSystem * site;
     mutable PigPointingModel * pointing;
     mutable PigPoint groundPoint;
     PigCoordSystem * cs;

     traditionalReprojectionError(double observed_samp_in, 
                                   double observed_line_in,
                                   PigPointingModel * pointing_in,
                                   PigCameraModel * camera_in,
                                   PigCoordSystem * site_in,
                                   char * sid,
                                   PigCoordSystem * cs_in);

   public:

     // Overloading of the () operator that will allow ceres to either compute 
     // the residual or the derivatives for a given set of parameters
     bool operator()( const double * const groundPointParams,
                      const double * const pointingParams,
                      const double * const sitePointingParams,
                      const double * const sitePointingParams2,
                      double * residual) const; 


     // Factory to hide the construction of the CostFunction object from the 
     // client code.
     static ceres::CostFunction* Create( const double observed_x_in,
                                         const double observed_y_in,
                                         PigPointingModel * pointing_in,
				         const int numPointingParam_in,
                                         PigCameraModel * camera_in,
                                         PigCoordSystem * site_in,
				         char * sid,
				         PigCoordSystem * cs_in);
};



