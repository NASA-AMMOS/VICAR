#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"

#include "ceres/ceres.h"
#include "tradiprojection.h"

// This class defines the "objective" function. It stores and computes
// the residual between observed pixel and reprojected pixel. For each 
// observation (i.e., each pixel) an instance is created and passed to
// the ceres "Problem" object. 
traditionalReprojectionError :: traditionalReprojectionError(
                               double observed_samp_in, 
                               double observed_line_in,
                               PigPointingModel * pointing_in,
                               PigCameraModel * camera_in,
                               PigCoordSystem * site_in,
                               char * sid,
                               PigCoordSystem * cs_in){

     
    // Initialize the observed pixel coordinates
    observed_samp = observed_samp_in;
    observed_line = observed_line_in;

    // Initializing the camera and pointing objects
    pointing = pointing_in;
    camera = camera_in;
    site = site_in;

    // Get and save the number of pointing parameters. This information
    // is useful during the optimization to know how many item must 
    // be read from the "larger" parameter array
    nbParams = pointing->getPointingParamCount();

    // Save the coordinate system locally. This represents the 
    // coordinates system of the groud point. This cs has to be 
    // the same through the entire data set because different images,
    // different instruments, from different location, can point to the 
    // same ground location.
    // For performance, it might be better to set the CS of pointing and camera
    // directly to the common one to avoid CS conversion at each estimate. 
    // However, not sure if it's doable. For now ground point is converted 
    // internally (and for each ceres iteration) into camera CS. Ideally 
    // conversion should happen just once at the initialization of pig pointing
    // and camera object.
    cs = cs_in;

    // Initialize a ground point
    groundPoint = PigPoint();
  }


// Overloading of the () operator that will allow ceres to either compute the 
// residual or the derivatives for a given set of parameters
bool traditionalReprojectionError :: operator()(
                  const double * const groundPointParams,
                  const double * const pointingParams,
                  const double * const sitePointingParams,
                  const double * const sitePointingParams2,
                  double * residual) const {


    // Update ground Point with the proposed point coordinates
    groundPoint.setXYZ(groundPointParams);

    // Update Site oject with the proposed pointing parameters.
    // The following seems similar to:
    // site->setPointingParameters(sitePointingParams, 6);
    // as ceres does not seems to break the continuous pointing address
    // location by making separate copies of the parameter array. 
    // This could be checked by taking a look at ceres code.
    double p[6];
    for (int i=0; i<3;i++)
       p[i] = sitePointingParams[i];
    for (int i=0; i<3;i++)
       p[i+3] = sitePointingParams2[i];
    site->setPointingParameters(p, 6);

    // Update camera pointing with the proposed pointing parameters.
    pointing->setPointingParameters(pointingParams, nbParams);
    
    // Compute pixel location
    double computed_line, computed_samp;
    camera->XYZtoLS(groundPoint, FALSE, &computed_line, &computed_samp, cs);

    // Update residual
    residual[0] = observed_line - computed_line;
    residual[1] = observed_samp - computed_samp;
    
    return true;
  }



  // Factory to hide the construction of the CostFunction object from the client
  // code.
  // For now, we switch on the number of degree of freedom to instanciate the 
  // cost function (need a const expression at compile time, so can't use the 
  // numPointingParam variable in the cost instanciation). The number of 
  // possible switch is based on PIG_MAX_PARAMS (WARNING: if PIG_MAX_PARAMS
  // is increased, the number of switch must be increased accordingly).
  // The other option would be to use the DynamicNumericDiffCostFunction of 
  // ceres that allows undefined number and size of parameters block. However,
  // that requires ALL the parameters to be packed in one single large array, 
  // which is doable but does require a carefull handling of indices AND 
  // apparently imposes a processing overhead from ceres (not tested though)
  // 
  // There are 5 parameters blocks, whose size are defined by the numerals:
  // - The first number (2) always represent the output residual number. In our
  //   case, it is the sample and line residuals (hence 2).
  // - The second number (3) represents the X,Y,Z of the ground point
  // - The third number (switch case) represents the number of pointings 
  //   parameters, which depends on the instrument and user input.
  // - The forth number (3) represents the Sites ORIENTATION pointing 
  //   parameters. It is always 3 (roll, pitch, yaw).
  // - The fifth number (3) represents the Sites POSITION pointing 
  //   parameters. It is always 3 (X, Y, Z).
  //
  // Note: for switch case = 0 (i.e. 0 dof, such as haz cam) there is a hack: 
  // we set the camera pointing parameters to 1 as ceres does not support 0. 
  // Normally in such situation, we are not supposed to supply the parameter 
  // block as nothing gets optimized. This would require a specific branching 
  // during the build of the NLLS problem for 0dof image. This hack makes the 
  // code simpler. The downside is that ceres handles an extra parameter block 
  // for nothing as no pointing change is going to happen anyway. This does not
  // change the output as this parameter block does not have any contribution
  // the the function cost.
  ceres::CostFunction* traditionalReprojectionError :: Create(
                                     const double observed_x_in,
                                     const double observed_y_in,
                                     PigPointingModel * pointing_in,
				     const int numPointingParam_in,
                                     PigCameraModel * camera_in,
                                     PigCoordSystem * site_in,
				     char * sid,
				     PigCoordSystem * cs_in){ 
    
#define FACTORY(numParam)                                                        \
      return (new ceres::NumericDiffCostFunction                                 \
           <traditionalReprojectionError, ceres::RIDDERS, 2, 3, numParam, 3, 3>( \
                        new traditionalReprojectionError(observed_x_in,          \
                                                         observed_y_in,          \
                                                         pointing_in,            \
                                                         camera_in,              \
                                                         site_in,                \
                                                         sid,                    \
                                                         cs_in)))              



 
    switch (numPointingParam_in) {
       case 0: FACTORY(1);  //Not an error - see comment
       case 1: FACTORY(1);
       case 2: FACTORY(2);
       case 3: FACTORY(3);
       case 4: FACTORY(4);
       case 5: FACTORY(5);
       case 6: FACTORY(6);
       case 7: FACTORY(7);
       case 8: FACTORY(8);
       case 9: FACTORY(9);
       case 10: FACTORY(10);
       default: return NULL;
    }

  }

