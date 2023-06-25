
/**
********************************************************
    Rover Sequencing and Visualization Program (RSVP)

       Copyright 2001
       California Institute of Technology.
       ALL RIGHTS RESERVED.
       U.S. Government sponsorship acknowledged.
*********************************************************
  Class CameraModelGeoData
  Created by John Wright
  Created Tue Feb 18 16:49:57 2003
*********************************************************
  Modification History
  Date        Programmer       Change

*********************************************************
  Current Version (CVS): $Id:$
*********************************************************
*/

#ifndef _CameraModelGeoData_H_
#define _CameraModelGeoData_H_

#include "litwin/include/Camera_Model.h"
#include "image/geodata.h"
#include <math.h>

/// Implements a GeoData using a Camera Model object
/**
 CameraModelGeoData implements a GeoData using a Camera Model object.
 Essentially, this uses a projection library to convert the image X,Y
 values to Az,El which projects the image onto a sphere from the inside.
 This is equivalent to projecting it from the outside with other types
 of GeoData objects.  Initially, most of these will use the JPLCameraModel
 based on Todd Litwin's CAHVORE camera model.  However, other models are
 supported if they derive from CameraModel.
*/

class CameraModelGeoData:public GeoData {

    private:

    protected:

	CameraModel	*cameraModel;

    public:

	virtual int     data_class_type(void) { return(CAMERA_GEO_DATA); } // Camera Model Based Image georeferencing class type

	// coordinate space converters (virtuals)
	virtual double  lltox(double latitude, double longitude);
	virtual double  lltoy(double latitude, double longitude);

	virtual double  xytolat(double , double );
	virtual double  xytolong(double , double );

	// overriding these for more speed
	virtual void    lltoxy(double latitude, double longitude, double &x, double&y);
	virtual void    xytoll(double x, double y, double &latitude, double &longitude);

	// Specific methods for this class

	/// set_cameraModel sets the cameraModel field to the specified value.
	/**
	* set_cameraModel sets the cameraModel field to the specified value.
	*/
	void set_cameraModel(CameraModel * in_val_arg) {
		if(cameraModel) delete(cameraModel);
		cameraModel = in_val_arg;
	}
	///get_cameraModel returns the cameraModel field to the caller.
	/**
	* get_cameraModel returns the cameraModel field to the caller.
	*/
	CameraModel * get_cameraModel() {
		return(cameraModel);
	}

    // Constructors

	CameraModelGeoData();

    // Destructor

	~CameraModelGeoData();

    // Unit Self Test Method

	static int self_test(void);

};


#endif
