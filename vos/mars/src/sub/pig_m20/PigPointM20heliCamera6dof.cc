////////////////////////////////////////////////////////////////////////
// PigPointM20heliCamera6dof
//
// Pointing model for Ingenuity helicopter cameras (nav, RTE).
// Ingenuity cameras system is conceptually considered as a arm camera
// (a virtual long arm from the take off location to the heli position/
// orientation)
//
////////////////////////////////////////////////////////////////////////

#include "PigM20.h"
#include "PigCameraModel.h"
#include "PigPointCamera6dof.h"
#include "PigPointM20heliCamera6dof.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointM20heliCamera6dof::~PigPointM20heliCamera6dof()
{
	// nothing to do...
}
////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointM20heliCamera6dof::pointCamera(PigFileModel *file)
{

    if (pointCameraViaLabel(file))
        return;

    PigFileModelM20 *fileM20 = (PigFileModelM20*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);


    // Get a handle on the file. Open if needed
    int unit;
    int needToClose = 0;
    if (fileM20->isFileOpen())
       unit = fileM20->getUnit();
    else {
       fileM20->openFile();
       unit = fileM20->getUnit();
       needToClose = 1;  //file was closed. Close it before exit
    }


    // Get the CS structure from the file labels
    LblCoordinate_typ * hcs = new LblCoordinate_typ;
    int status = LblCoordinateApi(unit, LBL_READ, hcs, 1, "HELI_M_COORDINATE_SYSTEM");

    // File not needed anymore. Close it if necessary
    if (needToClose)
       fileM20->closeFile();


    // Default Origin Offset Vector and Quaternions
    PigPoint location(0,0,0);
    PigQuaternion orientation(1,0,0,0);


    if (hcs == NULL) {
	printWarning("Warning: no HELI_M_COORDINATE_SYSTEM found");
    }
    else {

       if (!hcs->OriginOffsetVector.Valid)
          printWarning("Warning: no Origin Offset Vector found. Default to (0,0,0).");
       else
          location = PigPoint(hcs->OriginOffsetVector.Value[0],
                              hcs->OriginOffsetVector.Value[1],
                              hcs->OriginOffsetVector.Value[2]);

       if (!hcs->OriginRotationQuaternion.Valid)
          printWarning("Warning: no Origin Offset Quaternion found. Default to (1,0,0,0).");
       else orientation = PigQuaternion(hcs->OriginRotationQuaternion.Value[0],
                                        hcs->OriginRotationQuaternion.Value[1],
                                        hcs->OriginRotationQuaternion.Value[2],
                                        hcs->OriginRotationQuaternion.Value[3]);
    }


    orientation.getEulerAngles(_twist, _elevation, _azimuth);
    
    _azimuth = PigRad2Deg(_azimuth);
    _elevation = PigRad2Deg(_elevation);
    _twist = PigRad2Deg(_twist);
    
    pointCamera(_azimuth, _elevation, _twist,
                location.getX(), location.getY(), location.getZ(), 
	        _pointing_cs);
    
}



