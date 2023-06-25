////////////////////////////////////////////////////////////////////////
// PigFileModelMpf
//
// MPF-specific File model.  Since MPF does not conform to the 
// multimission label API, this class translates the MPF labels into their 
// MM equivalents.  Although we could create the label structures as needed, 
// it is much easier to just do them all at once in the ctor.  And since the 
// Pig classes are not the mainline MPF processing routines, this is sufficient.
//
// Note that not all possible mappings are currently being transferred...
// just the ones that are useful to the current software.
////////////////////////////////////////////////////////////////////////

#include "PigFileModelMpf.h"

extern "C" {
    #include "mpf_gen_labels.h"
    #include "mpf_imp_labels.h"
    #include "mpf_rvr_labels.h"
}

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigFileModelMpf::PigFileModelMpf(const char *filename, 
				 int unit,
				 const char *mission)
	       : PigFileModel(filename, unit, mission)
{
    int status;

    // Create all the structures and initialize them

    int err = FALSE;

    _lblIdentification = new LblIdentification_typ;
    if (_lblIdentification)
	memset(_lblIdentification, 0, sizeof(LblIdentification_typ));
    else
	err = TRUE;

    _lblInstrumentState = new LblInstrumentState_typ;
    if (_lblInstrumentState)
	memset(_lblInstrumentState, 0, sizeof(LblInstrumentState_typ));
    else
	err = TRUE;

    _lblImageData = new LblImageData_typ;
    if (_lblImageData)
	memset(_lblImageData, 0, sizeof(LblImageData_typ));
    else
	err = TRUE;

    _lblCameraModel = new LblCameraModel_typ;
    if (_lblCameraModel)
	memset(_lblCameraModel, 0, sizeof(LblCameraModel_typ));
    else
	err = TRUE;

    _lblDerivedGeometry = new LblDerivedGeometry_typ;
    if (_lblDerivedGeometry)
	memset(_lblDerivedGeometry, 0, sizeof(LblDerivedGeometry_typ));
    else
	err = TRUE;

    if (err) {
	printError("Error allocating memory for MPF IMP FileModel");
	return;
    }

    // Now read MPF structures and copy info to MM structures
    status = genericLabelsInit();

    //Imp and Rover have different ObsProperty and CameraProperty
    if (strncasecmp(_lblIdentification->InstrumentId[0].Value, "IMP", 3) == 0) 
        status = impLabelsInit();
    else //must be the rover
        status = roverLabelsInit();

   // Re-set the offsets, now that things are done properly
   setupOffsets();
}

int PigFileModelMpf::genericLabelsInit() {

    int status;
    LblCntrl_typ Controls;
    Controls.ProceedOnError = 1;

    ///////////////////
    // MpfTelemProperty
    ///////////////////

    MpfTelemProperty_typ Telemetry;
    memset(&Telemetry, 0, sizeof(MpfTelemProperty_typ));
    status = MpfTelemProperty(_unit, LBL_READ, &Controls, &Telemetry);

    strcpy(_lblIdentification->InstrumentId[0].Value, Telemetry.InstrumentId);
    if (strlen(Telemetry.InstrumentId) != 0)
	_lblIdentification->InstrumentId[0].Valid = TRUE;

    strcpy(_lblIdentification->InstrumentName[0].Value, Telemetry.InstrumentName);
    if (strlen(Telemetry.InstrumentName) != 0)
	_lblIdentification->InstrumentName[0].Valid = TRUE;

    strcpy(_lblIdentification->InstrumentType[0].Value, "IMAGING_CAMERA");
    _lblIdentification->InstrumentType[0].Valid = TRUE;

    strcpy(_lblIdentification->MissionName[0].Value, Telemetry.MissionName);
    if (strlen(Telemetry.MissionName) != 0)
	_lblIdentification->MissionName[0].Valid = TRUE;

    strcpy(_lblIdentification->ProducerId.Value, Telemetry.ProducerId);
    if (strlen(Telemetry.ProducerId) != 0)
	_lblIdentification->ProducerId.Valid = TRUE;

    strcpy(_lblIdentification->ProductCreationTime.Value, Telemetry.ProductCreationTime);
    if (strlen(Telemetry.ProductCreationTime) != 0)
	_lblIdentification->ProductCreationTime.Valid = TRUE;

    strcpy(_lblIdentification->ProductId.Value, Telemetry.ProductId);
    if (strlen(Telemetry.ProductId) != 0)
	_lblIdentification->ProductId.Valid = TRUE;

    strcpy(_lblIdentification->InstrumentHostName[0].Value, Telemetry.SpacecraftName);
    if (strlen(Telemetry.SpacecraftName) != 0)
	_lblIdentification->InstrumentHostName[0].Valid = TRUE;

    return status;
}
int PigFileModelMpf::impLabelsInit() {

    int status;
    LblCntrl_typ Controls;
    Controls.ProceedOnError = 1;
    /////////////////
    // ImpObsProperty
    /////////////////
    ImpObsProperty_typ Observation;
    memset(&Observation, 0, sizeof(ImpObsProperty_typ));
    status = ImpObservationProperty(_unit, LBL_READ, &Controls, &Observation);

    strcpy(_lblIdentification->FrameId[0].Value, Observation.FrameId);
    if (strlen(Observation.FrameId) != 0)
        _lblIdentification->FrameId[0].Valid = TRUE;

    sprintf(_lblIdentification->ImageId.Value, "%d", Observation.ImageId);
    _lblIdentification->ImageId.Valid = TRUE;

    strcpy(_lblIdentification->ImageTime.Value, Observation.ImageTime);
    if (strlen(Observation.ImageTime) != 0)
        _lblIdentification->ImageTime.Valid = TRUE;

    strcpy(_lblIdentification->ObservationName.Value, Observation.ObservationName);
    if (strlen(Observation.ObservationName) != 0)
        _lblIdentification->ObservationName.Valid = TRUE;

    _lblIdentification->PlanetDayNumber.Value = Observation.PlanetDayNumber;
    _lblIdentification->PlanetDayNumber.Valid = TRUE;

    sprintf(_lblIdentification->SpacecraftClockStartCount.Value, "%d", Observation.SCLK_StartCount);
    _lblIdentification->SpacecraftClockStartCount.Valid = TRUE;

    strcpy(_lblIdentification->TargetName.Value, Observation.TargetName);
    if (strlen(Observation.TargetName) != 0)
	    _lblIdentification->TargetName.Valid = TRUE;

    // These two are not quite correct, but are the closest available, and
    // the mosaic labels really want to see these
    strcpy(_lblIdentification->StartTime.Value, Observation.ImageTime);
    if (strlen(Observation.ImageTime) != 0)
        _lblIdentification->StartTime.Valid = TRUE;
    strcpy(_lblIdentification->StopTime.Value, Observation.ImageTime);
    if (strlen(Observation.ImageTime) != 0)
        _lblIdentification->StopTime.Valid = TRUE;

    _lblInstrumentState->ExposureDuration.Value = Observation.ExposureDuration;
    _lblInstrumentState->ExposureDuration.Valid = TRUE;

    strcpy(_lblInstrumentState->ExposureType.Value, Observation.ExposureType);
    if (strlen(Observation.ExposureType) != 0)
        _lblInstrumentState->ExposureType.Valid = TRUE;

    strcpy(_lblInstrumentState->FilterName[0].Value, Observation.FilterName);
    if (strlen(Observation.FilterName) != 0)
        _lblInstrumentState->FilterName[0].Valid = TRUE;

    //sprintf(_lblInstrumentState->FilterNumber.Value, "%d", Observation.FilterNumber);
    _lblInstrumentState->FilterNumber.Value = Observation.FilterNumber;
    _lblInstrumentState->FilterNumber.Valid = TRUE;

    strcpy(_lblInstrumentState->InstrumentDeploymentState.Value, 
                                          Observation.InstDeploymentState);
    if (strlen(Observation.InstDeploymentState) != 0)
        _lblInstrumentState->InstrumentDeploymentState.Valid = TRUE;

    _lblInstrumentState->InstrumentTemperature[0].Value = 
                                   Observation.InstTemperature[0];
    _lblInstrumentState->InstrumentTemperature[0].Valid = TRUE;
    _lblInstrumentState->InstrumentTemperature[1].Value = 
                                   Observation.InstTemperature[1];
    _lblInstrumentState->InstrumentTemperature[1].Valid = TRUE;

    _lblInstrumentState->InstrumentTemperatureCount[0].Value = 
                                   Observation.InstTemperatureCount[0];
    _lblInstrumentState->InstrumentTemperatureCount[0].Valid = TRUE;
    _lblInstrumentState->InstrumentTemperature[1].Value = 
                                   Observation.InstTemperature[1];
    _lblInstrumentState->InstrumentTemperature[1].Valid = TRUE;
    

    _lblImageData->FirstLine.Value = Observation.FirstLine;
    _lblImageData->FirstLine.Valid = TRUE;

    _lblImageData->FirstLineSample.Value = Observation.FirstLineSample;
    _lblImageData->FirstLineSample.Valid = TRUE;

    _lblImageData->Maximum.Value = Observation.Maximum;
    _lblImageData->Maximum.Valid = TRUE;

    _lblImageData->Mean.Value = Observation.Mean;
    _lblImageData->Mean.Valid = TRUE;
    
    _lblImageData->Median.Value = Observation.Median;
    _lblImageData->Median.Valid = TRUE;

    _lblImageData->Minimum.Value = Observation.Minimum;
    _lblImageData->Minimum.Valid = TRUE;

    _lblImageData->StandardDeviation.Value = Observation.StandardDeviation;
    _lblImageData->StandardDeviation.Valid = TRUE;

    ////////////////////////
    // ImpCameraProperty_typ
    ////////////////////////

    ImpCameraProperty_typ Camera;
    memset(&Camera, 0, sizeof(ImpCameraProperty_typ));
    status = ImpCameraModelProperty(_unit, LBL_READ, &Controls, &Camera);

    _lblInstrumentState->AzimuthFov.Value = Camera.AzimuthFOV;
    _lblInstrumentState->AzimuthFov.Valid = TRUE;

    _lblInstrumentState->ElevationFov.Value = Camera.ElevationFOV;
    _lblInstrumentState->ElevationFov.Valid = TRUE;
    
    _lblInstrumentState->InstrumentAzimuthCount.Value = 
                                   Camera.AzimuthMotorClicks;
    _lblInstrumentState->InstrumentAzimuthCount.Valid = TRUE;

    _lblInstrumentState->InstrumentElevationCount.Value = 
                                   Camera.ElevationMotorClicks;
    _lblInstrumentState->InstrumentElevationCount.Valid = TRUE;


    strcpy(_lblCameraModel->ModelName.Value, "MPF IMP");
    _lblCameraModel->ModelName.Valid = TRUE;

    strcpy(_lblCameraModel->ModelType.Value, "CAHV");
    _lblCameraModel->ModelType.Valid = TRUE;

    strcpy(_lblCameraModel->ModelComponentId[0].Value, "C");
    strcpy(_lblCameraModel->ModelComponentId[1].Value, "A");
    strcpy(_lblCameraModel->ModelComponentId[2].Value, "H");
    strcpy(_lblCameraModel->ModelComponentId[3].Value, "V");
    for (int i=0; i<4; i++) _lblCameraModel->ModelComponentId[i].Valid = TRUE;

    _lblCameraModel->ModelComponent1[0].Value = Camera.FocalCenterC[0];
    _lblCameraModel->ModelComponent1[1].Value = Camera.FocalCenterC[1];
    _lblCameraModel->ModelComponent1[2].Value = Camera.FocalCenterC[2];
    _lblCameraModel->ModelComponent1[0].Valid = TRUE;
    _lblCameraModel->ModelComponent1[1].Valid = TRUE;
    _lblCameraModel->ModelComponent1[2].Valid = TRUE;

    _lblCameraModel->ModelComponent2[0].Value=Camera.PointingDirectionA[0];
    _lblCameraModel->ModelComponent2[1].Value=Camera.PointingDirectionA[1];
    _lblCameraModel->ModelComponent2[2].Value=Camera.PointingDirectionA[2];
    _lblCameraModel->ModelComponent2[0].Valid = TRUE;
    _lblCameraModel->ModelComponent2[1].Valid = TRUE;
    _lblCameraModel->ModelComponent2[2].Valid = TRUE;

    _lblCameraModel->ModelComponent3[0].Value =Camera.HorizontalImagePlaneH[0];
    _lblCameraModel->ModelComponent3[1].Value =Camera.HorizontalImagePlaneH[1];
    _lblCameraModel->ModelComponent3[2].Value =Camera.HorizontalImagePlaneH[2];
    _lblCameraModel->ModelComponent3[0].Valid = TRUE;
    _lblCameraModel->ModelComponent3[1].Valid = TRUE;
    _lblCameraModel->ModelComponent3[2].Valid = TRUE;

    _lblCameraModel->ModelComponent4[0].Value = Camera.VerticalImagePlaneV[0];
    _lblCameraModel->ModelComponent4[1].Value = Camera.VerticalImagePlaneV[1];
    _lblCameraModel->ModelComponent4[2].Value = Camera.VerticalImagePlaneV[2];
    _lblCameraModel->ModelComponent4[0].Valid = TRUE;
    _lblCameraModel->ModelComponent4[1].Valid = TRUE;
    _lblCameraModel->ModelComponent4[2].Valid = TRUE;

    strcpy(_lblDerivedGeometry->CoordinateSystemName.Value, "Lander");	
    //!!!! CHECK THIS !!!!
    _lblDerivedGeometry->CoordinateSystemName.Valid = TRUE;

    _lblDerivedGeometry->LanderInstrumentAzimuth.Value = Camera.Azimuth;
    _lblDerivedGeometry->LanderInstrumentAzimuth.Valid = TRUE;

    _lblDerivedGeometry->LanderInstrumentElevation.Value = Camera.Elevation;
    _lblDerivedGeometry->LanderInstrumentElevation.Valid = TRUE;

    //!!!! Is the quaternion direction right here????!!!!
    _quat.setComponents(Camera.LanderSurfaceQuaternion);

    _lblDerivedGeometry->SrfcFxdLclLvlVector.Value[0] = Camera.MllMfxOffsetVector[0];
    _lblDerivedGeometry->SrfcFxdLclLvlVector.Value[1] = Camera.MllMfxOffsetVector[1];
    _lblDerivedGeometry->SrfcFxdLclLvlVector.Value[2] = Camera.MllMfxOffsetVector[2];
    _lblDerivedGeometry->SrfcFxdLclLvlVector.Valid = TRUE;

    _lblDerivedGeometry->SurfaceFixedInstAzimuth.Value = Camera.SurfaceBasedAzimuth;
    _lblDerivedGeometry->SurfaceFixedInstAzimuth.Valid = TRUE;

    _lblDerivedGeometry->SurfaceFixedInstElevation.Value = Camera.SurfaceBasedElevation;
    _lblDerivedGeometry->SurfaceFixedInstElevation.Valid = TRUE;

    return status;
}

////////////////////////////////////////////////
int PigFileModelMpf::roverLabelsInit() 
{

    int status;
    double argument;
    LblCntrl_typ Controls;
    Controls.ProceedOnError = 1;
    /////////////////
    // RvrObsProperty
    /////////////////

    RvrObsProperty_typ Observation;
    memset(&Observation, 0, sizeof(RvrObsProperty_typ));
    status = RvrObservationProperty(_unit, LBL_READ, &Controls, &Observation);

    strcpy(_lblIdentification->FrameId[0].Value, Observation.FrameId);
    if (strlen(Observation.FrameId) != 0)
        _lblIdentification->FrameId[0].Valid = TRUE;

    sprintf(_lblIdentification->ImageId.Value, "%d", Observation.ImageId);
    _lblIdentification->ImageId.Valid = TRUE;

    strcpy(_lblIdentification->ImageTime.Value, Observation.ImageTime);
    if (strlen(Observation.ImageTime) != 0)
        _lblIdentification->ImageTime.Valid = TRUE;

    strcpy(_lblIdentification->ObservationName.Value, Observation.ObservationName);
    if (strlen(Observation.ObservationName) != 0)
      _lblIdentification->ObservationName.Valid = TRUE;

    _lblIdentification->PlanetDayNumber.Value = Observation.PlanetDayNumber;
    _lblIdentification->PlanetDayNumber.Valid = TRUE;

    sprintf(_lblIdentification->SpacecraftClockStartCount.Value, "%d", Observation.SCLK_StartCount);
    _lblIdentification->SpacecraftClockStartCount.Valid = TRUE;

    strcpy(_lblIdentification->TargetName.Value, Observation.TargetName);
    if (strlen(Observation.TargetName) != 0)
        _lblIdentification->TargetName.Valid = TRUE;

    // These two are not quite correct, but are the closest available, and
    // the mosaic labels really want to see these
    strcpy(_lblIdentification->StartTime.Value, Observation.ImageTime);
    if (strlen(Observation.ImageTime) != 0)
        _lblIdentification->StartTime.Valid = TRUE;
    strcpy(_lblIdentification->StopTime.Value, Observation.ImageTime);
    if (strlen(Observation.ImageTime) != 0)
        _lblIdentification->StopTime.Valid = TRUE;

    _lblInstrumentState->ExposureDuration.Value = Observation.ExposureDuration;
    _lblInstrumentState->ExposureDuration.Valid = TRUE;

    strcpy(_lblInstrumentState->ExposureType.Value, Observation.ExposureType);
    if (strlen(Observation.ExposureType) != 0)
        _lblInstrumentState->ExposureType.Valid = TRUE;

    // Z value is always zero
    _lblInstrumentState->InstHostPosition[0].Value = 
                                 Observation.RoverPosition[0];
    _lblInstrumentState->InstHostPosition[0].Valid = TRUE;

    _lblInstrumentState->InstHostPosition[1].Value = 
                                 Observation.RoverPosition[1];
    _lblInstrumentState->InstHostPosition[1].Valid = TRUE;

    _lblInstrumentState->InstHostPosition[2].Value = 0.0;
    _lblInstrumentState->InstHostPosition[2].Valid = TRUE;

    strcpy(_lblDerivedGeometry->CoordinateSystemName.Value, "Rover");

    // !!!!This is Temporary Hack Change it as soon as _lblInstrumentState 
    // get enhanced to accomodate rover specific labels.

    // Observation.RoverHeading is angular measure clockwise from Lander North 
    //in BAMS(Binary Angle Measurement, where 2^16 BAMS equals one revolution
    _lblInstrumentState->InstrumentPosition[0].Value =
                         PigDeg2Rad(Observation.RoverHeading * 360.0/65536);
    _lblInstrumentState->InstrumentPosition[0].Valid = TRUE;

    // Acceleration = reading * 0.0009765 g (I16 value) (where 1 g = 9.8m/sec2)
    // X/Y inclination = arcsine(accel / G) (where G = 1g on Earth, 
    // 0.38g on Mars) X accelerometer sensitive direction is aligned with +X
    // (forward) axis, and indicates pitch (positive is front lower).
    // Y accelerometer sensitive direction is aligned with -Y (right) axis, and
    // indicates roll (positive is left side lower).
    argument = Observation.LinearAccelerometer[0] * 0.0009765/.38;
    if (argument < -1 || argument > 1)  //arcsine domain check
        _lblInstrumentState->InstrumentPosition[1].Valid = FALSE;
    else {
    _lblInstrumentState->InstrumentPosition[1].Value = asin(argument);
    _lblInstrumentState->InstrumentPosition[1].Valid = TRUE;
    }

    argument = Observation.LinearAccelerometer[1] * 0.0009765/.38;
    if (argument < -1 || argument > 1)  //arcsine domain check
        _lblInstrumentState->InstrumentPosition[2].Valid = FALSE;
    else {
    _lblInstrumentState->InstrumentPosition[2].Value = asin(argument);
    _lblInstrumentState->InstrumentPosition[2].Valid = TRUE;
    }

    _lblImageData->FirstLine.Value = Observation.FirstLine;
    _lblImageData->FirstLine.Valid = TRUE;

    _lblImageData->FirstLineSample.Value = Observation.FirstLineSample;
    _lblImageData->FirstLineSample.Valid = TRUE;

    _lblImageData->Maximum.Value = Observation.Maximum;
    _lblImageData->Maximum.Valid = TRUE;

    _lblImageData->Mean.Value = Observation.Mean;
    _lblImageData->Mean.Valid = TRUE;

    _lblImageData->Median.Value = Observation.Median;
    _lblImageData->Median.Valid = TRUE;

    _lblImageData->Minimum.Value = Observation.Minimum;
    _lblImageData->Minimum.Valid = TRUE;

    _lblImageData->StandardDeviation.Value = Observation.StandardDeviation;
    _lblImageData->StandardDeviation.Valid = TRUE;

    ////////////////////////
    // RvrCameraProperty_typ
    ////////////////////////

    RvrCameraProperty_typ Camera;
    memset(&Camera, 0, sizeof(RvrCameraProperty_typ));
    status = RvrCameraModelProperty(_unit, LBL_READ, &Controls, &Camera);

    _lblInstrumentState->AzimuthFov.Value = Camera.AzimuthFOV;
    _lblInstrumentState->AzimuthFov.Valid = TRUE;

    _lblInstrumentState->ElevationFov.Value = Camera.ElevationFOV;
    _lblInstrumentState->ElevationFov.Valid = TRUE;

    strcpy(_lblCameraModel->ModelName.Value, "MPF ROVER");
    _lblCameraModel->ModelName.Valid = TRUE;

    strcpy(_lblCameraModel->ModelType.Value, "CAHV");
    _lblCameraModel->ModelType.Valid = TRUE;

    strcpy(_lblCameraModel->ModelComponentId[0].Value, "C");
    strcpy(_lblCameraModel->ModelComponentId[1].Value, "A");
    strcpy(_lblCameraModel->ModelComponentId[2].Value, "H");
    strcpy(_lblCameraModel->ModelComponentId[3].Value, "V");
    for (int i=0; i<4; i++) _lblCameraModel->ModelComponentId[i].Valid = TRUE;

    _lblCameraModel->ModelComponent1[0].Value = Camera.FocalCenterC[0];
    _lblCameraModel->ModelComponent1[1].Value = Camera.FocalCenterC[1];
    _lblCameraModel->ModelComponent1[2].Value = Camera.FocalCenterC[2];
    _lblCameraModel->ModelComponent1[0].Valid = TRUE;
    _lblCameraModel->ModelComponent1[1].Valid = TRUE;
    _lblCameraModel->ModelComponent1[2].Valid = TRUE;

    _lblCameraModel->ModelComponent2[0].Value=Camera.PointingDirectionA[0];
    _lblCameraModel->ModelComponent2[1].Value=Camera.PointingDirectionA[1];
    _lblCameraModel->ModelComponent2[2].Value=Camera.PointingDirectionA[2];
    _lblCameraModel->ModelComponent2[0].Valid = TRUE;
    _lblCameraModel->ModelComponent2[1].Valid = TRUE;
    _lblCameraModel->ModelComponent2[2].Valid = TRUE;

    _lblCameraModel->ModelComponent3[0].Value =Camera.HorizontalImagePlaneH[0];
    _lblCameraModel->ModelComponent3[1].Value =Camera.HorizontalImagePlaneH[1];
    _lblCameraModel->ModelComponent3[2].Value =Camera.HorizontalImagePlaneH[2];
    _lblCameraModel->ModelComponent3[0].Valid = TRUE;
    _lblCameraModel->ModelComponent3[1].Valid = TRUE;
    _lblCameraModel->ModelComponent3[2].Valid = TRUE;

    _lblCameraModel->ModelComponent4[0].Value = Camera.VerticalImagePlaneV[0];
    _lblCameraModel->ModelComponent4[1].Value = Camera.VerticalImagePlaneV[1];
    _lblCameraModel->ModelComponent4[2].Value = Camera.VerticalImagePlaneV[2];
    _lblCameraModel->ModelComponent4[0].Valid = TRUE;
    _lblCameraModel->ModelComponent4[1].Valid = TRUE;
    _lblCameraModel->ModelComponent4[2].Valid = TRUE;

    strcpy(_lblDerivedGeometry->CoordinateSystemName.Value, "Lander");	
    //!!!! CHECK THIS !!!!
    _lblDerivedGeometry->CoordinateSystemName.Valid = TRUE;

    return status;
}

