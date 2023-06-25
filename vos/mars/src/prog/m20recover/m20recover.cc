
/* mslrough */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include <stdlib.h>

#include "lbl_mini_header.h"
#include "lbl_image_data.h"
#include "lbl_compression.h"
#include "lbl_instrument_state.h"
#include "lbl_product_request.h"
#include "lbl_observation_request.h"
#include "lbl_camera_model.h"
#include "lbl_telemetry.h"
#include "lbl_articulation.h"
#include "lbl_video_parms.h"
#include "del_prop_grp.h"


/* buffer sizes in main program */
#define MAX_INPUTS 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS


////////////////////////////////////////////////////////////////////////

void main44()
{
    int status, count, def;
    const size_t msgLen = 256;
    char msg[msgLen], fn[256];

    int nids, recovered_nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigFileModel *recovered_model;
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *thumbnail_cs, *recovered_cs;
    int thumbnail_unit, recovered_unit;

    // Outputs

    int out_unit;
    int nlo, nso;
    int bands;
    short int  *recover;     // buffer for transferring data from the recovered. 
                             // file to the output file one line at a time


    zvmessage("M20RECOVER version 1", "");

    // Get the input thumbnail file, and set up initial camera/pointing model
    // for the input.  Although we accept one input only, mars_setup does lots
    // of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    if (strncmp(mission, "M20", 3) != 0) {
	zvmessage("M20RECOVER is a M20-specific program.", "");
//	snprintf(msg, msgLen, "Cannot be used with mission '%s'", mission);
	snprintf(msg, msgLen, "Should not be used with mission '%s'", mission);
	zvmessage(msg, "");
//	zabend();
    }

    //  Get a file unit for the primary input thumbnail image.

    thumbnail_unit = file_models[0]->getUnit();
    if (file_models[0]->isFileOpen())
        file_models[0]->closeFile();
    zvopen(thumbnail_unit,  "OP", "READ",  "OPEN_ACT", "SA",  
        "IO_ACt", "SA",  "U_FORMAT", "HALF",  NULL);
    file_models[0]->setFileOpen(TRUE);

    //  Open the recovered image file and get a file unit for it.

    zvp("INP_RECOVER", fn, &count);
    recovered_model = PigFileModel::create(fn);
    if (recovered_model == NULL) {
	snprintf(msg, msgLen, "Unable to create file model for recovered product %s", fn);
	zvmessage(msg, "");
	zabend();
    }
    recovered_unit = recovered_model->getUnit();
    if (recovered_model->isFileOpen())
        recovered_model->closeFile();
    zvopen(recovered_unit,  "OP", "READ",  "OPEN_ACT", "SA",  
        "IO_ACt", "SA",  "U_FORMAT", "HALF",  NULL);
    recovered_model->setFileOpen(TRUE);

    // Get coord system for input thumbnail file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    thumbnail_cs = m->getCoordSystem(ref);

    snprintf(msg, msgLen, "Interpreting thumbnail image using the %s coordinate frame: %s",
                thumbnail_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get coord system for input recovered file

    PigCSReference *ref2;
    recovered_model->getDerivedImageCS(ref2);
    recovered_cs = m->getCoordSystem(ref2);

    snprintf(msg, msgLen, "Interpreting recovered image using the %s coordinate frame: ",
                recovered_cs->getFrameName(), ref2->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.

    snprintf(msg, msgLen, "Target coordinate frame %s is currently ignored!.",
	    cs->getFrameName());
    zvmessage(msg, "");

    // Output image dimensions and bands is the same as
    // the input recovered image dimensions and bands.

    nlo = recovered_model->getNL();
    nso = recovered_model->getNS();
    bands = recovered_model->getNB();

    // A note to myself:  I need a file-model for the output file for
    // some label handling.  However, I was not able to use 
    //     PigFileModel::create(fn)
    // on "OUT" filename.  It just returned NULL.  If it did not fail,
    // then I could have used the model to get a unit for zvopen.  This
    // same method does work for input files.
    //
    // I used 
    //     PigMission::createFileModel(fn, unit);
    // to create the file-model for the output file (see below).

    // Open the output file.  The image in the input
    // recovered file will be copied to this output file.

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "OP", "WRITE",  "OPEN_ACT", "SA",
	   "U_NS", nso,  "U_NL", nlo,  "U_NB", bands,
	   "U_FORMAT", "HALF", "O_FORMAT", recovered_model->getFormat(),
	   "U_ORG", "BSQ", NULL);
    zvplabel(out_unit, 0, 1);

    // The following models are needed for some label handling.
    // The file-model is only needed for setting PRODUCT_ID and
    // SOURCE_PRODUCT_ID labels in the IDENTIFICATION group.

    zvp("OUT", fn, &count);
    PigLabelModel *labelModel = m->createLabelModel(out_unit);
    PigFileModel *out_model = m->createFileModel(fn, out_unit);

    // gather all filemodels for handing over to label model

    int nids_all = 2;
    PigFileModel *file_models_all[2];
    file_models_all[0] = file_models[0];
    file_models_all[1] = recovered_model;
    labelModel->writeProductIds(file_models_all, nids_all);


    // Allocate memory for output.

    recover = (short int *)malloc(nso * sizeof(short int));
    if (recover == NULL) {
        snprintf(msg, msgLen, "Unable to allocate memory for output.");
        zvmessage(msg, "");
        zabend();
    }

    // Transfer reachability data from the temporary file to the
    // output image file one band and one line at a time.

    for (int band = 0; band < bands; band++) {
	for (int line = 0; line < nlo; line++) {
	    zvread(recovered_unit, recover, "BAND", band+1, "LINE", line+1, NULL);
	    zvwrit(out_unit, recover, "BAND", band+1, "LINE", line+1, NULL);
	}
    }

    // ============ VIDEO_PARMS ============

    // If the product is a video recovered product, indicated by
    // VIDEO_PARMS.GROUP_APPLICABILITY_FLAG=TRUE, then we have to transfer
    // over most of the time values - because there is just one thumb for
    // each GOP group of up to 16 images, and the times are adjusted in edrgen
    // to be correct for the video parms.  We also transfer over the video group
    // itself.

    int do_video = FALSE;

    LblVideoParms_typ *lblVideo = new LblVideoParms_typ;
    if (lblVideo == NULL) {
	zvmessage("Unable to allocate memory for label modification!", "");
	zabend();
    }
    status = LblVideoParms(recovered_unit, LBL_READ, lblVideo, 1);
    if (lblVideo->GroupApplicabilityFlag.Valid) {
	if (strcasecmp(lblVideo->GroupApplicabilityFlag.Value, "TRUE") == 0) {

	    // We got video!

	    do_video = TRUE;

	    status = LblVideoParms(out_unit, LBL_WRITE, lblVideo, 1);
	    delete lblVideo;
	}
    }


    // The following statement is only needed for setting PRODUCT_ID
    // and SOURCE_PRODUCT_ID labels in the IDENTIFICATION group.

    labelModel->setIdentification(NULL);

    // After copying image, now it is time to modify the labels.

    // ========== IDENTIFICATION ==========

    LblIdentification_typ *lblIdentification_out = new LblIdentification_typ;
    LblIdentification_typ *lblIdentification = new LblIdentification_typ;
    if (lblIdentification_out == NULL || lblIdentification == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }
    status = LblIdentification(thumbnail_unit, LBL_READ, lblIdentification_out, 1);
    status = LblIdentification(recovered_unit, LBL_READ, lblIdentification, 1);

    lblIdentification_out->ProductId.Valid = LBL_VALID;
    strcpy(lblIdentification_out->ProductId.Value, out_model->getProductId());

    for (int i=1; i<LBL_SOURCE_PROD_ID_ITEMS; i++)
        lblIdentification_out->SourceProductId[i].Valid = LBL_INVALID;
    lblIdentification_out->SourceProductId[0].Valid = LBL_VALID;
    strcpy(lblIdentification_out->SourceProductId[0].Value, out_model->getProductId());

    lblIdentification_out->ImageType.Valid = LBL_VALID;
    if (lblIdentification->ImageType.Valid)
        strcpy(lblIdentification_out->ImageType.Value, lblIdentification->ImageType.Value);
    else
        strcpy(lblIdentification_out->ImageType.Value, "REGULAR");

    if (lblIdentification->GeometryProjectionType.Valid) {
        lblIdentification_out->GeometryProjectionType.Valid = lblIdentification->GeometryProjectionType.Valid;
        strcpy(lblIdentification_out->GeometryProjectionType.Value, lblIdentification->GeometryProjectionType.Value);
    }
    if (lblIdentification->ImageAcquireMode.Valid) {
        lblIdentification_out->ImageAcquireMode.Valid = lblIdentification->ImageAcquireMode.Valid;
        strcpy(lblIdentification_out->ImageAcquireMode.Value, lblIdentification->ImageAcquireMode.Value);
    }
    for (int i=0; i<LBL_MAX_ITEMS; i++) {
        if (lblIdentification->InstrumentHostId[i].Valid) {
            lblIdentification_out->InstrumentHostId[i].Valid = lblIdentification->InstrumentHostId[i].Valid;
            strcpy(lblIdentification_out->InstrumentHostId[i].Value, lblIdentification->InstrumentHostId[i].Value);
        }
        if (lblIdentification->InstrumentHostName[i].Valid) {
            lblIdentification_out->InstrumentHostName[i].Valid = lblIdentification->InstrumentHostName[i].Valid;
            strcpy(lblIdentification_out->InstrumentHostName[i].Value, lblIdentification->InstrumentHostName[i].Value);
        }
        if (lblIdentification->InstrumentId[i].Valid) {
            lblIdentification_out->InstrumentId[i].Valid = lblIdentification->InstrumentId[i].Valid;
            strcpy(lblIdentification_out->InstrumentId[i].Value, lblIdentification->InstrumentId[i].Value);
        }
        if (lblIdentification->InstrumentName[i].Valid) {
            lblIdentification_out->InstrumentName[i].Valid = lblIdentification->InstrumentName[i].Valid;
            strcpy(lblIdentification_out->InstrumentName[i].Value, lblIdentification->InstrumentName[i].Value);
        }
        if (lblIdentification->InstrumentType[i].Valid) {
            lblIdentification_out->InstrumentType[i].Valid = lblIdentification->InstrumentType[i].Valid;
            strcpy(lblIdentification_out->InstrumentType[i].Value, lblIdentification->InstrumentType[i].Value);
        }
        if (lblIdentification->MissionName[i].Valid) {
            lblIdentification_out->MissionName[i].Valid = lblIdentification->MissionName[i].Valid;
            strcpy(lblIdentification_out->MissionName[i].Value, lblIdentification->MissionName[i].Value);
        }
    }
    if (lblIdentification->InstrumentSerialNumber.Valid)
        lblIdentification_out->InstrumentSerialNumber = lblIdentification->InstrumentSerialNumber;
    if (lblIdentification->InstrumentVersionId.Valid) {
        lblIdentification_out->InstrumentVersionId.Valid = lblIdentification->InstrumentVersionId.Valid;
        strcpy(lblIdentification_out->InstrumentVersionId.Value, lblIdentification->InstrumentVersionId.Value);
    }
    if (lblIdentification->MissionPhaseName.Valid) {
        lblIdentification_out->MissionPhaseName.Valid = lblIdentification->MissionPhaseName.Valid;
        strcpy(lblIdentification_out->MissionPhaseName.Value, lblIdentification->MissionPhaseName.Value);
    }
    if (lblIdentification->TargetName.Valid) {
        lblIdentification_out->TargetName.Valid = lblIdentification->TargetName.Valid;
        strcpy(lblIdentification_out->TargetName.Value, lblIdentification->TargetName.Value);
    }
    if (lblIdentification->TargetType.Valid) {
        lblIdentification_out->TargetType.Valid = lblIdentification->TargetType.Valid;
        strcpy(lblIdentification_out->TargetType.Value, lblIdentification->TargetType.Value);
    }
    if (lblIdentification->ProducerInstitutionName.Valid) {
        lblIdentification_out->ProducerInstitutionName.Valid = lblIdentification->ProducerInstitutionName.Valid;
        strcpy(lblIdentification_out->ProducerInstitutionName.Value, lblIdentification->ProducerInstitutionName.Value);
    }
    if (lblIdentification->ProductCreationTime.Valid) {
        lblIdentification_out->ProductCreationTime.Valid = lblIdentification->ProductCreationTime.Valid;
        strcpy(lblIdentification_out->ProductCreationTime.Value, lblIdentification->ProductCreationTime.Value);
    }

    // If we're in video mode, transfer over more time values

    if (do_video) {

	if (lblIdentification->ImageTime.Valid) {
	    lblIdentification_out->ImageTime.Valid = lblIdentification->ImageTime.Valid;
	    strcpy(lblIdentification_out->ImageTime.Value, lblIdentification->ImageTime.Value);
	}
	if (lblIdentification->LocalMeanSolarTime.Valid) {
	    lblIdentification_out->LocalMeanSolarTime.Valid = lblIdentification->LocalMeanSolarTime.Valid;
	    strcpy(lblIdentification_out->LocalMeanSolarTime.Value, lblIdentification->LocalMeanSolarTime.Value);
	}
	if (lblIdentification->LocalTrueSolarTime.Valid) {
	    lblIdentification_out->LocalTrueSolarTime.Valid = lblIdentification->LocalTrueSolarTime.Valid;
	    strcpy(lblIdentification_out->LocalTrueSolarTime.Value, lblIdentification->LocalTrueSolarTime.Value);
	}
	if (lblIdentification->SpacecraftClockStartCount.Valid) {
	    lblIdentification_out->SpacecraftClockStartCount.Valid = lblIdentification->SpacecraftClockStartCount.Valid;
	    strcpy(lblIdentification_out->SpacecraftClockStartCount.Value, lblIdentification->SpacecraftClockStartCount.Value);
	}
	if (lblIdentification->SpacecraftClockStopCount.Valid) {
	    lblIdentification_out->SpacecraftClockStopCount.Valid = lblIdentification->SpacecraftClockStopCount.Valid;
	    strcpy(lblIdentification_out->SpacecraftClockStopCount.Value, lblIdentification->SpacecraftClockStopCount.Value);
	}
	if (lblIdentification->StartTime.Valid) {
	    lblIdentification_out->StartTime.Valid = lblIdentification->StartTime.Valid;
	    strcpy(lblIdentification_out->StartTime.Value, lblIdentification->StartTime.Value);
	}
	if (lblIdentification->StopTime.Valid) {
	    lblIdentification_out->StopTime.Valid = lblIdentification->StopTime.Valid;
	    strcpy(lblIdentification_out->StopTime.Value, lblIdentification->StopTime.Value);
	}
    }

    status = LblIdentification(out_unit, LBL_WRITE, lblIdentification_out, 1);
    delete lblIdentification;
    delete lblIdentification_out;


    // ========== TELEMETRY ==========

    LblTelemetry_typ *lblTelemetry_out = new LblTelemetry_typ;
    LblTelemetry_typ *lblTelemetry = new LblTelemetry_typ;
    if (lblTelemetry_out == NULL || lblTelemetry == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }
    status = LblTelemetry(thumbnail_unit, LBL_READ, lblTelemetry_out, 1);
    status = LblTelemetry(recovered_unit, LBL_READ, lblTelemetry, 1);

    if (lblTelemetry->ApplicationPacketId.Valid)
        lblTelemetry_out->ApplicationPacketId = lblTelemetry->ApplicationPacketId;
    if (lblTelemetry->ExpectedPackets.Valid)
        lblTelemetry_out->ExpectedPackets = lblTelemetry->ExpectedPackets;
    if (lblTelemetry->ReceivedPackets.Valid)
        lblTelemetry_out->ReceivedPackets = lblTelemetry->ReceivedPackets;
    if (lblTelemetry->DownloadPriority.Valid)
        lblTelemetry_out->DownloadPriority = lblTelemetry->DownloadPriority;
    if (lblTelemetry->TelemetrySourceSize.Valid)
        lblTelemetry_out->TelemetrySourceSize = lblTelemetry->TelemetrySourceSize;
    if (lblTelemetry->TelemetrySourceProductCheckSum.Valid)
        lblTelemetry_out->TelemetrySourceProductCheckSum = lblTelemetry->TelemetrySourceProductCheckSum;
    if (lblTelemetry->BootCount.Valid)
        lblTelemetry_out->BootCount = lblTelemetry->BootCount;
    if (lblTelemetry->ApplicationProcessName.Valid) {
        lblTelemetry_out->ApplicationProcessName.Valid = lblTelemetry->ApplicationProcessName.Valid;
        strcpy(lblTelemetry_out->ApplicationProcessName.Value, lblTelemetry->ApplicationProcessName.Value);
    }
    if (lblTelemetry->EarthReceivedStartTime.Valid) {
        lblTelemetry_out->EarthReceivedStartTime.Valid = lblTelemetry->EarthReceivedStartTime.Valid;
        strcpy(lblTelemetry_out->EarthReceivedStartTime.Value, lblTelemetry->EarthReceivedStartTime.Value);
    }
    if (lblTelemetry->EarthReceivedStopTime.Valid) {
        lblTelemetry_out->EarthReceivedStopTime.Valid = lblTelemetry->EarthReceivedStopTime.Valid;
        strcpy(lblTelemetry_out->EarthReceivedStopTime.Value, lblTelemetry->EarthReceivedStopTime.Value);
    }
    if (lblTelemetry->TelemetryProviderId.Valid) {
        lblTelemetry_out->TelemetryProviderId.Valid = lblTelemetry->TelemetryProviderId.Valid;
        strcpy(lblTelemetry_out->TelemetryProviderId.Value, lblTelemetry->TelemetryProviderId.Value);
    }
    if (lblTelemetry->TelemetrySourceHostName.Valid) {
        lblTelemetry_out->TelemetrySourceHostName.Valid = lblTelemetry->TelemetrySourceHostName.Valid;
        strcpy(lblTelemetry_out->TelemetrySourceHostName.Value, lblTelemetry->TelemetrySourceHostName.Value);
    }
    if (lblTelemetry->TelemetrySourceName.Valid) {
        lblTelemetry_out->TelemetrySourceName.Valid = lblTelemetry->TelemetrySourceName.Valid;
        strcpy(lblTelemetry_out->TelemetrySourceName.Value, lblTelemetry->TelemetrySourceName.Value);
    }
    if (lblTelemetry->TelemetrySourceType.Valid) {
        lblTelemetry_out->TelemetrySourceType.Valid = lblTelemetry->TelemetrySourceType.Valid;
        strcpy(lblTelemetry_out->TelemetrySourceType.Value, lblTelemetry->TelemetrySourceType.Value);
    }
    if (lblTelemetry->CommunicationSessionId.Valid) {
        lblTelemetry_out->CommunicationSessionId.Valid = lblTelemetry->CommunicationSessionId.Valid;
        strcpy(lblTelemetry_out->CommunicationSessionId.Value, lblTelemetry->CommunicationSessionId.Value);
    }
    if (lblTelemetry->ExpectedTransmissionPath.Valid) {
        lblTelemetry_out->ExpectedTransmissionPath.Valid = lblTelemetry->ExpectedTransmissionPath.Valid;
        strcpy(lblTelemetry_out->ExpectedTransmissionPath.Value, lblTelemetry->ExpectedTransmissionPath.Value);
    }
    if (lblTelemetry->ProductCompletionStatus.Valid) {
        lblTelemetry_out->ProductCompletionStatus.Valid = lblTelemetry->ProductCompletionStatus.Valid;
        strcpy(lblTelemetry_out->ProductCompletionStatus.Value, lblTelemetry->ProductCompletionStatus.Value);
    }
    if (lblTelemetry->TelemetrySourceStartTime.Valid) {
        lblTelemetry_out->TelemetrySourceStartTime.Valid = lblTelemetry->TelemetrySourceStartTime.Valid;
        strcpy(lblTelemetry_out->TelemetrySourceStartTime.Value, lblTelemetry->TelemetrySourceStartTime.Value);
    }
    if (lblTelemetry->AutoDeleteFlag.Valid) {
        lblTelemetry_out->AutoDeleteFlag.Valid = lblTelemetry->AutoDeleteFlag.Valid;
        strcpy(lblTelemetry_out->AutoDeleteFlag.Value, lblTelemetry->AutoDeleteFlag.Value);
    }
    if (lblTelemetry->TransmissionPath.Valid) {
        lblTelemetry_out->TransmissionPath.Valid = lblTelemetry->TransmissionPath.Valid;
        strcpy(lblTelemetry_out->TransmissionPath.Value, lblTelemetry->TransmissionPath.Value);
    }
    if (lblTelemetry->VirtualChannelId.Valid) {
        lblTelemetry_out->VirtualChannelId.Valid = lblTelemetry->VirtualChannelId.Valid;
        strcpy(lblTelemetry_out->VirtualChannelId.Value, lblTelemetry->VirtualChannelId.Value);
    }
    if (lblTelemetry->DpCmprsType.Valid) {
        lblTelemetry_out->DpCmprsType.Valid = lblTelemetry->DpCmprsType.Valid;
        strcpy(lblTelemetry_out->DpCmprsType.Value, lblTelemetry->DpCmprsType.Value);
    }

    status = LblTelemetry(out_unit, LBL_WRITE, lblTelemetry_out, 1);
    delete lblTelemetry;
    delete lblTelemetry_out;


    // ========== GEOMETRIC CAMERA MODEL ==========

    LblCameraModel_typ *lblCameraModel_out = new LblCameraModel_typ;
    LblCameraModel_typ *lblCameraModel = new LblCameraModel_typ;
    if (lblCameraModel_out == NULL || lblCameraModel == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }
    status = LblGeometricCameraModel(thumbnail_unit, LBL_READ, lblCameraModel_out, 1);
    status = LblGeometricCameraModel(recovered_unit, LBL_READ, lblCameraModel, 1);

    if (lblCameraModel->CameraSerialNumber.Valid)
        lblCameraModel_out->CameraSerialNumber = lblCameraModel->CameraSerialNumber;
    if (lblCameraModel->ModelDesc.Valid) {
        lblCameraModel_out->ModelDesc.Valid = lblCameraModel->ModelDesc.Valid;
        strcpy(lblCameraModel_out->ModelDesc.Value, lblCameraModel->ModelDesc.Value);
    }
    if (lblCameraModel->ModelType.Valid) {
        lblCameraModel_out->ModelType.Valid = lblCameraModel->ModelType.Valid;
        strcpy(lblCameraModel_out->ModelType.Value, lblCameraModel->ModelType.Value);
    }
    if (lblCameraModel->ReferenceCoordSystemName.Valid) {
        lblCameraModel_out->ReferenceCoordSystemName.Valid = lblCameraModel->ReferenceCoordSystemName.Valid;
        strcpy(lblCameraModel_out->ReferenceCoordSystemName.Value, lblCameraModel->ReferenceCoordSystemName.Value);
    }
    for (int i=0; i<LBL_CAM_MODEL_ELEMENTS; i++) {
        if (lblCameraModel->ModelComponentId[i].Valid) {
            lblCameraModel_out->ModelComponentId[i].Valid = lblCameraModel->ModelComponentId[i].Valid;
            strcpy(lblCameraModel_out->ModelComponentId[i].Value, lblCameraModel->ModelComponentId[i].Value);
        }
        if (lblCameraModel->ModelComponentName[i].Valid) {
            lblCameraModel_out->ModelComponentName[i].Valid = lblCameraModel->ModelComponentName[i].Valid;
            strcpy(lblCameraModel_out->ModelComponentName[i].Value, lblCameraModel->ModelComponentName[i].Value);
        }
    }

    status = LblGeometricCameraModel(out_unit, LBL_WRITE, lblCameraModel_out, 1);
    delete lblCameraModel;
    delete lblCameraModel_out;


    // ========== OBSERVATION REQUEST PARMS ==========

    LblObsRequest_typ *lblObsRequest_out = new LblObsRequest_typ;
    LblObsRequest_typ *lblObsRequest = new LblObsRequest_typ;
    if (lblObsRequest_out == NULL || lblObsRequest == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }
    status = LblObservationRequest(thumbnail_unit, LBL_READ, lblObsRequest_out, 1);
    status = LblObservationRequest(recovered_unit, LBL_READ, lblObsRequest, 1);

    if (lblObsRequest->DetectorEraseCount.Valid)
        lblObsRequest_out->DetectorEraseCount = lblObsRequest->DetectorEraseCount;
    if (lblObsRequest->InstrumentModeId.Valid) {
        lblObsRequest_out->InstrumentModeId.Valid = lblObsRequest->InstrumentModeId.Valid;
        strcpy(lblObsRequest_out->InstrumentModeId.Value, lblObsRequest->InstrumentModeId.Value);
    }

    status = LblObservationRequest(out_unit, LBL_WRITE, lblObsRequest_out, 1);
    delete lblObsRequest;
    delete lblObsRequest_out;


    // ========== PRODUCT REQUEST PARMS ==========

    LblProdRequest_typ *lblProdRequest_out = new LblProdRequest_typ;
    LblProdRequest_typ *lblProdRequest = new LblProdRequest_typ;
    if (lblProdRequest_out == NULL || lblProdRequest == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }

        // ========== SUBFRAME REQUEST PARMS ==========

    status = LblProductRequest(thumbnail_unit, LBL_READ, lblProdRequest_out, 1, SUBFRAME_REQUEST_PARMS_PROPERTY_NAME);
    status = LblProductRequest(recovered_unit, LBL_READ, lblProdRequest, 1, SUBFRAME_REQUEST_PARMS_PROPERTY_NAME);

    if (lblProdRequest->SubframeType.Valid) {
        lblProdRequest_out->SubframeType.Valid = lblProdRequest->SubframeType.Valid;
        strcpy(lblProdRequest_out->SubframeType.Value, lblProdRequest->SubframeType.Value);
    }

    status = LblProductRequest(out_unit, LBL_WRITE, lblProdRequest_out, 1, SUBFRAME_REQUEST_PARMS_PROPERTY_NAME);

    delete lblProdRequest;
    delete lblProdRequest_out;


    // ========== INSTRUMENT STATE PARMS ==========

    LblInstrumentState_typ *lblInstrumentState_out = new LblInstrumentState_typ;
    LblInstrumentState_typ *lblInstrumentState = new LblInstrumentState_typ;
    if (lblInstrumentState_out == NULL || lblInstrumentState == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }
    status = LblInstrumentStateParms(thumbnail_unit, LBL_READ, lblInstrumentState_out, 1);
    status = LblInstrumentStateParms(recovered_unit, LBL_READ, lblInstrumentState, 1);

    if (lblInstrumentState->DetectorFirstLine.Valid)
        lblInstrumentState_out->DetectorFirstLine = lblInstrumentState->DetectorFirstLine;
    if (lblInstrumentState->DetectorFirstLineSample.Valid)
        lblInstrumentState_out->DetectorFirstLineSample = lblInstrumentState->DetectorFirstLineSample;
    if (lblInstrumentState->DetectorLines.Valid)
        lblInstrumentState_out->DetectorLines = lblInstrumentState->DetectorLines;
    if (lblInstrumentState->DetectorLineSamples.Valid)
        lblInstrumentState_out->DetectorLineSamples = lblInstrumentState->DetectorLineSamples;
    if (lblInstrumentState->ExposureCount.Valid)
        lblInstrumentState_out->ExposureCount = lblInstrumentState->ExposureCount;
    if (lblInstrumentState->ExposureDuration.Valid)
        lblInstrumentState_out->ExposureDuration = lblInstrumentState->ExposureDuration;
    if (lblInstrumentState->FilterNumber.Valid)
        lblInstrumentState_out->FilterNumber = lblInstrumentState->FilterNumber;
    if (lblInstrumentState->FocusPositionCount.Valid)
        lblInstrumentState_out->FocusPositionCount = lblInstrumentState->FocusPositionCount;
    if (lblInstrumentState->PixelAveragingHeight.Valid)
        lblInstrumentState_out->PixelAveragingHeight = lblInstrumentState->PixelAveragingHeight;
    if (lblInstrumentState->PixelAveragingWidth.Valid)
        lblInstrumentState_out->PixelAveragingWidth = lblInstrumentState->PixelAveragingWidth;
    if (lblInstrumentState->FilterPositionCount.Valid)
        lblInstrumentState_out->FilterPositionCount = lblInstrumentState->FilterPositionCount;
    if (lblInstrumentState->ZoomPositionCount.Valid)
        lblInstrumentState_out->ZoomPositionCount = lblInstrumentState->ZoomPositionCount;
    if (lblInstrumentState->FocalLength.Valid)
        lblInstrumentState_out->FocalLength = lblInstrumentState->FocalLength;
    if (lblInstrumentState->DCOffset.Valid)
        lblInstrumentState_out->DCOffset = lblInstrumentState->DCOffset;
    if (lblInstrumentState->BayerMethod.Valid) {
        lblInstrumentState_out->BayerMethod.Valid = lblInstrumentState->BayerMethod.Valid;
        strcpy(lblInstrumentState_out->BayerMethod.Value, lblInstrumentState->BayerMethod.Value);
    }
    if (lblInstrumentState->CfaType.Valid) {
        lblInstrumentState_out->CfaType.Valid = lblInstrumentState->CfaType.Valid;
        strcpy(lblInstrumentState_out->CfaType.Value, lblInstrumentState->CfaType.Value);
    }
    if (lblInstrumentState->CfaVenue.Valid) {
        lblInstrumentState_out->CfaVenue.Valid = lblInstrumentState->CfaVenue.Valid;
        strcpy(lblInstrumentState_out->CfaVenue.Value, lblInstrumentState->CfaVenue.Value);
    }
    if (lblInstrumentState->DownsampleMethod.Valid) {
        lblInstrumentState_out->DownsampleMethod.Valid = lblInstrumentState->DownsampleMethod.Valid;
        strcpy(lblInstrumentState_out->DownsampleMethod.Value, lblInstrumentState->DownsampleMethod.Value);
    }
    if (lblInstrumentState->ExposureDurationUnit.Valid) {
        lblInstrumentState_out->ExposureDurationUnit.Valid = lblInstrumentState->ExposureDurationUnit.Valid;
        strcpy(lblInstrumentState_out->ExposureDurationUnit.Value, lblInstrumentState->ExposureDurationUnit.Value);
    }
    for (int i=0; i<LBL_MAX_ITEMS; i++)
        if (lblInstrumentState->FilterName[i].Valid) {
            lblInstrumentState_out->FilterName[i].Valid = lblInstrumentState->FilterName[i].Valid;
            strcpy(lblInstrumentState_out->FilterName[i].Value, lblInstrumentState->FilterName[i].Value);
        }
    if (lblInstrumentState->FocalLengthUnit.Valid) {
        lblInstrumentState_out->FocalLengthUnit.Valid = lblInstrumentState->FocalLengthUnit.Valid;
        strcpy(lblInstrumentState_out->FocalLengthUnit.Value, lblInstrumentState->FocalLengthUnit.Value);
    }

    status = LblInstrumentStateParms(out_unit, LBL_WRITE, lblInstrumentState_out, 1);
    delete lblInstrumentState;
    delete lblInstrumentState_out;


    // ========== COMPRESSION PARMS ==========

    LblCompression_typ *lblCompression_out = new LblCompression_typ;
    if (lblCompression_out == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }
    del_prop_grp(out_unit, "COMPRESSION_PARMS", 1);
    status = LblCompressionParms(recovered_unit, LBL_READ, lblCompression_out, 1);
    status = LblCompressionParms(out_unit, LBL_WRITE, lblCompression_out, 1);
    delete lblCompression_out;


    // ========== IMAGE DATA ==========

    LblImageData_typ *lblImageData_out = new LblImageData_typ;
    if (lblImageData_out == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }
    del_prop_grp(out_unit, "IMAGE_DATA", 1);
    status = LblImageData(recovered_unit, LBL_READ, lblImageData_out, 1);
    status = LblImageData(out_unit, LBL_WRITE, lblImageData_out, 1);
    delete lblImageData_out;


    // ========== MINI HEADER ==========

    LblMiniHeader_typ *lblMiniHeader_out = new LblMiniHeader_typ;
    if (lblMiniHeader_out == NULL) {
        zvmessage("Unable to allocate memory for label modification!", "");
        zabend();
    }
    del_prop_grp(out_unit, "MINI_HEADER", 1);
    status = LblMiniHeader(recovered_unit, LBL_READ, lblMiniHeader_out, 1, (const char*)NULL);
    status = LblMiniHeader(out_unit, LBL_WRITE, lblMiniHeader_out, 1, (const char*)NULL);
    delete lblMiniHeader_out;


    zvclose(recovered_unit, NULL);
    zvclose(out_unit, NULL);
    zvclose(thumbnail_unit, NULL);
}

