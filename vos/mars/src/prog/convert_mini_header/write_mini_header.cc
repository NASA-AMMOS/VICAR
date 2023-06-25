#include <sstream>

#include "zvproto.h"
#include "MiniHeader.h"
#include <string>
using namespace std;
#include <stdio.h>

#include "lbl_mini_header.h"

void write_mini_header(LblMiniHeader_typ &idMiniHeader_, MiniHeader *mh)
{
  idMiniHeader_.ImageId.Value = mh->getProductId();
  idMiniHeader_.ImageId.Valid = LBL_VALID;

  sprintf(idMiniHeader_.MagicNumbers[0].Value,"%X",mh->getMagic0());
  idMiniHeader_.MagicNumbers[0].Valid=LBL_VALID;
  sprintf(idMiniHeader_.MagicNumbers[1].Value,"%X",mh->getMagic1());
  idMiniHeader_.MagicNumbers[1].Valid=LBL_VALID;

  idMiniHeader_.SpacecraftClockStartCount.Value = mh->getSclk();
  idMiniHeader_.SpacecraftClockStartCount.Valid=LBL_VALID;

  idMiniHeader_.DetectorEraseCount.Value = mh->getVFlush();
  idMiniHeader_.DetectorEraseCount.Valid=LBL_VALID;

  sprintf(idMiniHeader_.InstrumentModeId.Value,"MODE_0X%04d 10 MHz",mh->getMode());
  idMiniHeader_.InstrumentModeId.Valid=LBL_VALID;



   char *mode_name[10] = {"SPARE","CCD_STATE","LED1_CONTROL","LED2_CONTROL",
			  "LED3_CONTROL","VIDEO_EXPOSURE","CLKDIV2",
			   "LONG_INTEGRATION_MODE","TEST_MODE","CLKDIV1"};
   int mode_bits[10] = {4,4,1,1,1,1,1,1,1,1};
   unsigned char mode = mh->getMode();
   int b = 0;
   for (int i=0; i<10; i++) {
	b += mode_bits[i];
	int m = (mode >> (16-b));
	if (mode_bits[i] == 4)
	    m = m & 0x0f;
	else
	    m = m & 0x01;
	idMiniHeader_.InstrumentMode[i].Value = m;
	idMiniHeader_.InstrumentMode[i].Valid=LBL_VALID;
	sprintf(idMiniHeader_.InstrumentModeName[i].Value,"%s",mode_name[i]);
	idMiniHeader_.InstrumentModeName[i].Valid=LBL_VALID;
   }

  if(!mh->isZstack()){
      idMiniHeader_.FilterNumber.Value = mh->getFilter();
      idMiniHeader_.FilterNumber.Valid=LBL_VALID;

      idMiniHeader_.ExposureDurationCount.Value = (int)mh->getExposure();
      idMiniHeader_.ExposureDurationCount.Valid=LBL_VALID;

      idMiniHeader_.FirstLineSample.Value = mh->getWindowX()*8 + 1;
      idMiniHeader_.FirstLineSample.Valid=LBL_VALID;
    
      idMiniHeader_.FirstLine.Value = mh->getWindowY()*8 + 1;
      idMiniHeader_.FirstLine.Valid=LBL_VALID;
    
      idMiniHeader_.LineSamples.Value = mh->getWidth()*8;
      idMiniHeader_.LineSamples.Valid=LBL_VALID;
    
      idMiniHeader_.Lines.Value = mh->getHeight()*8;
      idMiniHeader_.Lines.Valid=LBL_VALID;
    
      if(mh->getAutofocus()==0){
          sprintf(idMiniHeader_.InstrumentFocusMode.Value,"%s","MANUAL");
      }else{
          if(mh->getZstack()==0)
              sprintf(idMiniHeader_.InstrumentFocusMode.Value,"%s","AUTOFOCUS");
          else
              sprintf(idMiniHeader_.InstrumentFocusMode.Value,"%s","Z-STACK");
      }
      idMiniHeader_.InstrumentFocusMode.Valid=LBL_VALID;
    
      idMiniHeader_.InstrumentFocusPosition.Value = mh->getInitialPosition();
      idMiniHeader_.InstrumentFocusPosition.Valid=LBL_VALID;
    
      idMiniHeader_.InstrumentFocusStepSize.Value = mh->getStepSize();
      idMiniHeader_.InstrumentFocusStepSize.Valid=LBL_VALID;
    
      // This is missing from the OPGS EDR!!!!
      idMiniHeader_.InstrumentFocusSteps.Value = mh->getAFNSteps();
      idMiniHeader_.InstrumentFocusSteps.Valid = LBL_VALID;

      // This is also missing...
      idMiniHeader_.AutoFocusZstackFlag.Value = mh->getZstack();
      idMiniHeader_.AutoFocusZstackFlag.Valid = LBL_VALID;

      idMiniHeader_.ExposureType.Valid=LBL_VALID;
      string val;
      if(mh->getAutoexposure() == 0){
          val = "MANUAL";
      }else{
          val = "AUTO";
      }
      sprintf(idMiniHeader_.ExposureType.Value,"%s",val.c_str());
    
      idMiniHeader_.AutoExposureDataCut.Value = mh->getTargetDN();
      idMiniHeader_.AutoExposureDataCut.Valid=LBL_VALID;
    
      idMiniHeader_.AutoExposurePixelFraction.Value = mh->getFraction();
      idMiniHeader_.AutoExposurePixelFraction.Valid=LBL_VALID;
    
      idMiniHeader_.AutoExposurePercent.Value = mh->getEarlyTermination();
      idMiniHeader_.AutoExposurePercent.Valid=LBL_VALID;
    
      idMiniHeader_.MaxAutoExposIterationCount.Value = mh->getAENSteps();
      idMiniHeader_.MaxAutoExposIterationCount.Valid=LBL_VALID;
    
      unsigned char* compression = mh->getCompression();
      int cmprsMode = -1;
      if(compression[3] == 0 && compression[2] != 0xff) {
          cmprsMode = 0; // No compression
      } else if(compression[3] == 0 && compression[2] == 0xff) {
	  cmprsMode = 0; // Lossless compression
      } else if(compression[3] != 0 && compression[2] == 0) {
   	  cmprsMode = 1; // JPEG Grey
      } else if(compression[3] != 0 && compression[2] == 1) {
          cmprsMode = 2; // JPEG 422
      } else if(compression[3] != 0 && compression[2] == 2) {
          cmprsMode = 3; // JPEG 444
      } 

      if(cmprsMode >= 0) {
	idMiniHeader_.InstCmprsMode.Value = cmprsMode; 
	idMiniHeader_.InstCmprsMode.Valid = LBL_VALID;
      } else {
	idMiniHeader_.InstCmprsMode.Valid = LBL_PDS_UNK;
      }    

      idMiniHeader_.InstCmprsQuality.Value = mh->getCompQuality();
      idMiniHeader_.InstCmprsQuality.Valid=LBL_VALID;
    
      idMiniHeader_.SampleBitModeId.Valid=LBL_VALID;
      int tmpu32 = compression[7];
      if(tmpu32<=32){
          sprintf(idMiniHeader_.SampleBitModeId.Value,"MMM_LUT%u",tmpu32);
      }else if(tmpu32==255){
          sprintf(idMiniHeader_.SampleBitModeId.Value,"NONE");
      }else{
          idMiniHeader_.SampleBitModeId.Valid=LBL_PDS_UNK;
      }
    
   }

   // Zstack
   else{
      idMiniHeader_.StartImageId.Value = mh->getStartImageId();
      idMiniHeader_.StartImageId.Valid=LBL_VALID;

      idMiniHeader_.ExposureCount.Value = (int)(mh->getExposureCount());
      idMiniHeader_.ExposureCount.Valid=LBL_VALID;

      unsigned int zstackparms = mh->getZstackParms();
      unsigned char imgBlendFlag = ((zstackparms>>3)&0x80)>>7;

      if(imgBlendFlag!=0)
          sprintf(idMiniHeader_.ImageBlendingFlag.Value,"%s","TRUE");
      else
          sprintf(idMiniHeader_.ImageBlendingFlag.Value,"%s","FALSE");
      idMiniHeader_.ImageBlendingFlag.Valid=LBL_VALID;

      unsigned char imgRegFlag = ((zstackparms>>3)&0x40)>>6;

      if(imgRegFlag!=0)
          sprintf(idMiniHeader_.ImageRegistrationFlag.Value,"%s","TRUE");
      else
          sprintf(idMiniHeader_.ImageRegistrationFlag.Value,"%s","FALSE");
      idMiniHeader_.ImageRegistrationFlag.Valid=LBL_VALID;
     
   }  

   char *camstat_name[8] = {"SPARE","UV_LED","VIS1_LED","VIS2_LED","SPARE",
			   "MASTCAM_FILTER_HALL_STATE","MAHLI_COVER_HALL_STATE",
			   "FOCUS_HALL_STATE" };
   unsigned char camstatus = mh->getCameraStatus();
   for(int i=0;i<8;i++){
       if((camstatus>>(7-i)&&0x1) == 0){
           sprintf(idMiniHeader_.InstrumentState[i].Value,"%s","FALSE");
       }else{
           sprintf(idMiniHeader_.InstrumentState[i].Value,"%s","TRUE");
       }
       idMiniHeader_.InstrumentState[i].Valid=LBL_VALID;
       sprintf(idMiniHeader_.InstrumentStateName[i].Value,"%s",camstat_name[i]);
       idMiniHeader_.InstrumentStateName[i].Valid=LBL_VALID;
   }

   idMiniHeader_.InstrumentSerialNumber.Value = mh->getSysSerialNo();
   idMiniHeader_.InstrumentSerialNumber.Valid = LBL_VALID;

   idMiniHeader_.ArticulationDevPosition[0].Value = mh->getMech0();
   idMiniHeader_.ArticulationDevPosition[0].Valid = LBL_VALID;
   idMiniHeader_.ArticulationDevPosition[1].Value = mh->getMech1();
   idMiniHeader_.ArticulationDevPosition[1].Valid = LBL_VALID;

   sprintf(idMiniHeader_.ArticulationDevPositionName[0].Value,"%s","FOCUS");
   idMiniHeader_.ArticulationDevPositionName[0].Valid = LBL_VALID;
   sprintf(idMiniHeader_.ArticulationDevPositionName[1].Value,"%s","FILTER");
   idMiniHeader_.ArticulationDevPositionName[1].Valid = LBL_VALID;

   if (zvptst("M20")) {
      sprintf(idMiniHeader_.ArticulationDevPositionName[2].Value,"%s","ZOOM");
      idMiniHeader_.ArticulationDevPositionName[2].Valid = LBL_VALID;
      idMiniHeader_.ArticulationDevPosition[2].Value = mh->getMech2();
      idMiniHeader_.ArticulationDevPosition[2].Valid = LBL_VALID;
   }

   idMiniHeader_.OffsetModeId.Value = mh->getDCOffset();
   idMiniHeader_.OffsetModeId.Valid = LBL_VALID;

   idMiniHeader_.InitialSize.Value = mh->getInitSize();
   idMiniHeader_.InitialSize.Valid = LBL_VALID;

   // Compute the detector readout rate.  This is documented in table 4.4-4
   // of the MSL MMM SIS

   int clkdiv1 = idMiniHeader_.InstrumentMode[9].Value;
   int clkdiv2 = idMiniHeader_.InstrumentMode[6].Value;
   int ccd_state = idMiniHeader_.InstrumentMode[1].Value;

   if (idMiniHeader_.InstrumentMode[1].Valid &&
       idMiniHeader_.InstrumentMode[6].Valid &&
       idMiniHeader_.InstrumentMode[9].Valid) {

	bool rate_valid = false;
	double rate = 0.0;

	if (clkdiv1 == 0) {
	    if (clkdiv2 == 0 && ccd_state == 0) {
		rate = 20.0;
		rate_valid = true;
	    }
	} else if (clkdiv1 == 1) {	// has to be, but check
	    if (clkdiv2 == 0) {
		if (ccd_state == 0) {
		    rate = 5.0;
		    rate_valid = true;
		} else if (ccd_state == 10) {
		    rate = 3.33;
		    rate_valid = true;
		} else if (ccd_state == 15) {
		    rate = 2.5;
		    rate_valid = true;
		}
	    } else if (clkdiv2 == 1) {	// has to be, but check
		if (ccd_state == 0) {
		    rate = 10.0;
		    rate_valid = true;
		}
	    }
	}

	if (rate_valid) {
	    idMiniHeader_.DetectorReadoutRate.Value = rate;
	    idMiniHeader_.DetectorReadoutRate.Valid = LBL_VALID;
	}
    }
}

