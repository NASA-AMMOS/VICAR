#include <stdio.h>
#include <stdlib.h>
#include "PigCameraModel.h"
#include "PigSurfaceModel.h"
#include "PigPointingModel.h"
#include "PigCoordSystem.h"
#include "PigFileModel.h"
#include "mars_support.h"

#include "PigProjectionJ.h"

#include "jpl_mipl_mica_pig_CameraModel.h"
#include "jpl_mipl_mica_pig_SurfaceModel.h"
#include "jpl_mipl_mica_pig_PointingModel.h"
#include "jpl_mipl_mica_pig_FileModel.h"

// do we need to make one of these? PigReproImpl.cc doesn't do this...
// #include "jpl_mipl_mica_operators_ReprojectionOpImage.h"

#include "mica_common.h"

extern "C" {

/*
 * Class:     jpl_mipl_mica_operators_ReprojectionOpImage
 * Method:    intNativeReprojection
 * Signature: (Ljpl/mipl/mica/projection/Projection;Ljpl/mipl/mica/pig/PigPoint;DDDLjpl/mipl/mica/pig/CoordSystem;Ljpl/mipl/mica/pig/SurfaceModel;Ljpl/mipl/mica/pig/PointingModel;Ljpl/mipl/mica/pig/FileModel;IIIIIII[[I[III[[I[IIIDI)V
 */
JNIEXPORT jint JNICALL Java_jpl_mipl_mica_operators_ReprojectionOpImage_intNativeReprojection
  (JNIEnv *env, jobject obj,
   jobject projectionObj,                   jobject surfObj, jobject originObj,
   jobject Coors,     jobject SurMod,       jobject pointModel, jobject fileModel, 
   
   jint dminx, jint dminy,
   jint dwidth,jint dheight,
   jint swidth,jint sheight,
   jint dnumBands,
      
   jobjectArray jdstDataArrays,jintArray jdstBandOffsets,
   jint dstPixelStride,jint dstScanlineStride,
      
   jobjectArray jsrcDataArrays,jintArray jsrcBandOffsets,
   jint srcPixelStride,jint srcScanlineStride,
     
   jdouble FOV, jint pixelSkip
   )

  {
      PigCoordSystem   *cs = NULL;
      PigSurfaceModel  *sm = NULL;
      PigVector        look;
      PigPoint         *surf = NULL;
      PigPoint         *origin = NULL;
      PigPointingModel *pm = NULL;
      PigFileModel     *fm = NULL;
      PigProjection    *proj = NULL;
      jobject          lookObj;
       jclass           clazz=0;
      jmethodID        mid=0;
      jfieldID         fid=0;

      double           out_az;
      double           out_el;
      int              hits;
      int	           infinity = 0;
      double           x_s;
      double           y_l;

      jboolean         isCopy=JNI_FALSE;
      jboolean         isCopyA[3],isCopyB[3],isCopyC=JNI_FALSE,isCopyD=JNI_FALSE;

      jint*            dstDataArrays[3];
      jintArray        dstTempBuffer[3];
      jint*            dstBandOffsets;
      jint*            srcDataArrays[3];
      jintArray        srcTempBuffer[3];
      jint*            srcBandOffsets;

      if ((proj = (PigProjection*)resolveField (env, projectionObj)) == 0)
	  return 0;

      if ((sm = (PigSurfaceModel*)resolveField (env, SurMod)) == 0)
	  return 0;

      if ((cs = (PigCoordSystem*)resolveField (env, Coors)) == 0)
	  return 0;

      if ((pm = (PigPointingModel*)resolveField (env, pointModel)) == 0)
	  return 0;

      if ((fm = (PigFileModel*)resolveField (env, fileModel)) == 0)
	  return 0;
      
      origin = (PigVector*)resolveField (env, originObj);
      if (origin == 0)
	  return 0;

      surf = (PigPoint*)resolveField (env, surfObj);
      if (surf == 0)
	  return 0;

	PigVector camera_orientation = pm->getCameraOrientation(cs);
	PigPoint camera_position = pm->getCameraPosition(cs);
	PigCameraModel* cm = (PigCameraModel*)(pm->getCameraModel());
//	double  FOV = cos((fm->getFOV(cm, 0) + fm->getFOV(cm, 1))/2);
//	if (FOV < 0.0)
//		FOV = 0.0;       // Limit to 90 degrees

      dstBandOffsets= env->GetIntArrayElements (jdstBandOffsets, &isCopyC);
      srcBandOffsets= env->GetIntArrayElements (jsrcBandOffsets, &isCopyD);

      int b; 
      for(b=0;b<dnumBands;b++){
	dstTempBuffer[b] = (jintArray) env->GetObjectArrayElement(jdstDataArrays, b);
	dstDataArrays[b] = env->GetIntArrayElements (dstTempBuffer[b], 0);

	srcTempBuffer[b] =(jintArray) env->GetObjectArrayElement(jsrcDataArrays, b);
	srcDataArrays[b] = env->GetIntArrayElements (srcTempBuffer[b] , &isCopyB[b]);

	// Raster scanning loop
	int dstScanlineOffset = 0;
//	for (int dstY = 0; dstY < dheight; dstY+=pixelSkip)  {
	for (int dstY = dminy; dstY < dheight; dstY+=pixelSkip)  {
	  
	  int dstPixelOffset = dstScanlineOffset;
//	  for (int dstX = 0; dstX < dwidth; dstX+=pixelSkip)  {
	  for (int dstX = dminx; dstX < dwidth; dstX+=pixelSkip)  {
	    
	    // start of marsmap main loop, leave untouched until pixel copy 
		// section below
//	    look = proj->getLookVector (dminx+dstX, dminy+dstY, cs);
	    look = proj->getLookVector (dstX, dstY, cs);
	    surf = proj->getSurfacePoint (*sm, look, *surf, infinity);
	    
		// start new code
		// skip picture if this point is outside of the FOV of the
		// input picture.  To handle cameras that move, we take the
		// XYZ point - the camera center, then dot that with the
		// camera look vector to get the angle between the camera
		// center and the point.  If we're at infinity, we can
		// simply check the point's look vector.
		//
		// One might question the computational efficiency of this
		// rather than just projecting the pixel, but something
		// like it is necessary to keep the point from projecting
		// "backwards" into a camera.
		

		if (infinity) {
			if ((look % camera_orientation) < FOV) {
				dstPixelOffset += dstPixelStride;
				// note: no pixel copy since JAI gave us a zero-filled Raster
				continue;
			}
		}
		else {
			PigVector new_look = *surf - camera_position;
			new_look.normalize();
			if ((new_look % camera_orientation) < FOV) {
				dstPixelOffset += dstPixelStride;
				// note: no pixel copy since JAI gave us a zero-filled Raster
				continue;
			}
		}
		
		// end new code
	    double in_line=0.0, in_samp=0.0;
//	    ((PigCameraModel*)(pm->getCameraModel()))->XYZtoLS(*surf, 
//									   proj->infinity, &in_line, &in_samp, cs);
	    cm->XYZtoLS(*surf, infinity, &in_line, &in_samp, cs);
	    
	    // Check if point is within the input image
	    if (fm->testPixelLocation(in_line,in_samp) != 0) {
	      dstPixelOffset += dstPixelStride;
	      // note: no pixel copy since JAI gave us a zero-filled Raster
	      continue;                       // skip if not
	    }
	    
	    // This line may be needed to ensure bounds checking on array
	    if(in_samp>=0 && in_line>=0 && in_samp<swidth && in_line<sheight) {
	      
	      
	      // get array index in source Raster for source pixel coords:
	      int srcPixelOffset = (int)(((int)in_samp-fm->getXOffset())*srcPixelStride  +
		((int)in_line-fm->getYOffset())*srcScanlineStride);
	      // for each color band, copy src pixel to dst pixel
	      dstDataArrays  [b][dstPixelOffset+dstBandOffsets[b]] = 
		srcDataArrays[b][srcPixelOffset+srcBandOffsets[b]];
	      
	    }
	    dstPixelOffset += dstPixelStride * pixelSkip;
	  }
	  dstScanlineOffset += dstScanlineStride * pixelSkip;
	}
	env->ReleaseIntArrayElements (dstTempBuffer[b],dstDataArrays[b], 0);
	env->ReleaseIntArrayElements (srcTempBuffer[b],srcDataArrays[b],
								JNI_ABORT);
      }

      // push the raster accessors back if a copy
      // note: there's a subsequent copy test on the Java side as well!!!!! 
	  // This could be a second buffer copy OR maybe the JAI people did 
	  // something cool in the RasterAccessor API to make it easier to code 
	  // native Op implementations... I'm not sure.
      env->ReleaseIntArrayElements (jsrcBandOffsets,srcBandOffsets, JNI_ABORT);
      env->ReleaseIntArrayElements (jdstBandOffsets,dstBandOffsets, JNI_ABORT);

      return 0;
  }


    /*
     * Class:     jpl_mipl_mica_operators_ReprojectionOpImage
     * Method:    ushort1to4NativeReprojection
     * Signature: (Ljpl/mipl/mica/projection/Projection;Ljpl/mipl/mica/pig/PigPoint;Ljpl/mipl/mica/pig/PigPoint;DDDLjpl/mipl/mica/pig/CoordSystem;Ljpl/mipl/mica/pig/SurfaceModel;Ljpl/mipl/mica/pig/PointingModel;Ljpl/mipl/mica/pig/FileModel;IIIIIII[[S[III[[S[IIII)V
     */
    JNIEXPORT jint JNICALL Java_jpl_mipl_mica_operators_ReprojectionOpImage_ushort1to4NativeReprojection
	(JNIEnv *env, jobject obj,
	 jobject projectionObj,                   jobject surfObj, jobject originObj,
	 jobject Coors,     jobject SurMod,       jobject pointModel, jobject fileModel, 
	 
	 jint dminx, jint dminy,
	 jint dwidth,jint dheight,
	 jint swidth,jint sheight,
	 jint dnumBands,
	 
	 jobjectArray jdstDataArrays,jintArray jdstBandOffsets,
	 jint dstPixelStride,jint dstScanlineStride,
	 
	 jobjectArray jsrcDataArrays,jintArray jsrcBandOffsets,
	 jint srcPixelStride,jint srcScanlineStride,
	 
	 jint pixelSkip
	 )
	 
    {
	PigCoordSystem   *cs;
	PigSurfaceModel  *sm;
	PigVector        look;
	PigPoint         *surf;
	PigPoint         *origin;
	PigPointingModel *pm;
	PigFileModel     *fm;
	PigProjection    *proj;
	jobject          lookObj;
	jclass           clazz=0;
	jmethodID        mid=0;
	jfieldID         fid=0;
	
	double           out_az;
	double           out_el;
	int              hits;
	int              infinity;
	double           x_s;
	double           y_l;
	
	jboolean         isCopy=JNI_FALSE;
	jboolean         isCopyA,isCopyB,isCopyC=JNI_FALSE,isCopyD=JNI_FALSE;
	
	jshort*          dstDataArray;
	jshortArray      dstTempBuffer;
	jint*            dstBandOffsets;
	jshort*          srcDataArray;
	jshortArray      srcTempBuffer;
	jint*            srcBandOffsets;
	
	if ((proj = (PigProjection*)resolveField (env, projectionObj)) == 0)
	    return 0;
	
	if ((sm = (PigSurfaceModel*)resolveField (env, SurMod)) == 0)
	    return 0;
	
	if ((cs = (PigCoordSystem*)resolveField (env, Coors)) == 0)
	    return 0;
	
	if ((pm = (PigPointingModel*)resolveField (env, pointModel)) == 0)
	    return 0;
	
	if ((fm = (PigFileModel*)resolveField (env, fileModel)) == 0)
	    return 0;
	
	origin = (PigVector*)resolveField (env, originObj);
	surf = (PigPoint*)resolveField (env, surfObj);
	
	jshort* tempPixelRaster = (jshort *)malloc(sizeof(jshort)*dheight*dwidth);
	if(tempPixelRaster == 0)
	  return 0;

	dstBandOffsets= env->GetIntArrayElements (jdstBandOffsets, &isCopyC);
	srcBandOffsets= env->GetIntArrayElements (jsrcBandOffsets, &isCopyD);
	
	int b, dstY, dstX, dstScanlineOffset, dstPixelOffset, srcPixelOffset;
	int alpha=dnumBands-1; // The highest raster color band is the alpha

	PigCameraModel *cm=(PigCameraModel*)(pm->getCameraModel());

	// ****************** REPROJECT FROM SOURCE INTO TEMP PIXEL RASTER *****************
	srcTempBuffer =(jshortArray) env->GetObjectArrayElement(jsrcDataArrays, 0);
	srcDataArray = env->GetShortArrayElements (srcTempBuffer , &isCopyB);
//!!!!	srcDataArray = (jshort *)env->GetPrimitiveArrayCritical (srcTempBuffer , &isCopyB);
	double in_line=0.0, in_samp=0.0;

	int srcBaseOffset=(int)(-fm->getXOffset()*srcPixelStride -fm->getYOffset()*srcScanlineStride + srcBandOffsets[0]);

	// Raster scanning loop
	for (dstY = 0; dstY < dheight; dstY+=pixelSkip)  {
	    for (dstX = 0; dstX < dwidth; dstX+=pixelSkip)  {
		look = proj->getLookVector (dminx+dstX, dminy+dstY, cs);
		surf = proj->getSurfacePoint (*sm, look, *surf, infinity);
		
		cm->XYZtoLS(*surf, infinity, &in_line, &in_samp, cs);
		// Check if point is within the input image
		if (fm->testPixelLocation(in_line,in_samp) != 0) {
		    tempPixelRaster[dstX+dstY*dwidth]=0;
		    continue;                       // skip if not
		}
		// This line may be needed to ensure bounds checking on array
		if(in_samp>=0 && in_line>=0 && in_samp<swidth && in_line<sheight) {
		    // get array index in source Raster for source pixel coords:
		    srcPixelOffset = ((int)in_samp)*srcPixelStride +((int)in_line)*srcScanlineStride + srcBaseOffset;
		    tempPixelRaster[dstX+dstY*dwidth]=srcDataArray[srcPixelOffset];
		} else {
		    tempPixelRaster[dstX+dstY*dwidth]=0;
		}
	    }
	}
	env->ReleaseShortArrayElements (srcTempBuffer,srcDataArray, JNI_ABORT);
//!!!!	env->ReleasePrimitiveArrayCritical (srcTempBuffer,srcDataArray, JNI_ABORT);
	
	// ****************** COPY TEMP PIXEL RASTER INTO DESTINATION RASTERS *****************
	
	for(b=0;b<alpha;b++){
	    dstTempBuffer = (jshortArray) env->GetObjectArrayElement(jdstDataArrays, b);
	    dstDataArray = env->GetShortArrayElements (dstTempBuffer , 0);
//!!!!	    dstDataArray = (jshort *)env->GetPrimitiveArrayCritical (dstTempBuffer , 0);
	    
	    // Raster scanning loop
	    dstScanlineOffset = 0;
	    for (dstY = 0; dstY < dheight; dstY+=pixelSkip)  {
		
		dstPixelOffset = dstScanlineOffset;
		for (dstX = 0; dstX < dwidth; dstX+=pixelSkip)  {
		    
		    dstDataArray[dstPixelOffset+dstBandOffsets[b]] = 
			tempPixelRaster[dstX+dstY*dwidth];
		    
		    dstPixelOffset += dstPixelStride * pixelSkip;
		}
		dstScanlineOffset += dstScanlineStride * pixelSkip;
	    }
	    env->ReleaseShortArrayElements (dstTempBuffer,dstDataArray, 0);
//!!!!	    env->ReleasePrimitiveArrayCritical (dstTempBuffer,dstDataArray, 0);
	}
	
	// ****************** DETERMINE ALPHA CHANNEL FOR DESTINATION RASTER *****************
	
	b=alpha;
	dstTempBuffer = (jshortArray) env->GetObjectArrayElement(jdstDataArrays, b);
	dstDataArray = env->GetShortArrayElements (dstTempBuffer , 0);
//!!!!	dstDataArray = (jshort *)env->GetPrimitiveArrayCritical (dstTempBuffer , 0);
	
	// Raster scanning loop
	dstScanlineOffset = 0;
	for (dstY = 0; dstY < dheight; dstY+=pixelSkip)  {
	    
	    dstPixelOffset = dstScanlineOffset;
	    for (dstX = 0; dstX < dwidth; dstX+=pixelSkip)  {
		
		if( tempPixelRaster[dstX+dstY*dwidth] == 0 ){
		    dstDataArray[dstPixelOffset+dstBandOffsets[b]] = 0;
		} else {
		    dstDataArray[dstPixelOffset+dstBandOffsets[b]] = -1;
		}
		
		dstPixelOffset += dstPixelStride * pixelSkip;
	    }
	    dstScanlineOffset += dstScanlineStride * pixelSkip;
	}
	env->ReleaseShortArrayElements (dstTempBuffer,dstDataArray, 0);
//!!!!	env->ReleasePrimitiveArrayCritical (dstTempBuffer,dstDataArray, 0);

	// ****************** 

	free(tempPixelRaster);

	env->ReleaseIntArrayElements(jsrcBandOffsets,srcBandOffsets, JNI_ABORT);
	env->ReleaseIntArrayElements(jdstBandOffsets,dstBandOffsets, JNI_ABORT);

	return 0;
    }   
}
