/*
  FileModel java proxy implementation.

  Each of the methods in this file implement behaviour on a java object.
  The initialize method found further on will create a c++ object and 
  place the address to this object into the java long variable named cpp.

  Each function will retrieve the pointer to the c++ object from the
  java variable (cpp) via a standard subroutine resolveField (). The file
  mica_common.cc defines a number of handy subroutines.

  In all cases if a call to a routine in mica_common.cc returns zero an 
  exceptional case has occured. Return to the java virtual machine
  (your caller) and let it handle things.
 */
#include <stdio.h>
#include "PigCameraModel.h"
#include "PigFileModel.h"
#include "PigCSReference.h"
#include "PigMission.h"
#include "PigRoverStateManager.h"

#include "mars_support.h"

#include "jpl_mipl_mica_pig_FileModel.h"

#include "mica_common.h"

extern "C" {
    /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    closeFile
   * Signature: ()V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_FileModel_closeFile
  (JNIEnv *, jobject)
  {
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getFOV
   * Signature: (Ljpl/mipl/mica/pig/CameraModel;I)D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_FileModel_getFOV
  (JNIEnv *env, jobject obj, jobject CamModel, jint which)
  {
    PigFileModel   *fm;
    PigCameraModel *cm;

    fm = (PigFileModel*)resolveField(env, obj);
    if (fm == 0)
      return 0;
    
    cm = (PigCameraModel*)resolveField(env, CamModel);
    if (cm == 0)
      return 0;

    return fm->getFOV (cm, which);
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getFileName
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getFileName
  (JNIEnv *, jobject)
  {
      return 0;  // not used 
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getImageBorders
   * Signature: (DDDD)I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_FileModel_getImageBorders
  (JNIEnv *, jobject, jdouble, jdouble, jdouble, jdouble)
  {
      return 0; // not used
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getMissionName
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getMissionName
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    const char *str;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    str = fm->getMissionName();

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return env->NewStringUTF (" ");
    } else
      return env->NewStringUTF(str);

  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getNL
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_FileModel_getNL
  (JNIEnv *, jobject);
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getNS
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_FileModel_getNS
  (JNIEnv *, jobject);
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    initialize
   * Signature: (Ljava/lang/String;)I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_FileModel_initialize
  (JNIEnv *env, jobject obj, jstring fileName)
  {
    /*
      create aPig FileModel.
      1. make the fileName usable
      2. then create the c++ impl of the object
      3. store a pointer to the c++ object in the java proxy
      4. add file coordinates to rsm database
    */
    const char         *fname;
    const char         *mission;
    jboolean           isCopy;
    PigFileModel       *fm;
    jclass             clazz = 0;
    jfieldID           fid = 0;

    fname = env->GetStringUTFChars(fileName, &isCopy);

    fm = PigFileModel::create (fname);
    if (fm == 0) {
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "initialize: could not create a PigFileModel");
      return 0;
    }

    if ((getField(env, obj, &clazz, &fid, "cpp", "J")) != 1)
      return 0;

    env->SetLongField (obj, fid, (long)fm);

    env->ReleaseStringUTFChars (fileName, fname);

	mission=fm->getMissionName();

	PigMission *m = PigMission::getMissionObject(mission);

	// Read coord systems from the input file and add them to the
	// RSM database...
	
	PigRoverStateManager *rsm = m->getRoverStateManager();
	rsm->addFileCoordSystems(fm);
	
    return 1;

  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    isFileOpen
   * Signature: ()Z
   */
  JNIEXPORT jboolean JNICALL Java_jpl_mipl_mica_pig_FileModel_isFileOpen
  (JNIEnv *, jobject);
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    setFileOpen
   * Signature: (Z)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_FileModel_setFileOpen
  (JNIEnv *, jobject, jboolean);
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    testPixelLocation
   * Signature: (DD)I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_FileModel_testPixelLocation
  (JNIEnv *env, jobject obj, jdouble jline, jdouble jsample)
  {
    PigFileModel *fm;
    jint hit;

    fm = (PigFileModel*)resolveField(env, obj);
    if (fm == 0)
      return 0;

    hit = fm->testPixelLocation(jline,jsample);
    //printf("hit = %d",hit);

    return hit;
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getFrameId
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getFrameId
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    const char *str;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    str = fm->getFrameId();

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return 0;
    } else
      return env->NewStringUTF(str);

  }
  /* -============================================================- */
  /*
  * Class:     jpl_mipl_mica_pig_FileModel
  * Method:    getImageId
  * Signature: ()Ljava/lang/String;
  */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getImageId
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    const char *str;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    str = fm->getImageId();

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return 0;
    } else
      return env->NewStringUTF(str);
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getCalibrationSourceId
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getCalibrationSourceId
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    const char *str;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    str = fm->getCalibrationSourceId();

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return 0;
    } else
      return env->NewStringUTF(str);

  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getFilterNumber
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getFilterNumber
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    const char *str;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    str = fm->getFilterNumber();

    // ok, got the name now package it as a jstring
    if (!str) 
      return env->NewStringUTF(str);
    else 
      return 0;
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getInstrumentId
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getInstrumentId
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    const char *str;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    str = fm->getInstrumentId();

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return 0;
    } else
      return env->NewStringUTF(str);

  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getRoverMotionCounter
   * Signature: ()[I
   */
  JNIEXPORT jintArray JNICALL Java_jpl_mipl_mica_pig_FileModel_getRoverMotionCounter
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    int indices[PIG_MAX_CS_INDEX], num_indices;
    jintArray rmc;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    fm->getRoverMotionCounter(indices, num_indices);

    rmc = env->NewIntArray(PIG_MAX_CS_INDEX);
    env->SetIntArrayRegion(rmc, 0, PIG_MAX_CS_INDEX, indices);

    return rmc;
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getCSRefName
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getCSRefName
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    const char *str;
    PigCSReference *csRef;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    int status = fm->getCameraModelCS(csRef, 1); 
    str = csRef->getFrameName();

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return 0;
    } else
      return env->NewStringUTF(str);
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getCSRefIndexSize
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_FileModel_getCSRefIndexSize
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    PigCSReference *csRef;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    int status = fm->getCameraModelCS(csRef, 1);
    return csRef->getNumIndices();
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_FileModel
   * Method:    getUniqueId
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_FileModel_getUniqueId
  (JNIEnv *env, jobject obj)
  {
    PigFileModel *fm;
    char str[33] = "";

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    fm = (PigFileModel*)resolveField (env, obj);
    if (fm == 0)
      return 0; // is I don't have a fm then an exception occured

    fm->getUniqueId(str);

    // ok, got the name now package it as a jstring
    if (strcmp(str, "")) 
      return env->NewStringUTF(str);
    else 
      return 0;
  }
  /* -============================================================- */
}
