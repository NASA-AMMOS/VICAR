#include <stdio.h>
#include "PigCameraModel.h"
#include "PigPointingModel.h"

#include "mars_support.h"

#include "jpl_mipl_mica_pig_PointingModel.h"

#include "mica_common.h"

extern "C" {
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    initialize
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_PointingModel_initialize
  (JNIEnv *env, jobject obj, jobject cameraModel, jstring filename)
  {
    const char         *fname;
    jboolean           isCopy;
    PigCameraModel     *cm;
    PigPointingModel   *pm;
    jclass             clazz = 0;
    jfieldID           fid = 0;

    // first get the c++ camera model pointer
    cm = (PigCameraModel*)resolveField (env, cameraModel);
    if (cm == 0)
      return 0;

    fname = env->GetStringUTFChars(filename, &isCopy);
	//printf ("PointingModel fname = %s\n", fname);
    if ((getField(env, obj, &clazz, &fid, "cpp", "J")) != 1)
      return 0;

    pm = PigPointingModel::create(cm, fname, NULL, true);
    if (pm == 0) 
      return 0;
    pm->pointCamera(fname);

    env->SetLongField (obj, fid, (long)pm);

    env->ReleaseStringUTFChars (filename, fname);

    return 1;
  }
    /* -============================================================- */
    /*
     * Class:     PointingModel
     * Method:    getInstrumentName
     * Signature: ()Ljava/lang/String;
     */
    JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_PointingModel_getInstrumentName
	(JNIEnv *env, jobject obj)
    {
    	PigPointingModel *pm;
	const char *str;
	
	/* the java object is a proxy to the c++ object.
	   resolveField find and retrieves the c++ pointer
	   that was stored in the java object.
	*/
	pm = (PigPointingModel*)resolveField (env, obj);
	if (pm == 0)
	    return 0; // is I don't have a pm then an exception occured
	
	str = pm->getInstrumentName();
	
	// ok, got the name now package it as a jstring
	if (str == 0) {
	    return env->NewStringUTF (" ");
	} else
	    return env->NewStringUTF(str);
    }
    /* -============================================================- */
    /*
     * Class:     PointingModel
     * Method:    getMissionName
     * Signature: ()Ljava/lang/String;
     */
    JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_PointingModel_getMissionName
	(JNIEnv *env, jobject obj)
    {
	PigPointingModel *pm;
	const char *str;
	
	/* the java object is a proxy to the c++ object.
	   resolveField find and retrieves the c++ pointer
	   that was stored in the java object.
	*/
	pm = (PigPointingModel*)resolveField (env, obj);
	if (pm == 0)
	    return 0; // is I don't have a pm then an exception occured
	
	str = pm->getMissionName();
	
	// ok, got the name now package it as a jstring
	if (str == 0) {
	    printf ("no mission name\n");
	    return env->NewStringUTF (" ");
	} else
	    return env->NewStringUTF(str);
    }
    /* -============================================================- */
    /*
     * Class:     PointingModel
     * Method:    getModelName
     * Signature: ()Ljava/lang/String;
     */
    JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_PointingModel_getModelName
        (JNIEnv *env, jobject obj)
    {
        PigPointingModel *pm;
        const char *str;

        /* the java object is a proxy to the c++ object.
           resolveField find and retrieves the c++ pointer
           that was stored in the java object.
        */
        pm = (PigPointingModel*)resolveField (env, obj);
        if (pm == 0)
            return 0; // is I don't have a pm then an exception occured

        str = pm->getModelName();

        // ok, got the name now package it as a jstring
        if (str == 0) {
            printf ("no pointing model name\n");
            return env->NewStringUTF (" ");
        } else
            return env->NewStringUTF(str);
    }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    getCameraOrientation
   * Signature: ()LPigPoint;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_PointingModel_getCameraOrientation
  (JNIEnv *env, jobject obj, jobject coordSys)
  {
    jclass           clazz=0;
    jclass           pointClazz=0;
    jmethodID        mid=0;
    jobject          newPoint=0;
    PigPointingModel *pm;
    PigCoordSystem   *cs;
    
    pm = (PigPointingModel*)resolveField(env, obj);
    if (pm == 0)
      return 0;

    cs = (PigCoordSystem*)resolveField(env, coordSys);
    if (cs == 0)
	return 0;

    if ((makeJObject(env, obj, "jpl/mipl/mica/pig/PigPoint", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;

    PigVector pv = pm->getCameraOrientation (cs);

    //printf("C orientation xyz=[ %lf , %lf , %lf ]\n",  pv.getX(), pv.getY(), pv.getZ());

    newPoint = env->NewObject(pointClazz, mid, pv.getX(), pv.getY(), pv.getZ());
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "getCameraOrientation: could not make a PigPoint");
      return 0;
    }
    return newPoint;
  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    getCameraPosition
   * Signature: ()LPigPoint;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_PointingModel_getCameraPosition
  (JNIEnv *env, jobject obj, jobject coordSys)
  {
    jclass           clazz=0;
    jclass           pointClazz=0;
    jmethodID        mid=0;
    jobject          newPoint=0;
    PigPointingModel *pm;
    PigCoordSystem   *cs;
    
    pm = (PigPointingModel*)resolveField(env, obj);
    if (pm == 0)
      return 0;

    cs = (PigCoordSystem*)resolveField (env, coordSys);
    if (cs == 0)
	return 0;

    if ((makeJObject(env, obj, "jpl/mipl/mica/pig/PigPoint", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;
    
    PigVector pv = pm->getCameraPosition (cs);
    
    newPoint = env->NewObject(pointClazz, mid, pv.getX(), pv.getY(), pv.getZ());
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "getCameraOrientation: could not make a PigPoint");
      return 0;
    }
    return newPoint;
  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    getCameraTwist
   * Signature: ()D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PointingModel_getCameraTwist
  (JNIEnv *env, jobject obj)
  {
    PigPointingModel *pm;
    
    pm = (PigPointingModel*)resolveField(env, obj);
    if (pm == 0)
      return 0;

    return pm->getCameraTwist();
  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    setCameraOrientation
   * Signature: (LPigPoint;)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PointingModel_setNtvCameraOrientation
  (JNIEnv *env, jobject obj, jobject vobj, jobject coordSys)
  {
    PigPointingModel *pm;
    PigVector        *pp;
    PigCoordSystem   *cs;

    pm = (PigPointingModel*)resolveField (env, obj);
    if (pm == 0)
      return ;

    pp = (PigVector*)resolveField(env, vobj);
    if (pp == 0)
      return;

    cs = (PigCoordSystem*)resolveField (env, coordSys);
    if (cs == 0)
	return ;

    pm->setCameraOrientation (*pp, cs);
    return ;
  }
    /* -============================================================- */  
  /*
   * Class:     PointingModel
   * Method:    setCameraPosition
   * Signature: (LPigPoint;)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PointingModel_setNtvCameraPosition
  (JNIEnv *env, jobject obj, jobject vobj, jobject coordSys)
  {
    PigPointingModel *pm;
    PigVector        *pp;
    PigCoordSystem   *cs;
    
    pm = (PigPointingModel*)resolveField (env, obj);
    if (pm == 0)
      return ;
    
    pp = (PigVector*)resolveField(env, vobj);
    if (pp == 0)
      return ;
    
    cs = (PigCoordSystem*)resolveField(env, coordSys);
    if (cs == 0)
	return ;

    pm->setCameraPosition (*pp, cs);
    return ;
  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    setCameraTwist
   * Signature: (D)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PointingModel_setNtvCameraTwist
  (JNIEnv *env, jobject obj, jdouble dd)
  {
    PigPointingModel *pm;
    PigVector       *pp;

    pm = (PigPointingModel*)resolveField (env, obj);
    if (pm == 0)
      return ;

    pm->setCameraTwist (dd);
    return ;

  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    pointCamera
   * Signature: (Ljava/lang/String;)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PointingModel_pointCamera
  (JNIEnv *env, jobject obj, jstring fileName)
  {
    PigPointingModel *pm;
    const char       *fname;
    jboolean         isCopy;

    pm = (PigPointingModel*)resolveField(env, obj);
    if (pm == 0)
      return ;
    
    fname = env->GetStringUTFChars(fileName, &isCopy);
    
    pm->pointCamera (fname);

    env->ReleaseStringUTFChars (fileName, fname);

    return ;
  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    getPointingParamCount
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_PointingModel_getPointingParamCount
  (JNIEnv *env, jobject obj)
  {
    PigPointingModel *pm;

    pm = (PigPointingModel*)resolveField (env, obj);
    if (pm == 0)
      return 0;

    return pm->getPointingParamCount ();
  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    getPointingParameters
   * Signature: ()[D
   */
  JNIEXPORT jdoubleArray JNICALL Java_jpl_mipl_mica_pig_PointingModel_getPointingParameters
  (JNIEnv *env, jobject obj)
  {
    jdouble          *ndp;
    jboolean          isCopy;
    PigPointingModel *pm;

    pm = (PigPointingModel*)resolveField (env, obj);
    if (pm == 0)
      return 0;

    int n = pm->getPointingParamCount ();

    jdoubleArray dd = env->NewDoubleArray (n);
    ndp = env->GetDoubleArrayElements (dd, &isCopy);
    pm->getPointingParameters (ndp, n);

    env->ReleaseDoubleArrayElements (dd, ndp, 0);

    return dd;
  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    setPointingParameters
   * Signature: ([DI)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PointingModel_setNtvPointingParameters
  (JNIEnv *env, jobject obj, jdoubleArray inArray, jint count)
  {
    PigPointingModel   *pm;
    jdouble            *dp;
    jboolean            isCopy;

    pm = (PigPointingModel*)resolveField(env, obj);
    if (pm == 0)
      return;

    dp = env->GetDoubleArrayElements (inArray, &isCopy);
    pm->setPointingParameters (dp, count);

    env->ReleaseDoubleArrayElements (inArray, dp, JNI_ABORT);

    return;
  }
    /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    getPointingErrorEstimate
   * Signature: ()[D
   */
  JNIEXPORT jdoubleArray JNICALL Java_jpl_mipl_mica_pig_PointingModel_getPointingErrorEstimate
  (JNIEnv *env, jobject obj)
  {
    jdouble          *ndp;
    jboolean          isCopy;
    PigPointingModel *pm;
    int               j;

    pm = (PigPointingModel*)resolveField (env, obj);
    if (pm == 0)
      return 0;
 
    int n = pm->getPointingParamCount ();

    jdoubleArray dd = env->NewDoubleArray (n);
    ndp = env->GetDoubleArrayElements (dd, &isCopy);
    pm->getPointingErrorEstimate(ndp, n);

    env->ReleaseDoubleArrayElements (dd, ndp, 0);

    return dd;
  }
  /* -============================================================- */
  /*
   * Class:     PointingModel
   * Method:    getPointingParamName
   * Signature: (I)Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_PointingModel_getPointingParamName
  (JNIEnv *env, jobject obj, jint index)
  {
    PigPointingModel *pm;
    const char *name;

    pm = (PigPointingModel*)resolveField(env, obj);
    if (pm == 0)
      return 0;

    name = pm->getPointingParamName (index);
    return env->NewStringUTF(name);
  }

}

