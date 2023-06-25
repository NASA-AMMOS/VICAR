#include <stdio.h>

#include "PigCoordSystem.h"
#include "mars_support.h"

#include "jpl_mipl_mica_pig_CoordSystem.h"

#include "mica_common.h"

extern "C" {
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_CoordSystem
   * Method:    constructVector
   * Signature: (DD)Ljpl/mipl/mica/pig/PigVector;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_CoordSystem_constructVector
  (JNIEnv *env, jobject obj, jdouble az, jdouble el)
  {
    PigCoordSystem *cs;
    PigVector      v;
    PigCameraModel *cm;
    jclass clazz=0, pointClazz=0;
    jfieldID fid=0;
    jmethodID mid=0;
    jdouble line, sample;
    jobject newPoint=0;

    cs = (PigCoordSystem*)resolveField (env, obj);
    if (cs == 0)
      return 0;

    v = cs->constructVector (az, el);

    if ((makeJObject(env, obj, "jpl/mipl/mica/pig/PigVector", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;

    newPoint = env->NewObject(pointClazz, mid, v.getX(), v.getY(), v.getZ());

    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "LStoLookVector: could not make a jpl.mipl.mica.pig.PigVector");
      return 0;
    }
    return newPoint;
  }
  /* -=================================================================- */  
  /*
   * Class:     jpl_mipl_mica_pig_CoordSystem
   * Method:    getAz
   * Signature: (Ljpl/mipl/mica/pig/PigVector;)D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_CoordSystem_getAz
  (JNIEnv *env, jobject obj, jobject vec)
  {
    PigCoordSystem *cs;
    PigVector      *pv;

    cs = (PigCoordSystem*)resolveField (env, obj);
    if (cs == 0)
      return 0;

    pv = (PigVector*)resolveField (env, vec);
    if (pv == 0)
      return 0;

    return cs->getAz (*pv);
  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_CoordSystem
   * Method:    getAzimuthDirection
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_CoordSystem_getAzimuthDirection
  (JNIEnv *env, jobject obj)
  {
    PigCoordSystem *cs;

    cs = (PigCoordSystem*)resolveField (env, obj);
    if (cs == 0)
      return 0;

    return cs->getAzimuthDirection ();
  }
  /* -=================================================================- */  
  /*
   * Class:     jpl_mipl_mica_pig_CoordSystem
   * Method:    getEl
   * Signature: (Ljpl/mipl/mica/pig/PigVector;)D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_CoordSystem_getEl
  (JNIEnv *env, jobject obj, jobject vec)
  {
    PigCoordSystem *cs;
    PigVector      *pv;

    cs = (PigCoordSystem*)resolveField (env, obj);
    if (cs == 0)
      return 0;

    pv = (PigVector*)resolveField (env, vec);
    if (pv == 0)
      return 0;

    return cs->getEl (*pv);
  }
  /* -=================================================================- */  
  /*
   * Class:     jpl_mipl_mica_pig_CoordSystem
   * Method:    getElevationDirection
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_CoordSystem_getElevationDirection
  (JNIEnv *env, jobject obj)
  {
    PigCoordSystem *cs;

    cs = (PigCoordSystem*)resolveField (env, obj);
    if (cs == 0)
      return 0;

    return cs->getElevationDirection ();
  }
  /* -=================================================================- */  
  /*
   * Class:     jpl_mipl_mica_pig_CoordSystem
   * Method:    initialize
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_CoordSystem_initialize
  (JNIEnv *env, jobject obj)
  {
    PigCoordSystem *cs;

    cs = (PigCoordSystem*)resolveField (env, obj);
    if (cs == 0)
      return 0 ;

    return 1;
  }
}
