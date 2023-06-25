/*
  CameraModel java proxy implementation

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
#include "PigCoordSystem.h"
#include "mars_support.h"

#include "jpl_mipl_mica_pig_CameraModel.h"

#include "mica_common.h"

extern "C" {
    /* -=================================================================- */
/*
 * Class:     CameraModel
 * Method:    initialize
 * Signature: (Ljava/lang/String;Ljava/lang/String;)I
 */
  
 JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_CameraModel_Initialize
  (JNIEnv *env, jobject obj, jstring filename, jstring special)
  {
    const char * fname;
    const char * spec;
    jboolean isCopy;
    PigCameraModel *cm;
    jclass clazz = 0;
    jfieldID fid = 0;


    /*
     * convert the java string to a c++ string
     * then get the cameramodel for this object.
     */
    fname = env->GetStringUTFChars(filename, &isCopy);
    //printf ("found my way to pig camera >%s<\n", fname);
    spec = 0; 

    //Register param getter function
//    PigModelBase::setDefaultParamFunction(PigSimpleParamGetter, NULL);

    cm = PigCameraModel::create(fname, NULL);

    if (cm == 0) {
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "initialize: could not create a PigCameraModel");
      return 0;
    }
    /*
     * now locate the field called "cpp", it's a java long
     */
    if ((getField (env, obj, &clazz, &fid, "cpp", "J")) != 1) {
      return 0;
    }

    env->SetLongField (obj, fid, (long)cm);

    env->ReleaseStringUTFChars (filename, fname);
    env->ReleaseStringUTFChars (special, spec);

    return 1;
  }
    /* -=================================================================- */
/*
 * Class:     CameraModel
 * Method:    getMissionName
 * Signature: ()Ljava/lang/String;
 *
 * GetMissionName - return the mission name as a java string
 *
 *  WARNING: if the cm return 0 this returns " " 
 */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_CameraModel_getMissionName
  (JNIEnv *env, jobject obj)
  {
    PigCameraModel *cm;
    const char *str;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    cm = (PigCameraModel*)resolveField (env, obj);
    if (cm == 0)
      return 0; // is I don't have a cm then an exception occured

    str = cm->getMissionName();

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return env->NewStringUTF (" ");
    } else
      return env->NewStringUTF(str);
  }
    /* -=================================================================- */
  /*
   * Class:     CameraModel
   * Method:    getInstrumentName
   * Signature: ()Ljava/lang/String;
   *
   * getINstrumentName - same as getMissionName but return the
   *   name of the instrumnent.
   *
   *  WARNING: if the cm return 0 this returns " " 
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_CameraModel_getInstrumentName
  (JNIEnv *env, jobject obj)
  {
    jclass clazz = 0;
    jfieldID fid = 0;
    jlong lp;
    PigCameraModel *cm;
    const char *str;
    
    cm = (PigCameraModel*)resolveField(env, obj);
    if (cm == 0)
      return 0; // no cm - return to let java process the exception

    str = cm->getInstrumentName();
    if (str == 0) {
      return env->NewStringUTF (" ");
    } else
      return env->NewStringUTF(str);

  }
    /* -=================================================================- */
  /*
   * Class:     CameraModel
   * Method:    getCameraPosition
   * Signature: ()LPigPoint;
   *
   * getCameraPosition - return a position vector 
   *
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_CameraModel_getCameraPosition
  (JNIEnv *env, jobject obj)
  {
    jobject         newPoint=0;
    jclass         pointClazz=0, clazz=0;
    jmethodID      mid=0;
    PigCameraModel *cm;
    
    cm = (PigCameraModel*)resolveField(env, obj);
    if (cm == 0)
      return 0; // java will process the exception

    PigVector pv = cm->getCameraPosition ();

    if ((makeJObject(env, obj, "jpl/mipl/mica/pig/PigPoint", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;

    newPoint = env->NewObject(pointClazz, mid, pv.getX(), pv.getY(), pv.getZ());
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "makePoint: could not make a jpl.mipl.mica.pig.PigPoint");
      return 0;
    }
    return newPoint;
  }
    /* -=================================================================- */
  /*
   * Class:     CameraModel
   * Method:    getCameraOrientation
   * Signature: ()LPigPoint;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_CameraModel_getCameraOrientation
  (JNIEnv *env, jobject obj)
  {
    jclass          clazz;
    jclass          pointClazz=0;
    jmethodID       mid=0;
    jobject         newPoint=0;
    PigCameraModel *cm;
    const char     *str;

    cm = (PigCameraModel*)resolveField(env, obj);
    if (cm == 0)
      return 0;

    if ((makeJObject (env, obj, "jpl/mipl/mica/pig/PigPoint", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;

    PigVector pv = cm->getCameraOrientation ();

    newPoint = env->NewObject(pointClazz, mid, pv.getX(), pv.getY(), pv.getZ());
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "getCameraOrientation: could not make a jpl.mipl.mica.pig.PigPoint");
      return 0;
    }

    return newPoint;

  }
  /* -=======================================================- */
  /*
   * Class:     CameraModel
   * Method:    getCameraTwist
   * Signature: ()D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_CameraModel_getCameraTwist
  (JNIEnv *env, jobject obj)
  {
    PigCameraModel *cm;
    
    cm = (PigCameraModel*)resolveField(env, obj);
    if (cm == 0)
      return 0;

    return cm->getCameraTwist();
   
  }
  /* -============================================================- */
  /*
   * Class:     CameraModel
   * Method:    getModelName
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_CameraModel_getModelName
  (JNIEnv *env, jobject obj)
  {
    PigCameraModel *cm;
    const char *str;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    cm = (PigCameraModel*)resolveField (env, obj);
    if (cm == 0)
      return 0; // is I don't have a fm then an exception occured

    str = cm->getModelName();

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return env->NewStringUTF (" ");
    } else
      return env->NewStringUTF(str);
  }
  /* -============================================================- */
  /*
   * Class:     CameraModel
   * Method:    writeToString
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_CameraModel_writeToString
  (JNIEnv *env, jobject obj)
  {
    PigCameraModel *cm;
    char str[350];
    int status;

    /* the java object is a proxy to the c++ object.
       resolveField find and retrieves the c++ pointer
       that was stored in the java object.
    */
    cm = (PigCameraModel*)resolveField (env, obj);
    if (cm == 0)
      return 0; // is I don't have a fm then an exception occured

    status = cm->writeToString(str, sizeof(str));

    // ok, got the name now package it as a jstring
    if (str == 0) {
      return env->NewStringUTF (" ");
    } else
      return env->NewStringUTF(str);
  }

  /* -============================================================- */
  /*
   * Class:     CameraModel
   * Method:    setNtvCameraPosition
   * Signature: (LPigPoint;)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_CameraModel_setNtvCameraPosition
  (JNIEnv *env, jobject obj, jobject pointObject, jobject coordSys)
  {
    PigCameraModel  *cm;
    PigVector       *pp;
    PigCoordSystem  *cs;

    cm = (PigCameraModel*)resolveField (env, obj);
    if (cm == 0)
      return ;

    pp = (PigVector*)resolveField(env, pointObject);
    if (pp == 0)
      return ;

    cs = (PigCoordSystem*)resolveField (env, coordSys);
    if (cs == 0)
      return ;

    cm->setCameraPosition (*pp, cs);
    return ;
  
  }

  /* -====================================================================- */
  /*
   * Class:     CameraModel
   * Method:    setNtvCameraPosition
   * Signature: (LPigPoint;)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_CameraModel_setNtvCameraOrientation
  (JNIEnv *env, jobject obj, jobject pobj, jobject coordSys)
  {
    PigCameraModel  *cm;
    PigVector       *pp;
    PigCoordSystem  *cs;

    cm = (PigCameraModel*)resolveField (env, obj);
    if (cm == 0)
      return ;

    pp = (PigVector*)resolveField(env, pobj);
    if (pp == 0)
      return;

    cs = (PigCoordSystem*)resolveField (env, coordSys);
    if (cs == 0)
      return ;

    cm->setCameraOrientation (*pp, cs);
    return ;
  }
  /* -====================================================================- */
  /*
   * Class:     CameraModel
   * Method:    setNtvCameraTwist
   * Signature: (D)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_CameraModel_setNtvCameraTwist
  (JNIEnv *env, jobject obj, jdouble dd)
  {
    PigCameraModel *cm;
    PigVector       *pp;

    cm = (PigCameraModel*)resolveField (env, obj);
    if (cm == 0)
      return ;

    cm->setCameraTwist (dd);
    return ;

  }
  /* -====================================================================- */
  /*
   * Class:     CameraModel
   * Method:    LStoLookVector
   * Signature: (IILPigPoint;)LPigPoint;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_CameraModel_LStoLookVector
  (JNIEnv *env, jobject obj, jdouble line, jdouble sample, jobject jporigin, jobject coordSys)
  {
    PigCameraModel  *cm;
    PigVector       *origin;
    PigVector       *look_direction = new PigVector();
    jclass          clazz;
    jmethodID       mid;
    jclass          pointClazz;
    jobject         newPoint;
    PigCoordSystem  *cs;


    cm = (PigCameraModel*)resolveField(env,obj);
    if (cm == 0)
      return 0;

    origin = (PigVector*)resolveField (env,jporigin);
    if (origin == 0)
      return 0;

    cs = (PigCoordSystem*)resolveField (env, coordSys);
    if (cs == 0)
      return 0;

    //printf ("1< %f %f %f >\n", origin->getX(), origin->getY(), origin->getZ());
    cm->LStoLookVector(line, sample, *origin, *look_direction, cs);
    //printf ("2< %f %f %f >\n", origin->getX(),origin->getY(), origin->getZ());

    if ((makeJObject(env, obj, "jpl/mipl/mica/pig/PigPoint", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;

    newPoint = env->NewObject(pointClazz, mid,
			      look_direction->getX(), 
			      look_direction->getY(), 
			      look_direction->getZ());
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "LStoLookVector: could not make a jpl.mipl.mica.pig.PigPoint");
      return 0;
    }
    return newPoint;

  }
  /* -====================================================================- */
  /*
   * Class:     CameraModel
   * Method:    XYZtoLS
   * Signature: (LPigPoint;)Ljava/awt/Point;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_CameraModel_XYZtoLS
  (JNIEnv *env, jobject obj, jobject vec, jobject coordSys)
  {
    PigCameraModel *cm;
    PigVector *v;
    jclass clazz=0, pointClazz=0;
    jfieldID fid=0;
    jmethodID mid=0;
    jdouble line, sample;
    jobject newPoint=0;
    PigCoordSystem *cs;

    cm = (PigCameraModel*)resolveField(env,obj);
    if (cm == 0)
      return 0;

    v = (PigVector*)resolveField (env,vec);
    if (v == 0)
      return 0;

    cs = (PigCoordSystem*)resolveField (env, coordSys);
    if (cs == 0)
      return 0;

    if ((getField (env, vec, &clazz, &fid, "infinity", "Z")) != 1)
      return 0;

    //printf("Infinity=%i \n",env->GetBooleanField(vec, fid));
    
    cm->XYZtoLS (*v, env->GetBooleanField(vec, fid), &line, &sample, cs);

    if ((makeJObject(env, obj, "jpl/mipl/mica/pig/PigPoint2D", "(DD)V", &pointClazz, &mid)) != 1)
      return 0;

    //printf ("CameraModel: XYZtoLineS: (%d %d)\n", jsample, jline);
    
    newPoint = env->NewObject(pointClazz, mid, sample, line);
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "XYZtoLS: could not make a Point object");
      return 0;
    }
    return newPoint;
  }
  /* -=======================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_CameraModel
   * Method:    getPixelAngle
   * Signature: (I)D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_CameraModel_getPixelAngle
  (JNIEnv *env, jobject obj, jint which)
  {
    PigCameraModel *cm;
    jdouble ret;

    cm = (PigCameraModel*)resolveField(env, obj);
    if (cm == 0)
      return 0;

    ret =  cm->getPixelAngle(which);
    //printf ("-------------i(%d) Pixel Angle %f\n", which, ret);
    return ret;
  } 
  /* -=======================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_CameraModel
   * Method:    getCameraCenter
   * Signature: ()Ljava/awt/Point;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_CameraModel_getCameraCenter
  (JNIEnv *env, jobject obj)
  {
    PigCameraModel *cm;
    jclass clazz=0,pointClazz=0;
    jmethodID mid=0;
    jdouble line=0, sample=0;
    jobject newPoint=0;

    cm = (PigCameraModel*)resolveField(env,obj);
    if (cm == 0)
      return 0;

    cm->getCameraCenter(line, sample);

    if ((makeJObject(env, obj, "jpl/mipl/mica/pig/PigPoint2D", "(DD)V", &pointClazz, &mid)) != 1)
      return 0;
    
    newPoint = env->NewObject(pointClazz, mid, sample, line);
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "getCameraCenter: could not make a Point object");
      return 0;
    }
    return newPoint;
  }
  
}

