/*
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
#include "jni.h"
#include "jpl_mipl_mica_pig_PigPoint.h"
#include "PigVector.h"
#include "mica_common.h"

extern "C" {

  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PigPoint_getX
  (JNIEnv *env, jobject obj)
  {
    PigPoint *fp;
    
    fp = (PigPoint*)resolveField (env, obj);
    if (fp == 0)
	return 0;

    return fp->getX();
  }
  
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PigPoint_getY
  (JNIEnv *env, jobject obj)
  {
    PigPoint *fp;
    
    fp = (PigPoint*)resolveField (env, obj);
    if (fp == 0)
	return 0;
    
    return fp->getY();
  }
  
  
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PigPoint_getZ
  (JNIEnv *env, jobject obj)
  {
    PigPoint *fp;
    
    fp = (PigPoint*)resolveField (env, obj);
    if (fp == 0)
	return 0;

    return fp->getZ();
  }

  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PigPoint_setXNtv
  (JNIEnv *env, jobject obj, jdouble val)
  {
    PigPoint *fp;
   
    fp = (PigPoint*)resolveField (env, obj);
    if (fp == 0)
	return 0;
    
    fp->setX(val);
    return val;
  }
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PigPoint_setYNtv
  (JNIEnv *env, jobject obj, jdouble val)
  {
    PigPoint *fp;
   
    fp = (PigPoint*)resolveField (env, obj);
    if (fp == 0)
	return 0;
    
    fp->setY(val);
    return val;
  }
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PigPoint_setZNtv
  (JNIEnv *env, jobject obj, jdouble val)
  {
    PigPoint *fp;
   
    fp = (PigPoint*)resolveField (env, obj);
    if (fp == 0)
	return 0;
    
    fp->setZ(val);
    return val;
  }

  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PigPoint_dot
  (JNIEnv *env, jobject obj, jobject vec)
  {
    PigPoint *fpO, *fpS;
    
    fpO = (PigPoint*)resolveField (env, obj);
    if (fpO == 0)
	return 0;

    fpS = (PigPoint*)resolveField (env, vec);
    if (fpS == 0)
	return 0;
    
    //    printf ("%f %f %f\n%f %f %f\n", fpO->getX(), fpO->getY(), fpO->getZ(), fpS->getX(), fpS->getY(), fpS->getZ());
    return (*fpO % *fpS);
  }
  
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PigPoint_subtractNtv
  (JNIEnv *env, jobject obj, jobject vec)
  {
    PigVector *fpO, *fpS;

    fpO = (PigVector*)resolveField (env, obj);
    if (fpO == 0)
	return;

    fpS = (PigVector*)resolveField (env, vec);
    if (fpS == 0)
	return;

    *fpO -= *fpS;
  }
  
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PigPoint_addNtv
  (JNIEnv *env, jobject obj, jobject vec)
  {
    PigPoint *fpO, *fpS;
    fpO = (PigPoint*)resolveField (env, obj);
    if (fpO == 0)
	return ;

    fpS = (PigPoint*)resolveField (env, vec);
    if (fpS == 0)
	return ;

    *fpO += *fpS;
    
  }
  
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PigPoint_normalizeNtv
  (JNIEnv *env, jobject obj)
  {
    PigPoint *fp;
    
    fp = (PigPoint*)resolveField (env, obj);
    fp->normalize();
  }
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_pig_PigPoint_magnitude
  (JNIEnv *env, jobject obj)
  {
    PigPoint *fp;
    
    fp = (PigPoint*)resolveField (env, obj);
    return fp->magnitude();
  }
  
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_PigPoint_initialize
  (JNIEnv *env, jobject obj, jdouble xx, jdouble yy, jdouble zz, jboolean bb)
  {
    PigPoint *fp ;
    jclass clazz = 0;
    jfieldID fid = 0;

    fp = new PigPoint(xx, yy, zz);
    //printf ("value here ========================> (%f %f %f)\n", xx, yy, zz);

    int r = getField (env, obj, &clazz, &fid, "cpp", "J");
    if (r == 0)
      return ;
    
    env->SetLongField (obj, fid, (long)fp);
    
  }
}

