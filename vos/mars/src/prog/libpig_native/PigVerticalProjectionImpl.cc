#include <stdio.h>
#include "PigVerticalProjectionJ.h"
#include "PigCoordSystem.h"
#include "mars_support.h"

#include "jpl_mipl_mica_pig_CameraModel.h"
#include "jpl_mipl_mica_projection_VerticalProjection.h"

#include "mica_common.h"

extern "C" {
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    create
   * Signature: ()V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_create
  (JNIEnv *env, jobject obj)
  {
    jclass clazz = 0;
    jfieldID fid = 0;

    PigVerticalProjection *proj = new PigVerticalProjection ();
    if (proj == 0) {
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "initialize: could not create a PigVerticalProjection");
      return ;
    }

    /*
     * locate the 'cpp' field
     */
    if ((getField (env, obj, &clazz, &fid, "cpp", "J")) != 1) {
      return ;
    }

    env->SetLongField (obj, fid, (long)proj);

    return ;
  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getFromLookVector
   * Signature: (Ljpl/mipl/mica/pig/SurfaceModel;Ljpl/mipl/mica/pig/PigVector;Ljpl/mipl/mica/pig/PigPoint;Ljpl/mipl/mica/pig/CoordSystem;)Ljpl/mipl/mica/pig/PigPoint2D;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getFromLookVector
  (JNIEnv *env, jobject obj, jobject SurfMod, jobject lookVect, jobject CameraOrigin, jobject CoordSys)
  {
    PigVerticalProjection    *proj;
    PigSurfaceModel          *sm;
    PigVector                *look;
    PigVector                *co;
    PigCoordSystem           *cs;
    jclass                   clazz;
    jclass                   pointClazz;
    jmethodID                mid=0;
    jobject                  newPoint=0;
    PigPoint2D               *fp;

    proj = (PigVerticalProjection*)resolveField (env, obj);
    if (proj == 0)
      return 0;

    sm = (PigSurfaceModel*)resolveField (env, SurfMod);
    if (sm == 0)
      return 0;

    look = (PigVector*)resolveField (env, lookVect);
    if (look == 0)
      return 0;

    co = (PigVector*)resolveField (env, CameraOrigin);
    if (co == 0)
      return 0;

    cs = (PigCoordSystem*)resolveField (env, CoordSys);
    if (cs == 0)
      return 0;
    PigPoint *surf=new PigPoint(0.0,0.0,0.0);
    
    int hits=sm->intersectRay(*co, *look, *surf);
    int infinity = (hits <= 0);
    
    // +X is up, +Y is right
    // C++ code returned 1 on error.  Up to the calling function to determine
    // in Java (error if ray doesn't hit plane)
    fp = new PigPoint2D(proj->_nlo/2 - (surf->getX() * proj->_vert_scale),
			(surf->getY() * proj->_vert_scale) * cs->getAzimuthDirection() + proj->_nso/2);

    if (fp == 0) {
	env->ExceptionDescribe();
	clazz = env->FindClass ("java/lang/InstantiationException");
	env->ThrowNew (clazz, "getFromLookVector: could not make a C++ PigPoint2D: out of memory!");
	delete surf;
	return 0;
    }

    if ((makeJObject (env, obj, "jpl/mipl/mica/pig/PigPoint2D", "(DD)V", &pointClazz, &mid)) != 1) {
      delete surf;
      delete fp;
      return 0;
    }

    newPoint = env->NewObject(pointClazz, mid, fp->getX(), fp->getY());
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "getFromLookVector: could not make a jpl.mipl.mica.pig.PigPoint2D");
      delete surf;
      delete fp;
      return 0;
    }
    delete surf;
    delete fp;
  
    return newPoint;
  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getHeight
   * Signature: ()D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getHeight
  (JNIEnv *env, jobject obj)
  {
    PigVerticalProjection *proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return 0;

    return proj->getHeight();
  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getLookVector
   * Signature: (DDLjpl/mipl/mica/pig/CoordSystem;)Ljpl/mipl/mica/pig/PigVector;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getLookVector
  (JNIEnv *env, jobject obj, jdouble sample, jdouble line, jobject CoordSys)
  {
    PigVerticalProjection *proj;
    PigCoordSystem        *cs;
    PigVector             p;
    jclass                   clazz;
    jclass                   pointClazz;
    jmethodID                mid=0;
    jobject                  newPoint=0;

    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return 0;

    cs = (PigCoordSystem*)resolveField (env, CoordSys);
    if (cs == 0)
      return 0;
  
    double x_ctr = (proj->_nlo/2 - sample) / proj->_vert_scale;
    double y_ctr = cs->getAzimuthDirection() * (line - proj->_nso/2) / proj->_vert_scale;
    
    if ((makeJObject (env, obj, "jpl/mipl/mica/pig/PigPoint", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;

    newPoint = env->NewObject(pointClazz, mid, x_ctr, y_ctr, 0.0);
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "getLookVector: could not make a jpl.mipl.mica.pig.PigPoint");
      return 0;
    }

    return newPoint;
  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getMaxX
   * Signature: ()D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getMaxX
  (JNIEnv *env, jobject obj)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return 0;
    
    return proj->getMaxX();
  } 
  /* -=================================================================- */  
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getMaxY
   * Signature: ()D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getMaxY
  (JNIEnv *env, jobject obj)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return 0;
    
    return proj->getMaxY();
  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getOrigin
   * Signature: ()Ljpl/mipl/mica/pig/PigPoint;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getOrigin
  (JNIEnv *env, jobject obj)
  {
    PigVerticalProjection *proj;
    PigVector             *p;
    jclass                clazz;
    jclass                pointClazz;
    jmethodID             mid=0;
    jobject               newPoint=0;

    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return 0;

    p = proj->getOrigin ();
    if ((makeJObject (env, obj, "jpl/mipl/mica/pig/PigPoint", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;

    newPoint = env->NewObject(pointClazz, mid, p->getX(), p->getY(), p->getZ());
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "getOrigin: could not make a jpl.mipl.mica.pig.PigPoint");
      return 0;
    }

    return newPoint;

  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getScale
   * Signature: ()D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getScale
  (JNIEnv *env, jobject obj)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return 0;
    
    return proj->getScale();
  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getSurfacePoint
   * Signature: (Ljpl/mipl/mica/pig/SurfaceModel;Ljpl/mipl/mica/pig/PigVector;Ljpl/mipl/mica/pig/PigPoint;)Ljpl/mipl/mica/pig/PigPoint;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getSurfacePoint
  (JNIEnv *env, jobject obj, jobject SurfMod, jobject LookVect, jobject SurfVect)
  {
    jclass                clazz;
    jclass                pointClazz=0;
    jmethodID             mid=0;
    jobject               newPoint=0;
    PigPoint              *p;
    PigVerticalProjection *proj;

    proj = (PigVerticalProjection*)resolveField (env, obj);
    if (proj == 0)
      return 0;

    PigSurfaceModel *sm = (PigSurfaceModel*)resolveField (env, SurfMod);
    if (sm == 0)
      return 0;

    PigVector *look = (PigVector*)resolveField (env, LookVect);
    if (look == 0)
      return 0;

    PigPoint *surf = (PigPoint*)resolveField (env, SurfVect);
    if (surf == 0)
      return 0;

    p = proj->getSurfacePoint (*sm, *look, *surf);

    if ((makeJObject (env, obj, "jpl/mipl/mica/pig/PigPoint", "(DDD)V", &pointClazz, &mid)) != 1)
      return 0;

    newPoint = env->NewObject(pointClazz, mid, p->getX(), p->getY(), p->getZ());
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "getLookVector: could not make a jpl.mipl.mica.pig.PigPoint");
      return 0;
    }

    return newPoint;
 
  }
  /* -=================================================================- */
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getVertScale
   * Signature: ()D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getVertScale
  (JNIEnv *env, jobject obj)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return 0;
    
    return proj->getVertScale();
  }

  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    getWidth
   * Signature: ()D
   */
  JNIEXPORT jdouble JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_getWidth
  (JNIEnv *env, jobject obj)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return 0;
    
    return proj->getWidth();
  }

  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    setMaxX
   * Signature: (D)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_setMaxX
  (JNIEnv *env, jobject obj, jdouble xx)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return ;
    
    proj->setMaxX(xx);
    return ;
  }

  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    setMaxY
   * Signature: (D)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_setMaxY
  (JNIEnv *env, jobject obj, jdouble yy)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return ;
    
    proj->setMaxY(yy);
    return ;
  }

  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    setOrigin
   * Signature: (DDD)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_setNtvOrigin
  (JNIEnv *env, jobject obj, jdouble xx, jdouble yy, jdouble zz)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return ;
    
    PigVector *p = new PigVector (xx, yy, zz);
    proj->setOrigin (p);
    return ;
  }
  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    setScale
   * Signature: (D)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_setNtvScale
  (JNIEnv *env, jobject obj, jdouble sc)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return ;
    
    proj->setScale (sc);
    return ;
  }

  /*
   * Class:     jpl_mipl_mica_projection_VerticalProjection
   * Method:    setVertScale
   * Signature: (D)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_projection_VerticalProjection_setVertScale
  (JNIEnv *env, jobject obj, jdouble vs)
  {
    PigVerticalProjection *proj;
    proj = (PigVerticalProjection*)resolveField(env, obj);
    if (proj == 0)
      return ;
    
    proj->setVertScale (vs);
    return ;
  }

}
