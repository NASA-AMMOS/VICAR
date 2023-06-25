/**
 *
 * SurfaceModel java proxy implementation
 *
 * Each of the methods in this file implement behaviour on a java object.
 * The initialize method found further on will create a c++ object and 
 * place the address to this object into the java long variable named cpp.
 *
 * The SurfaceModel is a little different then the other Pig proxy models.
 * The SurfaceModel has a CoordSystem object that is init'ed when the 
 * SurfaceModel is created. The initialize method in this file will stuff
 * the address of surfaceModel::getCoordSystem() into a java coordSystem 
 * variable.
 *
 * Each function will retrieve the pointer to the c++ object from the
 * java variable (cpp) via a standard subroutine resolveField (). The file
 * mica_common.cc defines a number of handy subroutines.
 * 
 * In all cases if a call to a routine in mica_common.cc returns zero an 
 * exceptional case has occured. Return to the java virtual machine
 * (your caller) and let it handle things.
 *
 */


#include <stdio.h>
#include "PigSurfaceModel.h"
#include "PigCoordSystem.h"
#include "PigMICAParamsJ.h"

#include "jpl_mipl_mica_pig_SurfaceModel.h"
#include "mica_common.h"



extern "C" {

    /* -============================================================- */
  /*
   * Class:     SurfaceModel
   * Method:    initialize
   * Signature: (Ljava/lang/String;)I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_initialize
  (JNIEnv *env, jobject obj, jstring filename, jobject coors)
  {
    jboolean         isCopy;
    const char       *fname;
    PigSurfaceModel  *sm;
    PigCoordSystem   *cs, *newcs;
    jclass           clazz=0;
    jfieldID         fid=0;

    fname = env->GetStringUTFChars (filename, &isCopy);

    sm = PigSurfaceModel::create (fname);
    if (sm == 0)
      return 0;

    if ((getField(env, obj, &clazz, &fid, "cpp", "J")) != 1)
      return 0;
    
    env->SetLongField (obj, fid, (long)sm);
    env->ReleaseStringUTFChars (filename, fname);
    /*
      at this point the surface model has been stuffed and init'ed.

      the surfaceModel coordinate system still needs to be 
      stuffed, get the coordSys for this surfacemodel filename combo
      NOTE: the coordSys is passed in!
    */
    newcs = sm->getCoordSystem();

    if ((getField(env, coors, &clazz, &fid, "cpp", "J")) != 1)
      return 0;

    env->SetLongField (coors, fid, (long)newcs);

    return 1;
  }

	/* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    setParamGetter
   * Signature: ()I
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_setParamGetter
  (JNIEnv *env, jobject obj)
	{
		PigMICAParams	 *params;
		jclass           clazz=0;
		jfieldID         fid=0;

		params = new PigMICAParams();
		if ((getField(env, obj, &clazz, &fid, "paramp", "J")) != 1)
			return 0;
		
		env->SetLongField (obj, fid, (long)
								PigMICAParams::PigMICAParamGetter);

		//Register param getter function
//		printf( "Setting paramgetter\n");
//		PigModelBase::setDefaultParamFunction(PigSimpleParamGetter, NULL);

		PigModelBase::setDefaultParamFunction(
							PigMICAParams::PigMICAParamGetter, NULL);
		int count;
		char surface[20];
		double vector[3];
  
//		PigModelBase::getStaticParam((char *)"GROUND", (void *)vector, 
//									&count, 3, 0);
  
		return 1;
	}

    /* -============================================================- */
  /*
   * Class:     SurfaceModel
   * Method:    intersectRay
   * Signature: (LPigPoint;LPigPoint;)LPigPoint;
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_intersectRay
  (JNIEnv *env, jobject obj, jobject origin, jobject lookV, jobject result)
  {    
    jobject         newPoint=0;
    jclass          pointClazz=0, clazz=0;
    jfieldID        fid=0;
    PigSurfaceModel *sm;
    const PigPoint  *org;
    const PigVector *look;
    PigPoint        *res;

    sm = (PigSurfaceModel*)resolveField(env, obj);
    if (sm == 0)
      return 0;

    org = (PigPoint*)resolveField(env, origin);
    if (org == 0)
      return 0;

    // printf ("org(%f, %f, %f)\n",  org->getX(), org->getY(), org->getZ());

    look = (PigPoint*)resolveField(env, lookV);
    if (look == 0)
      return 0;

    // printf ("look(%f, %f, %f)\n", look->getX(), look->getY(), look->getZ());

    res = (PigPoint *)resolveField (env, result);

    int r = sm->intersectRay (*org, *look, *res);

    if ((getField(env, result, &clazz, &fid, "x", "D")) != 1)
	return 0;

    env->SetDoubleField (result, fid, res->getX());

    if ((getField(env, result, &clazz, &fid, "y", "D")) != 1)
	return 0;

    env->SetDoubleField (result, fid, res->getY());

    if ((getField(env, result, &clazz, &fid, "z", "D")) != 1)
	return 0;

    env->SetDoubleField (result, fid, res->getZ());

    //printf ("Surface model return code = %d\n", r);


    //printf ("np(%f, %f, %f)\n", res->getX(), res->getY(), res->getZ());

    return r;
  }  
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    getMissionName
   * Signature: ()Ljava/lang/String;
   */
  JNIEXPORT jstring JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_getMissionName
      (JNIEnv *env, jobject obj)
  {
      PigSurfaceModel *sm;
      const char *str;
	
      sm = (PigSurfaceModel*)resolveField(env, obj);
      if (sm == 0)
	  return 0;

      str = sm->getMissionName();
      
      // ok, got the name now package it as a jstring
      if (str == 0) {
	  return env->NewStringUTF ("nothing ");
      } else
	  return env->NewStringUTF(str);
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    getRay
   * Signature: (Ljpl/mipl/mica/pig/PigPoint;Ljpl/mipl/mica/pig/PigPoint;Ljpl/mipl/mica/pig/PigPoint;)V
   */
  JNIEXPORT jint JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_getRay
  (JNIEnv *env, jobject obj, jobject origin, jobject surfV, jobject result)
  {
    jobject         newPoint=0;
    jclass          pointClazz=0, clazz=0;
    jfieldID        fid=0;
    PigSurfaceModel *sm;
    const PigPoint  *org;
    const PigVector *surf;
    PigPoint        *res;
    int             infinity_flag;

    sm = (PigSurfaceModel*)resolveField(env, obj);
    if (sm == 0)
      return 0;

    org = (PigPoint*)resolveField(env, origin);
    if (org == 0)
      return 0;

    //printf ("org(%f, %f, %f)\n",  org->getX(), org->getY(), org->getZ());

    surf = (PigPoint*)resolveField(env, surfV);
    if (surf == 0)
      return 0;

    //printf ("look(%f, %f, %f)\n", look->getX(), look->getY(), look->getZ());

    res = (PigPoint *)resolveField (env, result);


    if ((getField (env, surfV, &clazz, &fid, "infinity", "Z")) != 1)
      return 0;

    infinity_flag=env->GetBooleanField(surfV, fid);


    // ************ CALL THE P.I.G. FUNCTION ***************
    sm->getRay (*org, *surf, infinity_flag, *res);

    if ((getField(env, result, &clazz, &fid, "x", "D")) != 1)
	return 0;

    env->SetDoubleField (result, fid, res->getX());

    if ((getField(env, result, &clazz, &fid, "y", "D")) != 1)
	return 0;

    env->SetDoubleField (result, fid, res->getY());

    if ((getField(env, result, &clazz, &fid, "z", "D")) != 1)
	return 0;

    env->SetDoubleField (result, fid, res->getZ());

    //printf ("Surface model return code = %d\n", r);


    //printf ("np(%f, %f, %f)\n", res->getX(), res->getY(), res->getZ());
    
    return 0;
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    setCoordSystem
   * Signature: (Ljpl/mipl/mica/pig/CoordSystem;)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_setCoordSystem
  (JNIEnv *env, jobject obj, jobject coorSys)
  {
    PigSurfaceModel *sm;
    PigCoordSystem  *cs;

    sm = (PigSurfaceModel*)resolveField (env, obj);
    if (sm == 0)
      return ;

    cs = (PigCoordSystem*)resolveField(env, coorSys);
    if (cs == 0)
      return;

    sm->setCoordSystem (cs);
  }
  /* -============================================================- */
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    getCoordSystem
   * Signature: ()Ljpl/mipl/mica/pig/CoordSystem;
   */
  JNIEXPORT jobject JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_getCoordSystem
  (JNIEnv *env, jobject obj)
  {
    PigSurfaceModel *sm;
    PigCoordSystem  *cs;
    jclass clazz=0,pointClazz=0;
    jmethodID mid=0;
    jfieldID fid=0;
    jobject newPoint=0;
    
    sm = (PigSurfaceModel*)resolveField (env, obj);
    if (sm == 0)
      return 0;

    cs = sm->getCoordSystem ();
    if ((makeJObject(env, obj, "jpl/mipl/mica/pig/CoordSystem", "()V", &pointClazz, &mid)) != 1)
      return 0;

    newPoint = env->NewObject(pointClazz, mid);
    if (newPoint == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/InstantiationException");
      env->ThrowNew (clazz, "SurfaceModel::getCoordSystem: could not make a CoordSystem object");
      return 0;
    }

    if ((getField(env, newPoint, &clazz, &fid, "cpp", "J")) != 1)
      return 0;
    
    env->SetLongField (newPoint, fid, (long)cs);

    return newPoint;
  }
  
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    setGround
   * Signature: ([D)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_setGround
	    (JNIEnv *env, jobject obj, jdoubleArray jGround )
	{
		jclass           clazz=0;
		jfieldID         fid=0;
		PigMICAParams	 *params;
		jdouble			 *myGround;
		jdoubleArray	 javaGround;

		if ((getField(env, obj, &clazz, &fid, "paramp", "J")) != 1)
			return;
		
		params = (PigMICAParams*)env->GetLongField (obj, fid);
		if ( params == 0 )
			return;

		myGround = (env)->GetDoubleArrayElements( jGround, NULL );

		params->PigMICASetGround( (double*)myGround );

		if ((getField(env, obj, &clazz, &fid, "ground", "[D")) != 1)
			return;
		
		javaGround = (jdoubleArray)env->GetObjectField (obj, fid);
		if ( javaGround == 0 )
			return;
		
		(env)->SetDoubleArrayRegion( javaGround, 0, 3, myGround );
		jmethodID mid = (env)->GetMethodID( clazz, "reset", "()V" );
		if ( mid == 0 )
			return;
		(env)->CallVoidMethod( obj, mid );

	}
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    setNormal
   * Signature: ([D)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_setNormal
	    (JNIEnv *env, jobject obj, jdoubleArray jNormal )
  {
		jclass           clazz=0;
		jfieldID         fid=0;
		PigMICAParams	 *params;
		jdouble			 *myNormal;
		jdoubleArray	 javaNormal;

		if ((getField(env, obj, &clazz, &fid, "paramp", "J")) != 1)
			return;
		
		params = (PigMICAParams*)env->GetLongField (obj, fid);
		if ( params == 0 )
			return;

		myNormal = (env)->GetDoubleArrayElements( jNormal, NULL );

		params->PigMICASetNormal( (double*)myNormal );

		if ((getField(env, obj, &clazz, &fid, "normal", "[D")) != 1)
			return;
		
		javaNormal = (jdoubleArray)env->GetObjectField (obj, fid);
		if ( javaNormal == 0 )
			return;
		
		(env)->SetDoubleArrayRegion( javaNormal, 0, 3, myNormal );
		jmethodID mid = (env)->GetMethodID( clazz, "reset", "()V" );
		if ( mid == 0 )
			return;
		(env)->CallVoidMethod( obj, mid );

  }
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    setSurface
   * Signature: (Ljava/lang/String;)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_setSurface
	    (JNIEnv *env, jobject obj, jstring jSurface)
  {
		jboolean         isCopy;
		const char       *mySurface;
		jclass           clazz=0;
		jfieldID         fid=0;
		PigMICAParams	 *params;

		if ((getField(env, obj, &clazz, &fid, "paramp", "J")) != 1)
			return;
		
		params = (PigMICAParams*)env->GetLongField (obj, fid);
		if ( params == 0 )
			return;

		mySurface = env->GetStringUTFChars (jSurface, &isCopy);

		params->PigMICASetSurface( mySurface );

		if ((getField(env, obj, &clazz, &fid, "surface", 
										"Ljava/lang/String;")) != 1)
			return;
//	 	clazz = (env)->GetObjectClass( obj );
//		fid = (env)->GetFieldID( clazz, "surface", "Ljava/lang/String;" );
		
		(env)->SetObjectField( obj, fid, jSurface );
	 	(env)->ReleaseStringUTFChars( jSurface, mySurface );
		jmethodID mid = (env)->GetMethodID( clazz, "reset", "()V" );
		if ( mid == 0 )
			return;
		(env)->CallVoidMethod( obj, mid );

  }
  /*
   * Class:     jpl_mipl_mica_pig_SurfaceModel
   * Method:    setPointMethod
   * Signature: (Ljava/lang/String;)V
   */
  JNIEXPORT void JNICALL Java_jpl_mipl_mica_pig_SurfaceModel_setPointMethod
	    (JNIEnv *env, jobject obj, jstring jPointMethod)
  {
		jboolean         isCopy;
		const char       *myPointMethod;
		jclass           clazz=0;
		jfieldID         fid=0;
		PigMICAParams	 *params;

		if ((getField(env, obj, &clazz, &fid, "paramp", "J")) != 1)
			return;
		
		params = (PigMICAParams*)env->GetLongField (obj, fid);
		if ( params == 0 )
			return;

		myPointMethod = env->GetStringUTFChars (jPointMethod, &isCopy);

		params->PigMICASetPointMethod( myPointMethod );

		if ((getField(env, obj, &clazz, &fid, "surface", 
										"Ljava/lang/String;")) != 1)
			return;
//	 	clazz = (env)->GetObjectClass( obj );
//		fid = (env)->GetFieldID( clazz, "surface", "Ljava/lang/String;" );
		
		(env)->SetObjectField( obj, fid, jPointMethod );
	 	(env)->ReleaseStringUTFChars( jPointMethod, myPointMethod );
		jmethodID mid = (env)->GetMethodID( clazz, "reset", "()V" );
		if ( mid == 0 )
			return;
		(env)->CallVoidMethod( obj, mid );

  }

}

