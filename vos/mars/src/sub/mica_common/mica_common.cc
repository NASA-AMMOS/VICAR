#include <stdio.h>
#include "jni.h"
#include "PigVector.h"
/* -------------------------------------------------------------------- */
/**
 * return the c++ pointer stored in the java object.
 * 
 * This function assumes that a field of name "cpp"
 * is found in the object pointed to by obj.
 */
void * resolveField (JNIEnv *env, jobject obj) {
    jclass clazz = 0;
    jfieldID fid = 0;
    jlong lp;

    // first resolve a jclass object.
    clazz = env->GetObjectClass(obj);
    if (clazz == 0) {
      env->ExceptionDescribe ();
      clazz = env->FindClass ("java/lang/IllegalArgumentException");
      env->ThrowNew (clazz, "resolveField: no object class for input");
      return 0;
    }
    
    // then find the field id of the field "cpp" with signiture jlong
    fid = env->GetFieldID (clazz, "cpp", "J");
    if (fid == 0) {
      env->ExceptionDescribe ();
      clazz = env->FindClass ("java/lang/NoSuchFieldError");
      env->ThrowNew (clazz, "no c++ field found in input object");
      return 0;
    }
    // now return the contents of the long field
    // the caller can cast this if needed.
    lp = env->GetLongField (obj, fid);
    return (void *)lp;
}
/* -------------------------------------------------------------------- */
/**
 * given an object setup the field id and class for access to the 
 * input fieldname and signiture.
 *
 */
jint getField (JNIEnv *env, jobject obj, jclass *clazz, jfieldID *fid, char *fieldName, char *signi) 
{
    // change the input parameter clazz
    *clazz = env->GetObjectClass(obj);
    if (clazz == 0) {
      env->ExceptionDescribe ();
      jclass clz = env->FindClass ("java/lang/IllegalArgumentException");
      env->ThrowNew (clz, "resolveField: no object class for input");
      return 0;
    }
    // and change the input parm fid
    *fid = env->GetFieldID (*clazz, fieldName, signi);
    if (*fid == 0) {
      env->ExceptionDescribe ();
      jclass clz = env->FindClass ("java/lang/NoSuchFieldError");
      env->ThrowNew (clz, "no c++ field found in input object");
      return 0;
    }
    return 1;
}
/* -------------------------------------------------------------------- */
/**
 * make a new java object.
 *
 */
jint makeJObject (JNIEnv *env, jobject obj, char *className, char *signiture, jclass *pointClazz, jmethodID *mid)
{
    jclass          clazz=0;
    char bufr[4096];

    // first find the class for the object
    *pointClazz = env->FindClass (className);
    if (*pointClazz == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/NoClassDefFoundError");
      sprintf (bufr, "makeJObject: could not find\nclass %s\nwith signiture %s",className, signiture);
      env->ThrowNew (clazz, bufr);
      return 0;
    }

    // then find the method id for the constructor
    *mid = env->GetMethodID(*pointClazz, "<init>", signiture);
    if (*mid == 0) {
      env->ExceptionDescribe();
      clazz = env->FindClass ("java/lang/NoSuchMethodError");
      env->ThrowNew (clazz, "makeJObject: no method for constructor\n");
      return 0;
    }

    return 1;
}

