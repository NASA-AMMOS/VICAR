extern void * resolveField(JNIEnv *env, jobject obj);
extern jint  getField(JNIEnv *env, jobject obj, jclass *, jfieldID *, char *fieldName, char *signiture);
extern jint   makeJObject (JNIEnv *env, jobject obj, char *className, char *signiture, jclass *clazz, jmethodID *mid);
