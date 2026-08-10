#include "jni_Main.h"

#include <string>
#include <iostream>

JNIEXPORT void JNICALL Java_jni_Main_register
    (JNIEnv *env, jclass cls, jobject target)
{
    jclass targetClass = env->GetObjectClass(target);
    jmethodID method = env->GetMethodID(targetClass, "someFunction", "(Ljava/lang/String;)V");
    if (method)
    {
        std::string str = "abcdeあいうえお";
        std::cout << "[native] " << str << std::endl;

        jstring jstr = env->NewStringUTF(str.c_str());
        env->CallVoidMethod(target, method, jstr);
    }

    env->DeleteLocalRef(targetClass);
}