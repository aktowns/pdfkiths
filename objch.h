#include <objc/objc.h>
#include <objc/runtime.h>
#include <objc/message.h>
#include <stdio.h>

#if __LP64__ || (TARGET_OS_EMBEDDED && !TARGET_OS_IPHONE) || TARGET_OS_WIN32 || NS_BUILD_32_LIKE_64
typedef long NSInteger;
typedef unsigned long NSUInteger;
#else
typedef int NSInteger;
typedef unsigned int NSUInteger;
#endif

typedef signed char     BOOL; 

id msgSend (id klass, SEL sel);
id msgSend1 (id klass, SEL sel, id arg1);
id msgSend2 (id klass, SEL sel, id arg1, id arg2);
id msgSend3 (id klass, SEL sel, id arg1, id arg2, id arg3);
id msgSend4 (id klass, SEL sel, void* arg1, void* arg2, void* arg3, void* arg4);
id msgSend5 (id klass, SEL sel, void* arg1, void* arg2, void* arg3, void* arg4, void* arg5);

id nilPtr ();
NSInteger id2NSInteger(id val);
NSUInteger id2NSUInteger(id val);
BOOL id2Bool(id val);
id NSInteger2id(NSInteger val);
id NSUInteger2id(NSUInteger val);
id Bool2id(BOOL val);
