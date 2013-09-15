#include <objc/objc.h>
#include <objc/runtime.h>
#include <objc/message.h>
#include <stdio.h>

id msgSend (id klass, SEL sel);
id msgSend1 (id klass, SEL sel, id arg1);
id msgSend2 (id klass, SEL sel, id arg1, id arg2);
id msgSend3 (id klass, SEL sel, id arg1, id arg2, id arg3);
id msgSend4 (id klass, SEL sel, void* arg1, void* arg2, void* arg3, void* arg4);
id msgSend5 (id klass, SEL sel, void* arg1, void* arg2, void* arg3, void* arg4, void* arg5);
