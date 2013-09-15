#include "objch.h"

id msgSend (id klass, SEL sel) {
  return objc_msgSend(klass, sel);
}
id msgSend1 (id klass, SEL sel, id arg1) {
  return objc_msgSend(klass, sel, arg1);
}
id msgSend2 (id klass, SEL sel, id arg1, id arg2) {
  return objc_msgSend(klass, sel, arg1, arg2);
}
id msgSend3 (id klass, SEL sel, id arg1, id arg2, id arg3) {
  return objc_msgSend(klass, sel, arg1, arg2, arg3);
}
id msgSend4 (id klass, SEL sel, void* arg1, void* arg2, void* arg3, void* arg4) {
  return objc_msgSend(klass, sel, arg1, arg2, arg3, arg4);
}
id msgSend5 (id klass, SEL sel, void* arg1, void* arg2, void* arg3, void* arg4, void* arg5) {
  return objc_msgSend(klass, sel, arg1, arg2, arg3, arg4, arg5);
}
