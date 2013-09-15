{-# LANGUAGE OverloadedStrings,ForeignFunctionInterface,FlexibleInstances,DataKinds #-}

module Cocoa where

import Foreign hiding (unsafePerformIO) -- deprecated
import Foreign.C
import Data.String
import System.IO.Unsafe(unsafePerformIO)

-- obj-c like type mappings, just to make things
-- read a bit clearer
-- TODO: Primitives need to be handled a lot more cleanly.
type NSUInteger = CULong 
newtype Class = Class (Ptr ())
newtype Selector = Selector (Ptr ())
newtype Method = Method (Ptr ())

newtype Id = Id (Ptr ())

-- I'm quite sure this isn't the best way
-- I'd like a type compatable with Id whilst being able to 
-- have its own instances. 
data NSString a = NSString Id

class Idable a where
  idVal :: a -> Id

-- imports
-- Method class_getClassMethod(Class aClass, SEL aSelector)
foreign import ccall "class_getClassMethod" class_getClassMethod 
  :: Class -> Selector -> IO Method

-- id objc_getClass(const char *name)
foreign import ccall "objc_getClass" objc_getClass 
  :: CString -> IO Id

-- SEL sel_registerName(const char *str)
foreign import ccall "sel_registerName" sel_registerName 
  :: CString -> IO Selector

-- id objc_msgSend(id theReceiver, SEL theSelector, ...)
-- these are defined via the objch.c, as ffi + varargs is a world of pain
foreign import ccall "msgSend" objc_msgSend 
  :: Id -> Selector -> Id
foreign import ccall "msgSend1" objc_msgSend1 
  :: Id -> Selector -> Id -> Id
foreign import ccall "msgSend2" objc_msgSend2 
  :: Id -> Selector -> Id -> Id -> Id
foreign import ccall "msgSend3" objc_msgSend3 
  :: Id -> Selector -> Id -> Id -> Id -> Id
foreign import ccall "msgSend4" objc_msgSend4 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id
foreign import ccall "msgSend5" objc_msgSend5 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> Id

msgSend :: Id -> Selector -> [Id] -> Id
msgSend kl me [] = objc_msgSend kl me
msgSend kl me [x1] = objc_msgSend1 kl me x1
msgSend kl me [x1,x2] = objc_msgSend2 kl me x1 x2
msgSend kl me [x1,x2,x3] = objc_msgSend3 kl me x1 x2 x3
msgSend kl me [x1,x2,x3,x4] = objc_msgSend4 kl me x1 x2 x3 x4
msgSend kl me [x1,x2,x3,x4,x5] = objc_msgSend5 kl me x1 x2 x3 x4 x5

-- Variations, is there any easier way to recast?
-- objc_msgSend -> NSUInteger
foreign import ccall "msgSend" objc_msgSendCU
  :: Id -> Selector -> NSUInteger
foreign import ccall "msgSend1" objc_msgSendCU1 
  :: Id -> Selector -> Id -> NSUInteger
foreign import ccall "msgSend2" objc_msgSendCU2 
  :: Id -> Selector -> Id -> Id -> NSUInteger
foreign import ccall "msgSend3" objc_msgSendCU3 
  :: Id -> Selector -> Id -> Id -> Id -> NSUInteger
foreign import ccall "msgSend4" objc_msgSendCU4 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> NSUInteger
foreign import ccall "msgSend5" objc_msgSendCU5 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> NSUInteger

msgSendCU :: Id -> Selector -> [Id] -> NSUInteger
msgSendCU kl me [] = objc_msgSendCU kl me
msgSendCU kl me [x1] = objc_msgSendCU1 kl me x1
msgSendCU kl me [x1,x2] = objc_msgSendCU2 kl me x1 x2
msgSendCU kl me [x1,x2,x3] = objc_msgSendCU3 kl me x1 x2 x3
msgSendCU kl me [x1,x2,x3,x4] = objc_msgSendCU4 kl me x1 x2 x3 x4
msgSendCU kl me [x1,x2,x3,x4,x5] = objc_msgSendCU5 kl me x1 x2 x3 x4 x5

foreign import ccall "msgSend" objc_msgSend'CU 
  :: Id -> Selector -> NSUInteger -> Id
foreign import ccall "msgSend" objc_msgSend'CU'
  :: Id -> Selector -> NSUInteger -> NSUInteger

-- return class for name
getClass :: [Char] -> Id
getClass name = unsafePerformIO ((newCString name) >>= objc_getClass)

-- @selector(..)
registerName :: [Char] -> Selector
registerName name = unsafePerformIO ((newCString name) >>= sel_registerName)

-- NSString creation
foreign import ccall "msgSend1" cStringMsgSend1
  :: Id -> Selector -> CString -> Id

-- [NSString stringWithUTF8String: "CStr"];
newNSString :: [Char] -> (NSString Id)
newNSString val = NSString (unsafePerformIO ((newCString val) >>= \str -> 
  return (cStringMsgSend1 "NSString" "stringWithUTF8String:" str)))

-- retrieve the underlying Id type from an NSString
getNSStringId :: (NSString Id) -> Id
getNSStringId (NSString n) = n

-- overloaded strings ftw!
instance IsString Id where 
  fromString = getClass

instance IsString Selector where
  fromString = registerName

instance IsString (NSString Id) where
  fromString = newNSString

-- "receiver" $<- "selector"
($<-) :: Id -> Selector -> Id
($<-) i s = msgSend i s []

-- "receiver" $<<- "selector" ["argument"]
($<<-) :: Id -> Selector -> [Id] -> Id
($<<-) i s a = msgSend i s a

-- [[NSAutoreleasePool alloc] init];
newAutoreleasePool :: Id 
newAutoreleasePool = "NSAutoreleasePool" $<- "alloc" $<- "init"

-- [pool release];
delAutoreleasePool :: Id -> Id
delAutoreleasePool pool = pool $<- "release"

-- [[NSRunloop currentRunLoop] run];
startRunloop :: Id
startRunloop = "NSRunloop" $<- "currentRunLoop" $<- "run"

-- NSString encases an Id
instance Idable (NSString Id) where 
  idVal = getNSStringId
