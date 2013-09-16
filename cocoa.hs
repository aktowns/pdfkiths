{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings,FlexibleInstances #-}

module Cocoa where

import Foreign hiding (unsafePerformIO) -- deprecated
import Foreign.C
import Data.String
import System.IO.Unsafe(unsafePerformIO)

-- obj-c like type mappings, just to make things
-- read a bit clearer
-- TODO: Primitives need to be handled a lot more cleanly.
type NSUInteger = CULong 
type NSInteger = CInt

type NSBool = CInt

newtype Class = Class (Ptr ())
newtype Selector = Selector (Ptr ())
newtype Method = Method (Ptr ())

newtype Id = Id (Ptr ()) deriving (Eq)

-- I'm quite sure this isn't the best way
-- I'd like a type compatable with Id whilst being able to 
-- have its own instances. 
data NSString a = NSString Id

class Idable a where
  idVal :: a -> Id

nsYES :: NSBool
nsYES = 1
nsNO :: NSBool
nsNO  = 0

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
  :: Id -> Selector -> IO Id
foreign import ccall "msgSend1" objc_msgSend1 
  :: Id -> Selector -> Id -> IO Id
foreign import ccall "msgSend2" objc_msgSend2 
  :: Id -> Selector -> Id -> Id -> IO Id
foreign import ccall "msgSend3" objc_msgSend3 
  :: Id -> Selector -> Id -> Id -> Id -> IO Id
foreign import ccall "msgSend4" objc_msgSend4 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> IO Id
foreign import ccall "msgSend5" objc_msgSend5 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> IO Id
foreign import ccall "msgSend6" objc_msgSend6 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> Id -> IO Id
foreign import ccall "msgSend7" objc_msgSend7 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> Id -> Id -> IO Id
foreign import ccall "msgSend8" objc_msgSend8 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> Id -> Id -> Id -> IO Id

msgSend :: Id -> Selector -> [Id] -> IO Id
msgSend kl me [] = objc_msgSend kl me
msgSend kl me [x1] = objc_msgSend1 kl me x1
msgSend kl me [x1,x2] = objc_msgSend2 kl me x1 x2
msgSend kl me [x1,x2,x3] = objc_msgSend3 kl me x1 x2 x3
msgSend kl me [x1,x2,x3,x4] = objc_msgSend4 kl me x1 x2 x3 x4
msgSend kl me [x1,x2,x3,x4,x5] = objc_msgSend5 kl me x1 x2 x3 x4 x5
msgSend kl me [x1,x2,x3,x4,x5,x6] = objc_msgSend6 kl me x1 x2 x3 x4 x5 x6
msgSend kl me [x1,x2,x3,x4,x5,x6,x7] = objc_msgSend7 kl me x1 x2 x3 x4 x5 x6 x7
msgSend kl me [x1,x2,x3,x4,x5,x6,x7,x8] = objc_msgSend8 kl me x1 x2 x3 x4 x5 x6 x7 x8
msgSend _ _ _ = error "To many arguments to msgSend"

foreign import ccall "nilPtr" nilPtr :: Id

-- hacks because msgSend can return primitives, instead of the id struct.. 
foreign import ccall "id2NSInteger" id2nsinteger :: Id -> IO NSInteger
foreign import ccall "NSInteger2id" nsInteger2id :: NSInteger -> IO Id
foreign import ccall "id2NSUInteger" id2nsuinteger :: Id -> IO NSUInteger
foreign import ccall "NSUInteger2id" nsuinteger2id :: NSUInteger -> IO Id
foreign import ccall "id2Bool" id2bool :: Id -> IO NSBool
foreign import ccall "Bool2id" bool2id :: NSBool -> IO Id
foreign import ccall "id2Cstr" id2cstr :: Id -> IO CString
foreign import ccall "Cstr2id" cstr2id :: CString -> IO Id

-- return class for name
getClass :: String -> IO Id
getClass name = newCString name >>= objc_getClass

getClass' :: String -> Id
getClass' name = unsafePerformIO (getClass name)

-- @selector(..)
registerName :: String -> IO Selector
registerName name = newCString name >>= sel_registerName

registerName' :: String -> Selector
registerName' name = unsafePerformIO (registerName name)

-- NSString creation
foreign import ccall "msgSend1" cStringMsgSend1
  :: Id -> Selector -> CString -> IO Id

-- [[NSString alloc] initWithUTF8String: "CStr"]];
newNSString :: String -> IO (NSString Id)
newNSString val = do
  cStr <- newCString val
  nsstr <- "NSString" $<- "alloc"
  string <- cStringMsgSend1 nsstr "initWithUTF8String:" cStr
  return $ NSString string

newNSString' :: String -> NSString Id
newNSString' val = unsafePerformIO $ newNSString val

-- retrieve the underlying Id type from an NSString
getNSStringId :: NSString Id -> Id
getNSStringId (NSString n) = n

nsboolToBool :: NSBool -> Bool 
nsboolToBool bl = bl == nsYES

nsstringToString :: NSString Id -> IO String
nsstringToString str = do
  cStr <- idVal str $<- "UTF8String"
  id2cstr cStr >>= peekCString

isEqualToString :: NSString Id -> NSString Id -> IO Bool 
isEqualToString str1 str2 = do 
  nsbool <- (idVal str1 $<<- "isEqualToString:") [idVal str2] >>= id2bool 
  return (nsboolToBool nsbool)

isEqualToString' :: NSString Id -> NSString Id -> Bool 
isEqualToString' str1 str2 = unsafePerformIO (str1 `isEqualToString` str2)

instance Eq (NSString Id) where 
  (==) x y = x `isEqualToString'` y

-- overloaded strings ftw!
instance IsString Id where 
  fromString = getClass'

instance IsString Selector where
  fromString = registerName'

instance IsString (NSString Id) where
  fromString = newNSString'

-- "receiver" $<- "selector"
($<-) :: Id -> Selector -> IO Id
($<-) i s = msgSend i s []

-- "receiver" $<<- "selector" ["argument"]
($<<-) :: Id -> Selector -> [Id] -> IO Id
($<<-) = msgSend

($<~) :: IO Id -> IO Selector -> IO Id
($<~) i s = do
  uwid <- i
  uwsel <- s
  msgSend uwid uwsel []

($<<~) :: IO Id -> IO Selector -> [IO Id] -> IO Id 
($<<~) i s a = do
  uwid <- i 
  uwsel <- s
  args <- sequence a
  msgSend uwid uwsel args

-- |Allocates a new autorelease pool, 'newAutoreleasePool' is the
-- equivalent of `[[NSAutoreleasePool alloc] init];`
newAutoreleasePool :: IO Id 
newAutoreleasePool = do 
  poolAlloc <- "NSAutoreleasePool" $<- "alloc"
  poolAlloc $<- "init"

-- |Deallocates an autorelease pool, 'delAutoreleasePool' is the 
-- equivalent of `[pool release];`
delAutoreleasePool :: Id -> IO ()
delAutoreleasePool pool = do 
  _ <- pool $<- "release"
  return ()

-- [[NSRunloop currentRunLoop] run];
startRunloop :: IO ()
startRunloop = do 
  mainRunloop <- "NSRunLoop" $<- "mainRunLoop"
  _ <- mainRunloop $<- "run"
  return ()

-- [x description]
nsObjectDescribe :: Id -> IO String 
nsObjectDescribe obj = do
  descString <- obj $<- "description"
  nsstringToString $ NSString descString

-- NSString encases an Id
instance Idable (NSString Id) where 
  idVal = getNSStringId
