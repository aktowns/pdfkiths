{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings,FlexibleInstances #-}

module Cocoa where

import Foreign hiding (unsafePerformIO) -- deprecated
import Foreign.C
import Data.String
import System.IO.Unsafe(unsafePerformIO)

-- * Objective-C like Type mappings
-- TODO: Primitives need to be handled a lot more cleanly.

-- |Treat 'NSUInteger' like 'CULong'.. this is 64-bit dependent.
type NSUInteger = CULong 
-- |Treat 'NSInteger' like 'CInt'.. this is 64-bit dependent.
type NSInteger = CInt
-- |Treat NSBool as a CInt (that should be either 1 | 0). 
-- It seems most of the NS frameworks use this, instead of C99 booleans.
type NSBool = CInt

-- |Class, essentially void* as we don't need anything from it.
-- Just used for passing around.
newtype Class = Class (Ptr ())
-- |Selector, essentially void* as we don't need anything from it.
-- Just used for passing around.
newtype Selector = Selector (Ptr ())
-- |Method, essentially void* as we don't need anything from it.
-- Just used for passing around.
newtype Method = Method (Ptr ())
-- |Id, essentially void* as we don't need anything from it.
-- Just used for passing around.
newtype Id = Id (Ptr ()) deriving (Eq)

-- |[YES] in obj-c. this is a 'Int' with the value /1/
nsYES :: NSBool
nsYES = 1
-- |[NO] in obj-c. this is a 'Int' with the value /0/
nsNO :: NSBool
nsNO  = 0

-- * FFI imports
-- |ffi import for class_getClassMethod
foreign import ccall "class_getClassMethod" class_getClassMethod 
  :: Class -> Selector -> IO Method

-- |ffi import for objc_getClass
foreign import ccall "objc_getClass" objc_getClass 
  :: CString -> IO Id

-- |ffi import for sel_registerName
foreign import ccall "sel_registerName" sel_registerName 
  :: CString -> IO Selector

-- |These are defined via the objch.c, as ffi + varargs is a world of pain
-- |ffi import for objc_msgSend taking no arguments
foreign import ccall "msgSend" objc_msgSend
  :: Id -> Selector -> IO Id
-- |ffi import for objc_msgSend taking 1 argument
foreign import ccall "msgSend1" objc_msgSend1 
  :: Id -> Selector -> Id -> IO Id
-- |ffi import for objc_msgSend taking 2 arguments
foreign import ccall "msgSend2" objc_msgSend2 
  :: Id -> Selector -> Id -> Id -> IO Id
-- |ffi import for objc_msgSend taking 3 arguments
foreign import ccall "msgSend3" objc_msgSend3 
  :: Id -> Selector -> Id -> Id -> Id -> IO Id
-- |ffi import for objc_msgSend taking 4 arguments
foreign import ccall "msgSend4" objc_msgSend4 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> IO Id
-- |ffi import for objc_msgSend taking 5 arguments
foreign import ccall "msgSend5" objc_msgSend5 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> IO Id
-- |ffi import for objc_msgSend taking 6 arguments
foreign import ccall "msgSend6" objc_msgSend6 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> Id -> IO Id
-- |ffi import for objc_msgSend taking 7 arguments
foreign import ccall "msgSend7" objc_msgSend7 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> Id -> Id -> IO Id
-- |ffi import for objc_msgSend taking 8 arguments
foreign import ccall "msgSend8" objc_msgSend8 
  :: Id -> Selector -> Id -> Id -> Id -> Id -> Id -> Id -> Id -> Id -> IO Id

-- |nil in objective-c
foreign import ccall "nilPtr" nilPtr :: Id

-- |hacks because /objc_msgSend/ can return primitives, instead of the id struct.. 
foreign import ccall "id2NSInteger" id2nsinteger :: Id -> IO NSInteger
foreign import ccall "NSInteger2id" nsInteger2id :: NSInteger -> IO Id
foreign import ccall "id2NSUInteger" id2nsuinteger :: Id -> IO NSUInteger
foreign import ccall "NSUInteger2id" nsuinteger2id :: NSUInteger -> IO Id
foreign import ccall "id2Bool" id2bool :: Id -> IO NSBool
foreign import ccall "Bool2id" bool2id :: NSBool -> IO Id
foreign import ccall "id2Cstr" id2cstr :: Id -> IO CString
foreign import ccall "Cstr2id" cstr2id :: CString -> IO Id

-- * Functions

-- |Sends 'Selector' to 'Id'. a /message/ to an /object/. 
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

-- |Returns a class 'Id' for the given 'String'.
getClass :: String -> IO Id
getClass name = newCString name >>= objc_getClass

-- |Returns a class 'Id' for the given 'String'.
-- this function uses 'unsafePerformIO'
getClass' :: String -> Id
getClass' name = unsafePerformIO (getClass name)

-- |Creates a selector, this is like \@selector() in obj-c.
registerName :: String -> IO Selector
registerName name = newCString name >>= sel_registerName

-- |Creates a selector, this is like \@selector() in obj-c.
-- this function uses 'unsafePerformIO'
registerName' :: String -> Selector
registerName' name = unsafePerformIO (registerName name)

-- |Convert an 'NSBool' to 'Bool'
nsboolToBool :: NSBool -> Bool 
nsboolToBool bl = bl == nsYES

-- |Allocates a new autorelease pool, 'newAutoreleasePool' is the
-- equivalent of /[[NSAutoreleasePool alloc] init];/
newAutoreleasePool :: IO Id 
newAutoreleasePool = do 
  poolAlloc <- "NSAutoreleasePool" $<- "alloc"
  poolAlloc $<- "init"

-- |Deallocates an autorelease pool, 'delAutoreleasePool' is the 
-- equivalent of /[pool release];/
delAutoreleasePool :: Id -> IO ()
delAutoreleasePool pool = do 
  _ <- pool $<- "release"
  return ()

-- |This is required to make obj-c calls work
-- enables the default runloop, this call locks the thread
-- its run in, so its advised to forkIO it. 
-- This is the equivalent to /[[NSRunloop mainRunLoop] run];/
startRunloop :: IO ()
startRunloop = do 
  mainRunloop <- "NSRunLoop" $<- "mainRunLoop"
  _ <- mainRunloop $<- "run"
  return ()

-- * Infix operators
-- |Infix for 'msgSend' with an empty list as args.
($<-) :: Id -> Selector -> IO Id
($<-) i s = msgSend i s []

-- |Infix for 'msgSend'
($<<-) :: Id -> Selector -> [Id] -> IO Id
($<<-) = msgSend

-- |Infix for 'msgSend' taking values wrapped with 'IO' with an empty list
-- as args
($<~) :: IO Id -> IO Selector -> IO Id
($<~) i s = do
  uwid <- i
  uwsel <- s
  msgSend uwid uwsel []

-- |Infix for 'msgSend' taking values wrapped with 'IO'
($<<~) :: IO Id -> IO Selector -> [IO Id] -> IO Id 
($<<~) i s a = do
  uwid <- i 
  uwsel <- s
  args <- sequence a
  msgSend uwid uwsel args

-- * Type classes
-- |Used to create light wrappers around objects that are based 'Id'
-- this could be used for any NSObject object. its only used for 'NSString'
-- at the moment so overloaded strings can auto convert to NSString (and "Data.Eq" to compare)
class IsNSObject a where
  -- |The underlying 'Id' object
  idVal :: a -> Id


-- Instances
-- |Overload strings for 'Id' with 'getClass''
instance IsString Id where 
  fromString = getClass'

-- |Overload strings for 'Selector' with 'registerName''
instance IsString Selector where
  fromString = registerName'
