{-# LANGUAGE ForeignFunctionInterface,OverloadedStrings,FlexibleInstances #-}

module Cocoa where

import Foreign hiding (unsafePerformIO) -- deprecated
import Foreign.C
import Data.String
import System.IO.Unsafe(unsafePerformIO)

-- * Objective-C like Type mappings

-- | just to make things read a bit clearer


-- TODO: Primitives need to be handled a lot more cleanly.

-- | Treat 'NSUInteger' like 'CULong'.. this is 64-bit dependent.
type NSUInteger = CULong 
type NSInteger = CInt
type NSBool = CInt

newtype Class = Class (Ptr ())
newtype Selector = Selector (Ptr ())
newtype Method = Method (Ptr ())
newtype Id = Id (Ptr ()) deriving (Eq)

data NSURL a = NSURL Id
data NSString a = NSString Id

-- |/YES/ in obj-c. this is a 'Int' with the value /1/
nsYES :: NSBool
nsYES = 1
-- |/NO/ in obj-c. this is a 'Int' with the value /0/
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

-- * Type classes
-- |Used to create light wrappers around objects that are based 'Id'
-- this could be used for any NSObject object. its only used for 'NSString'
-- at the moment so overloaded strings can auto convert to NSString (and "Data.Eq" to compare)
class NSObject a where
  -- |The underlying 'Id' object
  idVal :: a -> Id

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

-- |Creates a selector, this is like @selector() in obj-c.
registerName :: String -> IO Selector
registerName name = newCString name >>= sel_registerName

-- |Creates a selector, this is like @selector() in obj-c.
-- this function uses 'unsafePerformIO'
registerName' :: String -> Selector
registerName' name = unsafePerformIO (registerName name)

-- TODO: A way to group all the NSObject instances under the one set of functions 
-- using pattern matching? or ADT w/ DataTypes? 
-- |Retrieve the underlying 'Id' type from an 'NSObject'
getStringId :: (NSString a) -> Id
getStringId (NSString a) = a
-- |Retrieve the underlying 'Id' type from an 'NSObject'
getURLId :: (NSURL a) -> Id
getURLId (NSURL a) = a

-- |Create a new 'NSString'
newNSString :: String -> IO (NSString Id)
newNSString val = do
  cStr <- newCString val >>= cstr2id
  nsstr <- "NSString" $<- "alloc"
  string <- (nsstr $<<- "initWithUTF8String:") [cStr]
  return $ NSString string

-- |Create a new 'NSString'
-- this function uses 'unsafePerformIO'
newNSString' :: String -> NSString Id
newNSString' val = unsafePerformIO $ newNSString val

-- |Convert an 'NSBool' to 'Bool'
nsboolToBool :: NSBool -> Bool 
nsboolToBool bl = bl == nsYES

-- |Convert an 'NSString' to an 'IO' 'String'
nsstringToString :: NSString Id -> IO String
nsstringToString str = do
  cStr <- idVal str $<- "UTF8String"
  id2cstr cStr >>= peekCString

-- |Checks equality of two 'NSString', uses /isEqualToString:/ internally.
isEqualToString :: NSString Id -> NSString Id -> IO Bool 
isEqualToString str1 str2 = do 
  nsbool <- (idVal str1 $<<- "isEqualToString:") [idVal str2] >>= id2bool 
  return (nsboolToBool nsbool)

-- |Checks equality of two 'NSString', uses /isEqualToString:/ internally.
-- this function uses 'unsafePerformIO'
isEqualToString' :: NSString Id -> NSString Id -> Bool 
isEqualToString' str1 str2 = unsafePerformIO (str1 `isEqualToString` str2)

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

-- |Returns a string representation of the current obj-c object. 
-- This is the equivalent of /[x description];/
nsObjectDescribe :: Id -> IO String 
nsObjectDescribe obj = do
  descString <- obj $<- "description"
  nsstringToString $ NSString descString

-- * NSArray Functions
-- |Items in array equiv to: /[array count];/
arrayCount :: Id -> IO NSUInteger
arrayCount array = do
  count <- array $<- "count"
  id2nsuinteger count

-- |Retrieve object at specified index equiv to: /[array objectAtIndex:index];/
arrayObjectAtIndex :: Id -> NSUInteger -> IO Id
arrayObjectAtIndex array index = do
  idindex <- nsuinteger2id index
  (array $<<- "objectAtIndex:") [idindex]

-- |Convert NSArray to list
arrayToList :: Id -> IO [Id]
arrayToList array = do
  len <- arrayCount array
  sequence [ arrayObjectAtIndex array i| i <- [0..(len - 1)] ]

-- * NSURL Functions
-- |Specify path to file returning an NSURL. 
-- equiv to: /[NSURL fileURLWithPath:@""];/
fileURLWithPath :: NSString Id -> IO (NSURL Id)
fileURLWithPath path = do
  url <- ("NSURL" $<<- "fileURLWithPath:") [idVal path]
  return (NSURL url)

-- |Retrieve path specified by the NSURL as a string. equiv to: /[url path];/
fileURLPath :: (NSURL Id) -> IO String 
fileURLPath url = do 
  furl <- (getURLId url) $<- "path"
  nsstringToString $ NSString furl

-- * NSData Functions
-- |Return NSData from path specified by an NSURL. 
-- equiv to: /[NSData dataWithContentsOfURL:url];/
dataWithContentsOfURL :: Id -> IO Id
dataWithContentsOfURL aURL = ("NSData" $<<- "dataWithContentsOfURL:") [aURL]


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

-- Instances
-- | when checking equality of 'NSString' run 'isEqualToString''
instance Eq (NSString Id) where 
  (==) x y = x `isEqualToString'` y

-- |Overload strings for 'Id' with 'getClass''
instance IsString Id where 
  fromString = getClass'

-- |Overload strings for 'Selector' with 'registerName''
instance IsString Selector where
  fromString = registerName'

-- |Overload strings for ('NSString' 'Id') with 'newNSString''
instance IsString (NSString Id) where
  fromString = newNSString'

-- |'NSString' encases an 'Id'
instance NSObject (NSString Id) where 
  idVal = getStringId

-- |'NSURL' encases an 'Id'
instance NSObject (NSURL Id) where
  idVal = getURLId
