{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}

-- |The NSString class declares the programmatic interface for an object that manages 
-- immutable strings. An immutable string is a text string that is defined when it 
-- is created and subsequently cannot be changed. NSString is implemented to represent 
-- an array of Unicode characters, in other words, a text string.
module Cocoa.NSString where

import Cocoa
import Data.String
import Foreign.C
import System.IO.Unsafe(unsafePerformIO)

-- * Types
-- |'IsNSObject' compatable wrapper for NSStrings
data TyNSString a = TyNSString Id

-- * Functions

-- |Create a new 'NSString'
newNSString :: String -> IO (TyNSString Id)
newNSString val = do
  cStr <- newCString val >>= cstr2id
  nsstr <- "NSString" $<- "alloc"
  toNSString $ (nsstr $<<- "initWithUTF8String:") [cStr]

-- |Create a new 'NSString'
-- this function uses 'unsafePerformIO'
newNSString' :: String -> TyNSString Id
newNSString' val = unsafePerformIO $ newNSString val

-- |Checks equality of two 'NSString', uses /isEqualToString:/ internally.
isEqualToString :: TyNSString Id -> TyNSString Id -> IO Bool 
isEqualToString str1 str2 = do 
  nsbool <- (idVal str1 $<<- "isEqualToString:") [idVal str2] >>= id2bool 
  return (nsboolToBool nsbool)

-- |Checks equality of two 'NSString', uses /isEqualToString:/ internally.
-- this function uses 'unsafePerformIO'
isEqualToString' :: TyNSString Id -> TyNSString Id -> Bool 
isEqualToString' str1 str2 = unsafePerformIO (str1 `isEqualToString` str2)

-- TODO: A way to group all the NSObject instances under the one set of functions 
-- using pattern matching? or ADT w/ DataTypes? 
-- |Retrieve the underlying 'Id' type from an 'NSObject'
getStringId :: TyNSString a -> Id
getStringId (TyNSString a) = a

-- |Wrap 'IO' 'Id' object in 'TyNSString'
toNSString :: IO Id -> IO (TyNSString Id)
toNSString obj = obj >>= \o -> return (TyNSString o)

-- |Convert an 'NSString' to an 'IO' 'String'
nsstringToString :: TyNSString Id -> IO String
nsstringToString str = do
  cStr <- idVal str $<- "UTF8String"
  id2cstr cStr >>= peekCString

-- instances
-- | when checking equality of 'NSString' run 'isEqualToString''
instance Eq (TyNSString Id) where 
  (==) x y = x `isEqualToString'` y

-- |Overload strings for ('NSString' 'Id') with 'newNSString''
instance IsString (TyNSString Id) where
  fromString = newNSString'

-- |'NSString' encases an 'Id'
instance IsNSObject (TyNSString Id) where 
  idVal = getStringId
