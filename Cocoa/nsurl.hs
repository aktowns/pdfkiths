{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}

-- |An NSURL object lets you manipulate URLs and the resources they reference. 
-- The URLs employed by the NSURL class are described in RFCs 1808, 1738, and 2732.
module Cocoa.NSURL where

import Cocoa
import Cocoa.NSString

-- * Types
-- |'IsNSObject' compatable wrapper for NSURLs
data TyNSURL a = TyNSURL Id

-- * Functions

-- |Retrieve the underlying 'Id' type from an 'NSObject'
getURLId :: (TyNSURL a) -> Id
getURLId (TyNSURL a) = a

-- |Wrap 'IO' 'Id' object in 'TyNSURL'
toNSURL :: IO Id -> IO (TyNSURL Id)
toNSURL obj = obj >>= \o -> return (TyNSURL o)

-- |Specify path to file returning an NSURL. 
-- equiv to: /[NSURL fileURLWithPath:@""];/
fileURLWithPath :: TyNSString Id -> IO (TyNSURL Id)
fileURLWithPath path = do
  url <- ("NSURL" $<<- "fileURLWithPath:") [idVal path]
  return (TyNSURL url)

-- |Retrieve path specified by the NSURL as a string. equiv to: /[url path];/
fileURLPath :: (TyNSURL Id) -> IO String 
fileURLPath url = do 
  furl <- (getURLId url) $<- "path"
  nsstringToString $ TyNSString furl

-- instances
-- |'NSURL' encases an 'Id'
instance IsNSObject (TyNSURL Id) where
  idVal = getURLId


