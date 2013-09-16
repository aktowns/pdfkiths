{-# LANGUAGE OverloadedStrings #-}

-- |NSObject is the root class of most Objective-C class hierarchies. 
-- Through NSObject, objects inherit a basic interface to the runtime system 
-- and the ability to behave as Objective-C objects.
module Cocoa.NSObject where

import Cocoa
import Cocoa.NSString

-- |Returns a string representation of the current obj-c object. 
-- This is the equivalent of /[x description];/
description :: (IsNSObject a) => a -> IO String 
description obj = do
  descString <- idVal obj $<- "description"
  nsstringToString $ TyNSString descString
