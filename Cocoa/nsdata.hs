{-# LANGUAGE OverloadedStrings #-}

-- |NSData provides data objects, object-oriented wrappers for byte buffers. 
-- Data objects let simple allocated buffers (that is, data with no embedded pointers) 
-- take on the behavior of Foundation objects.
module Cocoa.NSData where

import Cocoa

-- |Return NSData from path specified by an NSURL. 
-- equiv to: /[NSData dataWithContentsOfURL:url];/
dataWithContentsOfURL :: Id -> IO Id
dataWithContentsOfURL aURL = ("NSData" $<<- "dataWithContentsOfURL:") [aURL]
