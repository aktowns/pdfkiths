{-# LANGUAGE OverloadedStrings,ForeignFunctionInterface #-}
-- |Replace objective-c methods with haskell methods at runtime.
--
-- @
-- \{\-\# NOINLINE swizzleDescription #-}
-- swizzleDescription :: SwizzleVoid
-- swizzleDescription _ _ = do
--    putStrLn \"No soup for you\"
--
-- \.\.\. 
-- description str \>\>\= putStrLn
-- str \<\- toNSString \$ \(\"NSDate\" \$\<\- \"alloc\"\) \>\>\= \str \-\> str \$\<\- \"init\"
-- swizzleHSMethod \(idVal str\) \"description\" swizzleDescription
-- description str \>\>\= putStrLn
-- @
--
-- Outputs:
--
-- /2013-09-16 14:37:28 +0000/
--
-- /No soup for you/
-- 

module Swizzle where

import Foreign hiding (new)
import Foreign.C
import Cocoa

-- * Types
-- |Represents an objective-c method
type SwizzleVoid = Id -> Selector -> IO ()

-- * FFI Imports
-- |Adds a new method to a 'Class' with a given name and implementation.
foreign import ccall "class_addMethod" class_addMethod'
  :: Class -> Selector -> FunPtr SwizzleVoid -> CString -> Bool

-- |Wrapper that converts a 'SwizzleVoid' to a 'FunPtr' 'SwizzleVoid'
foreign import ccall "wrapper" mkSwizzleVoid :: SwizzleVoid -> IO (FunPtr SwizzleVoid)

-- * Functions
-- |Replaces original 'Selector' on 'Class' with a new 'Selector'
swizzle :: Class -> Selector -> Selector -> IO ()
swizzle klass orig new = do 
  origMethod <- class_getInstanceMethod klass orig 
  newMethod <- class_getInstanceMethod klass new
  methIMP <- method_getImplementation newMethod
  methTy <- method_getTypeEncoding newMethod 
  if (class_addMethod klass orig methIMP methTy) then do
    origIMP <- method_getImplementation origMethod
    origTy <- method_getTypeEncoding origMethod 
    _ <- class_replaceMethod klass new origIMP origTy
    return ()
  else 
    method_exchangeImplementations origMethod newMethod 

-- |Replaces original 'Selector' on instance of an object with new
swizzleMethod :: Id -> Selector -> Selector -> IO ()
swizzleMethod inst orig new = do
  instKlass <- inst $<- "class" >>= id2class
  swizzle instKlass orig new

-- |Replaces original selector on instance of an object with haskell 'SwizzleVoid'
swizzleHSMethod :: Id -> Selector -> SwizzleVoid -> IO ()
swizzleHSMethod inst orig new = do 
  instKlass <- inst $<- "class" >>= id2class
  methTy <- newCString "v@:"
  func <- mkSwizzleVoid new 
  let ret = class_addMethod' instKlass "tempMethod" func methTy
  if ret then do putStrLn "Succesfully swizzled"
  else putStrLn "No swizzle for you"
  swizzle instKlass orig "tempMethod" 
