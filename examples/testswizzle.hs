{-# LANGUAGE OverloadedStrings #-}

{-
replaces description on the NSDate object with swizzleDescription

pdfkiths[master*] % dist/build/testswizzle/testswizzle
2013-09-16 14:22:50 +0000
Succesfully swizzled
No soup for you
[1]    50767 segmentation fault  dist/build/testswizzle/testswizzle
-}

module Main where

import Cocoa
import Cocoa.NSObject
import Cocoa.NSString
import Swizzle
import Control.Concurrent(forkIO)

{-# NOINLINE swizzleDescription #-}
swizzleDescription :: SwizzleVoid
swizzleDescription _ _ = do
  putStrLn "No soup for you"

main :: IO ()
main = do 
  pool <- newAutoreleasePool
  _ <- forkIO startRunloop

  str <- toNSString $ ("NSDate" $<- "alloc") >>= \str -> str $<- "init"

  description str >>= putStrLn
  swizzleHSMethod (idVal str) "description" swizzleDescription
  description str >>= putStrLn

  delAutoreleasePool pool
  return ()
