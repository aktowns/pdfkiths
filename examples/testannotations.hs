{-# LANGUAGE OverloadedStrings #-}

module Main where
import Cocoa
import Cocoa.PdfKit
import Cocoa.NSURL
import Cocoa.NSString
import Control.Concurrent(forkIO)
import Control.Monad(forM_)
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs

  filename <- newNSString $ unwords args

  pool <- newAutoreleasePool
  _ <- forkIO startRunloop

  fileUrl <- fileURLWithPath filename
  pdfDoc <- initWithURL fileUrl

  page <- pageAtIndex pdfDoc 0
  links <- annotationLinks page
  linkDestinations <-  mapM annotationLinkURL links 

  putStrLn "Annotations:"
  forM_ linkDestinations (\x -> putStrLn ('\t' : x))

  -- threadDelay (1000000 * 10) -- sleep for 10s

  delAutoreleasePool pool
  return ()
