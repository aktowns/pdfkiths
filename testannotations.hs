{-# LANGUAGE OverloadedStrings,BangPatterns #-}

module Main where
import Cocoa
import Cocoa.PdfKit
import Control.Concurrent(threadDelay, forkIO)

main :: IO ()
main = do
  pool <- newAutoreleasePool
  forkIO startRunloop

  fileUrl <- fileURLWithPath "/Users/ashleyis/Projects/pdfkiths/P1-1.pdf"
  pdfDoc <- initWithURL fileUrl

  page <- pageAtIndex pdfDoc 0
  links <- annotationLinks page
  linkDestinations <-  mapM annotationLinkURL links 

  putStrLn "Annotations:"
  print linkDestinations

  threadDelay (1000000 * 10) -- sleep for 10s

  delAutoreleasePool pool
  return ()
