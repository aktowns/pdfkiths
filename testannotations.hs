{-# LANGUAGE OverloadedStrings #-}

module Main where
import Cocoa
import Cocoa.PdfKit
import Control.Concurrent(forkIO)
import Control.Monad(forM_)

main :: IO ()
main = do
  pool <- newAutoreleasePool
  _ <- forkIO startRunloop

  fileUrl <- fileURLWithPath "P1-1.pdf"
  pdfDoc <- initWithURL fileUrl

  page <- pageAtIndex pdfDoc 0
  links <- annotationLinks page
  linkDestinations <-  mapM annotationLinkURL links 

  putStrLn "Annotations:"
  forM_ linkDestinations (\x -> putStrLn ('\t' : x))

  -- threadDelay (1000000 * 10) -- sleep for 10s

  delAutoreleasePool pool
  return ()
