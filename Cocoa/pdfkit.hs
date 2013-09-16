{-# LANGUAGE OverloadedStrings #-}

module Cocoa.PdfKit where

import Cocoa
import Control.Monad(filterM)

type PDFDocument = Id
type PDFPage = Id
type PDFAnnotation = Id

type PDFAnnotationButtonWidget = PDFAnnotation
type PDFAnnotationCircle = PDFAnnotation
type PDFAnnotationFreeText = PDFAnnotation
type PDFAnnotationInk = PDFAnnotation
type PDFAnnotationLine = PDFAnnotation
type PDFAnnotationLink = PDFAnnotation
type PDFAnnotationMarkup = PDFAnnotation
type PDFAnnotationPopup = PDFAnnotation
type PDFAnnotationSquare = PDFAnnotation
type PDFAnnotationStamp = PDFAnnotation
type PDFAnnotationText = PDFAnnotation
type PDFAnnotationTextWidget = PDFAnnotation


-- NSURL
fileURLWithPath :: (NSString Id) -> (IO Id)
fileURLWithPath path = ("NSURL" $<<- "fileURLWithPath:") [(idVal path)]

fileURLPath :: Id -> IO String 
fileURLPath url = do 
  furl <- url $<- "path"
  nsstringToString $ NSString furl

-- NSArray
arrayCount :: Id -> (IO NSUInteger)
arrayCount array = (array `msgSendCU` "count") []

-- - (id)objectAtIndex:(NSUInteger)index
arrayObjectAtIndex :: Id -> NSUInteger -> (IO Id)
arrayObjectAtIndex array index = (array `objc_msgSend'CU` "objectAtIndex:") index

-- NSArray to list
arrayToList :: Id -> IO [Id]
arrayToList array = do
  len <- arrayCount array
  sequence $ [ arrayObjectAtIndex array i| i <- [0..(len - 1)] ]

-- NSData
dataWithContentsOfURL :: Id -> IO Id
dataWithContentsOfURL aURL = ("NSData" $<<- "dataWithContentsOfURL:") [aURL]

-- PdfKit
initWithData :: Id -> IO PDFDocument
initWithData dat = do 
  allocDoc <- "PDFDocument" $<- "alloc"
  (allocDoc $<<- "initWithData:") [dat]

initWithURL :: Id -> IO PDFDocument
initWithURL url = do 
  allocDoc <- "PDFDocument" $<- "alloc"
  (allocDoc $<<- "initWithURL:") [url]

pageAtIndex :: PDFDocument -> NSUInteger -> IO PDFPage 
pageAtIndex doc index = (doc `objc_msgSend'CU` "pageAtIndex:") index

pageCount :: PDFDocument -> IO NSUInteger
pageCount doc = (doc `msgSendCU` "pageCount") []

annotations :: PDFPage -> IO [PDFAnnotation]
annotations page = do 
  annots <- page $<- "annotations"
  arrayToList annots

annotationType :: PDFAnnotation -> IO (NSString Id)
annotationType anno = do 
  nsstr <- anno $<- "type"
  return (NSString nsstr)

annotationIsLink :: PDFAnnotation -> IO Bool 
annotationIsLink anno = do 
  annoType <- annotationType anno
  return (annoType == "Link")

annotationLinks :: PDFPage -> IO [PDFAnnotationLink]
annotationLinks page = do
  annots <- annotations page
  filterM annotationIsLink annots

annotationLinkURL :: PDFAnnotationLink -> IO String
annotationLinkURL anno = do
  destURL <- anno $<- "URL"
  if destURL == nilPtr
    then do return ""
  else do
    destStr <- destURL $<- "absoluteString"
    nsstringToString (NSString destStr)
