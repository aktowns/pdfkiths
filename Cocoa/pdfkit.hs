{-# LANGUAGE OverloadedStrings #-}

module Cocoa.PdfKit where

import Cocoa
import Control.Monad(filterM)

-- * Types
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

-- * PdfKit Functions
initWithData :: Id -> IO PDFDocument
initWithData dat = do 
  allocDoc <- "PDFDocument" $<- "alloc"
  (allocDoc $<<- "initWithData:") [dat]

initWithURL :: (NSURL Id) -> IO PDFDocument
initWithURL url = do 
  allocDoc <- "PDFDocument" $<- "alloc"
  (allocDoc $<<- "initWithURL:") [getURLId url]

pageAtIndex :: PDFDocument -> NSUInteger -> IO PDFPage 
pageAtIndex doc index = do 
  idindex <- nsuinteger2id index
  (doc $<<- "pageAtIndex:") [idindex]

pageCount :: PDFDocument -> IO NSUInteger
pageCount doc = do 
  count <- doc $<- "pageCount"
  id2nsuinteger count

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
    then return ""
  else do
    destStr <- destURL $<- "absoluteString"
    nsstringToString (NSString destStr)
