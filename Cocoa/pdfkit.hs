{-# LANGUAGE OverloadedStrings #-}

module Cocoa.PdfKit where

import Cocoa
import Cocoa.NSArray
import Cocoa.NSURL
import Cocoa.NSString
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
-- ** Initializing Documents

initWithData :: Id -> IO PDFDocument
initWithData dat = do 
  allocDoc <- "PDFDocument" $<- "alloc"
  (allocDoc $<<- "initWithData:") [dat]

initWithURL :: (TyNSURL Id) -> IO PDFDocument
initWithURL url = do 
  allocDoc <- "PDFDocument" $<- "alloc"
  (allocDoc $<<- "initWithURL:") [getURLId url]

-- ** Accessing Document Information
-- |Returns the URL for the document.
documentURL :: PDFDocument -> IO (TyNSURL Id)
documentURL doc = toNSURL $ doc $<- "documentURL"

-- |Returns the major version of the document.
majorVersion :: PDFDocument -> IO (NSInteger)
majorVersion doc = doc $<- "majorVersion" >>= id2nsinteger

-- |Returns the minor version of the document.
minorVersion :: PDFDocument -> IO (NSInteger)
minorVersion doc = doc $<- "minorVersion" >>= id2nsinteger

-- |Returns a string representing the textual content for the entire document.
string :: PDFDocument -> IO (TyNSString Id)
string doc = toNSString $ doc $<- "string"

-- ** Managing Document Security
-- |Returns a Boolean value specifying whether the document is encrypted.
isEncrypted :: PDFDocument -> IO NSBool
isEncrypted doc = doc $<- "isEncrypted" >>= id2bool

-- |Returns a Boolean value indicating whether the document is locked.
isLocked :: PDFDocument -> IO NSBool 
isLocked doc = doc $<- "isLocked" >>= id2bool

-- ** Writing Out the PDF Data
-- |Writes the document to a file at the specified path.
writeToFile :: PDFDocument -> TyNSString Id -> IO NSBool 
writeToFile doc path = 
  (doc $<<- "writeToFile:") [idVal path] >>= id2bool

-- ** Working with Pages
-- |Returns the number of pages in the document.
pageCount :: PDFDocument -> IO NSUInteger
pageCount doc = do 
  count <- doc $<- "pageCount"
  id2nsuinteger count

-- |Returns the page at the specified index number.
pageAtIndex :: PDFDocument -> NSUInteger -> IO PDFPage 
pageAtIndex doc index = do 
  idindex <- nsuinteger2id index
  (doc $<<- "pageAtIndex:") [idindex]

-- |Gets the index number for the specified page.
indexForPage :: PDFDocument -> PDFPage -> IO NSUInteger
indexForPage doc page = do
  (doc $<<- "indexForPage:") [page] >>= id2nsuinteger

-- |Inserts a page at the specified index point.
insertPageAtIndex :: PDFDocument -> PDFPage -> NSUInteger -> IO ()
insertPageAtIndex doc page index = do 
  castedIndex <- nsuinteger2id index
  (doc $<<- "insertPage:atIndex:") [page, castedIndex]
  return ()

removePageAtIndex :: PDFDocument -> NSUInteger -> IO ()
removePageAtIndex doc index = do
  castedIndex <- nsuinteger2id index
  (doc $<<- "removePageAtIndex:") [castedIndex]
  return ()

-- ** Managing Find Operations

-- ** Working with Selections

-- ** Setting the Delegate

-- ** Searching Documents

-- ** Unlocking Documents

-- ** Determining the Page Class

-- * PdfPage Functions
-- ** Initializing a Page
-- ** Getting Information About a Page
-- ** Working with Annotations
-- |Returns an array containing the page’s annotations.
annotations :: PDFPage -> IO [PDFAnnotation]
annotations page = do 
  annots <- toNSArray $ page $<- "annotations"
  arrayToList annots

-- ** Rendering Pages
-- ** Working with Textual Content
-- ** Working with Selections
-- ** Miscellaneous

-- * PdfAnnotation Functions
-- ** Initializing an Annotation
-- ** Accessing Information About an Annotation
-- |Returns the type of the annotation.
-- Types include Line, Link, Text, and so on
annotationType :: PDFAnnotation -> IO (TyNSString Id)
annotationType anno = do 
  nsstr <- anno $<- "type"
  return (TyNSString nsstr)

-- ** Managing Annotation Display Characteristics
-- ** Managing Annotation Drawing and Output



-- * Misc
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
    nsstringToString (TyNSString destStr)
