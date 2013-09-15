{-# LANGUAGE OverloadedStrings #-}

module Cocoa.PdfKit where

import Cocoa

type PdfDocument = Id
type PDFPage = Id
type PDFAnnotation = Id

-- NSURL
fileURLWithPath :: (NSString Id) -> Id
fileURLWithPath path = ("NSURL" $<<- "fileURLWithPath:") [(idVal path)]

-- NSArray
arrayCount :: Id -> NSUInteger 
arrayCount array = (array `msgSendCU` "count") []

-- - (id)objectAtIndex:(NSUInteger)index
arrayObjectAtIndex :: Id -> NSUInteger -> Id
arrayObjectAtIndex array index = (array `objc_msgSend'CU` "objectAtIndex:") index

-- NSArray to list
arrayToList :: Id -> [Id]
arrayToList array = 
  let len = arrayCount array in
  [ arrayObjectAtIndex array i | i <- [0..len] ]

-- NSData
dataWithContentsOfURL :: Id -> Id
dataWithContentsOfURL aURL = ("NSData" $<<- "dataWithContentsOfURL:") [aURL]

-- PdfKit
initWithData :: Id -> Id
initWithData dat = ("NSPDFDocument" $<<- "initWithData:") [dat]

initWithURL :: Id -> Id
initWithURL url = ("PDFDocument" $<<- "initWithURL:") [url]

pageAtIndex :: PdfDocument -> NSUInteger -> PDFPage 
pageAtIndex doc index = (doc `objc_msgSend'CU` "pageAtIndex:") index

pageCount :: PdfDocument -> NSUInteger
pageCount doc = (doc `msgSendCU` "pageCount") []

annotations :: PDFPage -> [PDFAnnotation]
annotations page = arrayToList (page $<- "annotations")
