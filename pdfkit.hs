{-# LANGUAGE OverloadedStrings #-}

module Cocoa.PdfKit where

import Cocoa

type PdfDocument = Id
type PDFPage = Id

-- NSURL
fileURLWithPath :: (NSString Id) -> Id
fileURLWithPath path = ("NSURL" $<<- "fileURLWithPath:") [(idVal path)]

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
