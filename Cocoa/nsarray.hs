{-# LANGUAGE OverloadedStrings,FlexibleInstances #-}

-- | 'NSArray' manage ordered collections of objects called arrays
module Cocoa.NSArray where

import Cocoa

-- * Types
-- |'IsNSObject' compatable wrapper for NSArrays
data TyNSArray a = TyNSArray Id

-- * Functions

-- |Items in array equiv to: /[array count];/
arrayCount :: TyNSArray Id -> IO NSUInteger
arrayCount array = do
  count <- idVal array $<- "count"
  id2nsuinteger count

-- |Retrieve object at specified index equiv to: /[array objectAtIndex:index];/
arrayObjectAtIndex :: TyNSArray Id -> NSUInteger -> IO Id
arrayObjectAtIndex array index = do
  idindex <- nsuinteger2id index
  (idVal array $<<- "objectAtIndex:") [idindex]

-- |Convert NSArray to list
arrayToList :: TyNSArray Id -> IO [Id]
arrayToList array = do
  len <- arrayCount array
  sequence [ arrayObjectAtIndex array i| i <- [0..(len - 1)] ]

-- |Retrieve the underlying 'Id' type from an 'NSObject'
getArrayId :: (TyNSArray a) -> Id
getArrayId (TyNSArray a) = a

-- |Wrap 'IO' 'Id' object in 'TyNSArray'
toNSArray :: IO Id -> IO (TyNSArray Id)
toNSArray obj = obj >>= \o -> return (TyNSArray o)

-- instances
-- |'NSURL' encases an 'Id'
instance IsNSObject (TyNSArray Id) where
  idVal = getArrayId
