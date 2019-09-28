-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Poseidon
-- Copyright   :  (c) 2019 Florian Grignon
-- License     :  BSD3
--
-- Maintainer  :  grignon.florian@gmail.com
-- Stability   :  experimental
--
-- This library provide a Simple and Extensible access to PostgreSQL.
--
-- Simple: Poseidon runs a SQL query and returns a set of custom datatype.
-- **It is not an ORM.**
--
-- Extensible: As a user of the library, you can map your custom PostgreSQL
-- type to your Haskell datatype easily, in a pluggable way (e.g. if you're
-- using postgis, you will be most likely interested by poseidon-postgis,
-- that maps GeoJSON WKT to GeospatialGeometry).
--
-----------------------------------------------------------------------------
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Poseidon where


import GHC.Generics

import Data.Text
import Data.UUID
import Data.Time
import Data.Aeson as A
import Data.ByteString as B
import Data.ByteString.Lazy as BSL


newtype PGText = PGText Text
  deriving (Generic, Show)

toText :: PGText -> Text
toText pgText = case pgText of
                  PGText text' -> text'

newtype PGBool = PGBool Bool
  deriving (Generic, Show)

toBool :: PGBool -> Bool
toBool pgBool = case pgBool of
                  PGBool value' -> value'

newtype PGTimestamp = PGTimestamp UTCTime
  deriving (Generic, Show)

toUTCTime :: PGTimestamp -> UTCTime
toUTCTime pgTimestamp = case pgTimestamp of
                  PGTimestamp value' -> value'

newtype PGUUID = PGUUID UUID
  deriving (Generic, Show)

toUUID :: PGUUID -> UUID
toUUID pgUUID = case pgUUID of
                  PGUUID uuid' -> uuid'

newtype PGJsonValue = PGJsonValue A.Value
  deriving (Generic, Show)

toValue :: PGJsonValue -> A.Value
toValue pgJsonValue = case pgJsonValue of
                        PGJsonValue value' -> value'

newtype PGInteger = PGInteger Integer
  deriving (Generic, Show)

toInteger :: PGInteger -> Integer
toInteger pgInteger = case pgInteger of
                        PGInteger integer' -> integer'

newtype PGDouble = PGDouble Double
  deriving (Generic, Show)

toDouble :: PGDouble -> Double
toDouble pgDouble = case pgDouble of
                        PGDouble integer' -> integer'

newtype PGDecimal = PGDecimal Float
  deriving (Generic, Show)

toDecimal :: PGDecimal -> Float
toDecimal pgDecimal = case pgDecimal of
                        PGDecimal value' -> value'

newtype PGByteString = PGByteString B.ByteString
  deriving (Generic, Show)

toByteString :: PGByteString -> B.ByteString
toByteString pgByteString = case pgByteString of
                        PGByteString value' -> value'

newtype PGLazyByteString = PGLazyByteString BSL.ByteString
  deriving (Generic, Show)

toLazyByteString :: PGLazyByteString -> BSL.ByteString
toLazyByteString pgLazyByteString = case pgLazyByteString of
                        PGLazyByteString value' -> value'
