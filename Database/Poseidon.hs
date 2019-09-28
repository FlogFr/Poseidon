-----------------------------------------------------------------------------
-- |
-- Module      :  Database.Poseidon
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

module Database.Poseidon where

import Prelude

import Generics.Eot

import Control.Concurrent.Async

import Foreign.C.Types
import Control.Exception
import Data.Maybe (fromMaybe)
import Database.Poseidon.Internal
import Data.Poseidon()

import Data.Binary
import Data.Binary.Get
import Data.UUID
import Data.Time
import Data.Aeson as A hiding (Result)
import Data.Text hiding (splitAt)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Database.PostgreSQL.LibPQ
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal      as B
                                                ( ByteString(..) )

newtype ExceptionText = ExceptionText Text
  deriving (Show, Eq)
instance Exception ExceptionText

data ExceptionPostgreSQL = ExceptionPGUniqueViolation
                         | ExceptionPGJWTMalformed
                         | ExceptionPGJWTInvalid
                         | ExceptionPGUnknown
  deriving (Show, Eq)
instance Exception ExceptionPostgreSQL

queryFromText :: (HasEot a, EotDeserialize (Eot a)) => Connection -> Text -> [Maybe (Oid, B.ByteString, Format)] -> IO [a]
queryFromText conn sqlQuery params = do
  mresult <- execParams conn (encodeUtf8 sqlQuery) params Binary 
  case mresult of
    Just result -> do
      rStatus <- resultStatus result
      case rStatus of
        TuplesOk -> do
          (Row nbRows) <- ntuples result
          let rows = [0..(nbRows-1)]
          mapConcurrently (\k -> genericDeserialize result k 0) rows
        FatalError -> do
          diagSqlstate <- resultErrorField result DiagSqlstate

          diagSeverity <- resultErrorField result DiagSeverity
          putStrLn $ ("DiagSeverity : " <> (show diagSeverity))
          diagMessagePrimary <- resultErrorField result DiagMessagePrimary
          putStrLn $ ("DiagMessagePrimary : " <> (show diagMessagePrimary))
          diagMessageDetail <- resultErrorField result DiagMessageDetail
          putStrLn $ ("DiagMessageDetail : " <> (show diagMessageDetail))
          diagMessageHint <- resultErrorField result DiagMessageHint
          putStrLn $ ("DiagMessageHint : " <> (show diagMessageHint))
          diagStatementPosition <- resultErrorField result DiagStatementPosition
          putStrLn $ ("DiagStatementPosition : " <> (show diagStatementPosition))
          diagInternalPosition <- resultErrorField result DiagInternalPosition
          putStrLn $ ("DiagInternalPosition : " <> (show diagInternalPosition))
          diagInternalQuery <- resultErrorField result DiagInternalQuery
          putStrLn $ ("DiagInternalQuery : " <> (show diagInternalQuery))
          diagContext <- resultErrorField result DiagContext
          putStrLn $ ("DiagContext : " <> (show diagContext))
          diagSourceFile <- resultErrorField result DiagSourceFile
          putStrLn $ ("DiagSourceFile : " <> (show diagSourceFile))
          diagSourceLine <- resultErrorField result DiagSourceLine
          putStrLn $ ("DiagSourceLine : " <> (show diagSourceLine))
          diagSourceFunction <- resultErrorField result DiagSourceFunction
          putStrLn $ ("DiagSourceFunction : " <> (show diagSourceFunction))
          putStrLn $ ("DiagSqlstate : " <> (show diagSqlstate))

          case diagSqlstate of
            Just sqlStateBS -> do
              let sqlState = show sqlStateBS :: [Char]
              case sqlState of
                -- https://www.postgresql.org/docs/current/errcodes-appendix.html
                "\"23505\"" -> throw ExceptionPGUniqueViolation
                "\"P1002\"" -> throw ExceptionPGJWTMalformed
                "\"P1003\"" -> throw ExceptionPGJWTInvalid
                _ -> throw $ ExceptionPGUnknown
            Nothing -> throw $ ExceptionText "Cant retrieve the PostgreSQL error field SqlState"
        otherStatus -> do
          putStrLn $ ("PG Status : " <> (show otherStatus))
          throw $ ExceptionText "Wrong PostgreSQL result status, please check"
    Nothing -> throw $ ExceptionText "Didnt receive a PostgreSQL result"


class EotDeserialize eot where
  eotDeserialize :: Result -> CInt -> CInt -> IO eot

instance (EotDeserialize this, EotDeserialize next) => EotDeserialize (Either this next) where

  eotDeserialize res row _ = Left <$> eotDeserialize res row 0


instance (Deserialize x, EotDeserialize xs) => EotDeserialize (x, xs) where
  -- eotDeserialize :: (Result, CInt, CInt) -> IO eot
  eotDeserialize res row col = do
    firstField <- deserialize res row col
    nextFields <- eotDeserialize res row (succ col)
    pure ( firstField, nextFields )

instance EotDeserialize Void where
  eotDeserialize _ _ _ = error "invalid input"

instance EotDeserialize () where
  eotDeserialize _ _ _ = mempty

getBSValue :: Result -> CInt -> CInt -> IO (Maybe BSL.ByteString)
getBSValue res row col = do
  mValueBS <- getvalue res (Row row) (Col col)
  case mValueBS of
    Just valueBS' -> pure $ Just (BSL.fromStrict valueBS')
    Nothing -> pure $ Nothing

class Deserialize a where
  deserialize :: Result -> CInt -> CInt -> IO a

-- All base datatype we can deserialize
instance Deserialize BSL.ByteString where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    pure $ bs

instance Deserialize BS.ByteString where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    pure $ BSL.toStrict bs

instance Deserialize Integer where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    pure $ fromIntegral . runGet getInt16be $ bs

instance Deserialize Float where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    pure $ runGet getFloatbe bs

instance Deserialize Double where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    pure $ runGet getDoublebe bs

instance Deserialize Text where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    pure $ decodeUtf8 . BSL.toStrict $ bs

instance Deserialize UTCTime where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    let (relDays, relSeconds) = runGet getDate bs
    pure $ UTCTime (ModifiedJulianDay relDays) (secondsToDiffTime relSeconds)

instance Deserialize (Maybe Text) where
  deserialize res row col = (decodeUtf8 . BSL.toStrict <$>) <$> getBSValue res row col

instance Deserialize UUID where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    let mValue = fromByteString bs
    case mValue of
      Just value' -> pure value'
      Nothing -> error "Impossible to decode expected UUID"

getPGBool :: Get Bool
getPGBool = do
  w <- getWord8
  return $ (fromIntegral w :: Integer) /= 0

instance Deserialize Bool where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    pure $ runGet getPGBool bs

instance Deserialize (Maybe Bool) where
  deserialize res row col = (runGet getPGBool <$>) <$> getBSValue res row col

instance Deserialize [Text] where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    let pgArray = runGet getPGArray bs
    let words8 = fmap pgArrayDataData $ pgArrayData pgArray
    pure $ (decodeUtf8 . BS.pack) <$> words8

instance Deserialize () where
  deserialize _ _ _ = pure ()

genericDeserialize :: (HasEot a, EotDeserialize (Eot a)) => Result -> CInt -> CInt -> IO a
genericDeserialize res row col = do
  resrow <- eotDeserialize res row col
  pure $ fromEot resrow

instance Deserialize A.Value where
  deserialize res row col = do
    bs <- (fromMaybe mempty) <$> getBSValue res row col
    let mValue = A.decode bs :: Maybe A.Value
    case mValue of
      Just value' -> pure $ value'
      Nothing -> error "Impossible to decode JSON"
