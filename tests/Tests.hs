{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Prelude
import GHC.Generics

import Data.Aeson as A
import Data.List ((!!))
import           Database.PostgreSQL.LibPQ
import Data.Poseidon
import Database.Poseidon

import Test.Hspec
import Data.Scientific
import Data.Time
import qualified Data.UUID as U
import Data.HashMap.Strict
import Data.ByteString as B
import Data.ByteString.Lazy as BSL
import Data.Text hiding (head)
import Data.Text.Encoding (encodeUtf8)

data User = User {
    first_name :: Text
  , is_admin :: Bool
  } deriving (Generic, Eq, Show)

data UserDetails = UserDetails {
    age :: Integer
  , extra_values :: Value
  } deriving (Generic, Eq, Show)

main :: IO ()
main = do
  conn <- connectdb . encodeUtf8 $ "service=test"
  hspec $ do
    describe "Single Value Deserialization tests" $ do
      it "Single Value Text deserialization" $ do
        dataResult <- queryFromText conn "SELECT CAST('Florian :)' AS TEXT);" mempty :: IO [PGText]
        let resultExpected = "Florian :)"
        resultExpected `shouldBe` (toText $ dataResult!!0)
      it "Single Value Bool deserialization" $ do
        dataResult <- queryFromText conn "SELECT CAST('t' AS BOOL);" mempty :: IO [PGBool]
        let resultExpected = True
        resultExpected `shouldBe` (toBool $ dataResult!!0)
      it "Single Value Timestamp With TZ deserialization" $ do
        dataResult <- queryFromText conn "SELECT CAST('2019-09-24 02:04:23+1' AS TIMESTAMPTZ);" mempty :: IO [PGTimestamp]
        let resultExpected = UTCTime (ModifiedJulianDay 58750) (secondsToDiffTime 3863)
        resultExpected `shouldBe` (toUTCTime $ dataResult!!0)
      it "Single Value UUID deserialization" $ do
        dataResult <- queryFromText conn "SELECT CAST('0cfa19ce-eed9-42f8-87b2-64f97dc27770' AS UUID);" mempty :: IO [PGUUID]
        let resultExpected = U.fromString "0cfa19ce-eed9-42f8-87b2-64f97dc27770"
        resultExpected `shouldBe` (Just (toUUID $ dataResult!!0))
      it "Single Value JSON deserialization" $ do
        jsonResult <- queryFromText conn "SELECT '{\"username\": \"florian728\", \"age\": 28}'::JSON ;" mempty :: IO [PGJsonValue]
        let resultExpected = A.Object . fromList $ [("username", "florian728"), ("age", A.Number ( read "28" :: Scientific ) )]
        resultExpected `shouldBe` (toValue $ jsonResult!!0)
      it "Single Value Strict Binary deserialization" $ do
        jsonResult <- queryFromText conn "SELECT CAST(E'\\120\\121' AS BYTEA) ;" mempty :: IO [PGByteString]
        let resultExpected = B.pack [80, 81]
        resultExpected `shouldBe` (toByteString $ jsonResult!!0)
      it "Single Value Lazy Binary deserialization" $ do
        jsonResult <- queryFromText conn "SELECT CAST(E'\\120\\121' AS BYTEA) ;" mempty :: IO [PGLazyByteString]
        let resultExpected = BSL.pack [80, 81]
        resultExpected `shouldBe` (toLazyByteString $ jsonResult!!0)
    describe "User Datatype Deserialization tests" $ do
      it "Full User deserialization" $ do
        dataResult <- queryFromText conn "SELECT CAST('Florian :)' AS TEXT), CAST('t' AS BOOL);" mempty :: IO [User]
        let resultExpected = User "Florian :)" True
        resultExpected `shouldBe` (dataResult!!0)
      it "Full UserDetails deserialization" $ do
        dataResult <- queryFromText conn "SELECT CAST(28 AS SMALLINT), CAST('{\"paid_user\": true, \"registered_at\": \"2019-08-27\"}' AS JSON);" mempty :: IO [UserDetails]
        let resultExpected = UserDetails 28 (A.Object . fromList $ [("paid_user", A.Bool True), ("registered_at", "2019-08-27")])
        resultExpected `shouldBe` (dataResult!!0)
