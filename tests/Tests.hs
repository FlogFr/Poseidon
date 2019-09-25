{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Prelude
import GHC.Generics

import Data.List ((!!))
import           Database.PostgreSQL.LibPQ
import Database.Poseidon

import Test.Hspec
import Test.QuickCheck
import Data.Text hiding (head)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

data User = User {
    first_name :: Text
  , is_admin :: Bool
  } deriving (Generic, Eq, Show)

main :: IO ()
main = do
  conn <- connectdb . encodeUtf8 $ "service=test"
  hspec $ do
    describe "Deserialization tests" $ do
      it "Full user deserialization" $ do
        poseidonResult <- queryFromText conn "SELECT CAST('Florian :)' AS TEXT), CAST('t' AS BOOL);" mempty :: IO [User]
        let resultExpected = User "Florian :)" True
        resultExpected `shouldBe` (poseidonResult!!0)
      it "returns second the first element of a list" $ do
        head [24 ..] `shouldBe` (24 :: Int)
