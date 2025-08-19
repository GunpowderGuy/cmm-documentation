-- Save as: src/NestedAesonDemo.hs
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import GHC.Generics (Generic)
import Data.Aeson   (ToJSON, FromJSON, encode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)

newtype ID = ID { unID :: Int }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Tag = Tag
  { label :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Profile = Profile
  { name :: Text
  , tags :: [Tag]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data User = User
  { userId  :: ID
  , profile :: Profile
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

demo :: IO ()
demo = do
  let u = User (ID 42) (Profile "Diego" [Tag "haskell", Tag "cmm"])
  putStrLn "-- Encode:"
  LBS.putStrLn (encode u)
  putStrLn "\n-- Decode:"
  print (eitherDecode (encode u) :: Either String User)

