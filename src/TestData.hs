{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module TestData where

import Test.HUnit
import Data.Yaml(FromJSON(..), (.:))
import qualified Data.Yaml as Y
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encoding
import GHC.Generics

data Problem =
  Problem
  { name         :: String
  , tests        :: [(Test,Int)]
  , requirements :: Int
  , style        :: Int
  , design       :: Int
  , testcases    :: Int }
  deriving (Show, Generic)



data ManualScore =
  ManualScore
  { requirement_score :: Int,
    style_score       :: Int,
    design_score      :: Int,
    testcases_score   :: Int,
    comments          :: String }
  deriving (Generic, Show)

instance Semigroup ManualScore where
  (<>) :: ManualScore -> ManualScore -> ManualScore
  (ManualScore r s d t c) <> (ManualScore r2 s2 d2 t2 c2) =
    (ManualScore (r + r2) (s + s2) (d + d2) (t+t2) (c ++ c2))

instance Monoid ManualScore where
  mempty :: ManualScore
  mempty = ManualScore 0 0 0 0 "" 

data AutoScore =
  Auto
  { auto_score :: [(String,Int)] } deriving (Generic, Show)

data Report =
  Report
  { problems     :: [Problem],
    maxTotal     :: Int,
    autoScores   :: [AutoScore],    --
    autoTotals   :: [Int],          -- per problem totals
    manualScores :: [ManualScore],
    manualTotal  :: Int,
    totalScore   :: Maybe Int }
  deriving (Generic,Show)

baseReport =
  Report [] 0 [] [] [] 0 Nothing

-- Fake out the test cases
instance ToJSON Test where
  toJSON _ = Null
instance FromJSON Test where
 parseJSON _ = return (1 ~?= 1)


instance ToJSON Problem where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON ManualScore where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON AutoScore where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Report where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Problem where
instance FromJSON ManualScore where
instance FromJSON AutoScore where
instance FromJSON Report where


-------------------------

p1 :: Problem
p1 = Problem "p1" [(0 ~?= 1, 2)] 1 2 3 4
