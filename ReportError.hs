-- Report an error in results.json, for Gradescope autograding
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, OverloadedStrings,
             LambdaCase, ScopedTypeVariables #-}

{-# OPTIONS_GHC -W #-}

{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

module ReportError where

import GHC.Generics
import Data.Aeson ( ToJSON(toJSON), Value(..), Object )
import Data.Aeson.Encoding ( encodingToLazyByteString, value )

import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as B
import Data.Text ( Text )
import qualified Data.Text as T
import qualified System.Directory as D


import System.Environment
import Data.Monoid

data Visibility = Hidden
                | AfterDueDate
                | AfterPublished
                | Visible
  deriving Show

instance ToJSON Visibility where
  toJSON Hidden = String "hidden"
  toJSON AfterDueDate = String "after_due_date"
  toJSON AfterPublished = String "after_published"
  toJSON Visible = String "visible"

data AGTest = AGTest deriving (Show, Generic, ToJSON )
data AGResult = AGResult { score :: Maybe Double
                         , execution_time :: Maybe Double
                         , output :: Text
                         , visibility :: Visibility
                         , tests :: [AGTest] }
            deriving (Show, Generic, ToJSON)

encodeNoNulls :: ToJSON a => a -> B.ByteString
encodeNoNulls = encodingToLazyByteString . value . noNulls . toJSON

noNulls :: Value -> Value
noNulls = everywhere' (mkT remove_null)
  where
    remove_null :: Object -> Object
    remove_null = Map.filter (\case Null -> False; _ -> True)

writeResult :: AGResult -> IO ()
writeResult result = do
  let encoded = encodeNoNulls result
  let resultsDir = "/autograder/results/"
  dirExists <- D.doesDirectoryExist resultsDir
  if dirExists then
     B.writeFile (resultsDir ++ "results.json") encoded
  else
     print result


main = do
  [error_log] <- getArgs
  error_msgs <- T.readFile error_log

  let result :: AGResult
      result = def_result { output = "Test harness failure:\n" <> error_msgs
                          , score = Just 0 }
  writeResult result
