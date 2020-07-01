-- Report an error in results.json, for Gradescope autograding

{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

module ReportError where

import Gradescope
import qualified Data.Text.IO as T
import System.Environment
import Data.Monoid

main = do
  [error_log] <- getArgs
  error_msgs <- T.readFile error_log

  let result :: AGResult
      result = def_result { output = "Test harness failure:\n" <> error_msgs
                          , score = Just 0 }
  writeResult result
