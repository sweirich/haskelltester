-- Report an error in results.json, for Gradescope autograding
{-# LANGUAGE DuplicateRecordFields #-}
-- Report an error in results.json, for Gradescope autograding
{-# LANGUAGE OverloadedStrings #-}

module ReportError where

import Data.Monoid
import Data.Text.IO qualified as T
import Gradescope
import System.Environment

main = do
  [error_log] <- getArgs
  error_msgs <- T.readFile error_log

  let result :: AGResult
      result =
        def_result
          { output = "Test harness failure:\n" <> error_msgs,
            result_score = Just 0
          }
  writeResult result
