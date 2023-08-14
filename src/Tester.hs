{-# LANGUAGE DuplicateRecordFields, RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}

module Tester (module TestData, testerMain, fromQuickCheck, withName, gradeScopeMain) where


import Data.List (intercalate)
import Data.Monoid
import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (when)

import qualified System.Directory as D
import System.IO
import System.Environment
import System.Exit

import Test.HUnit      hiding (test)
import Test.QuickCheck hiding (elements, total)
----
import TestData
import qualified Gradescope as G
import qualified Data.Text  as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as Aeson

import Data.Maybe ( catMaybes )

----


fromQuickCheck :: Test.QuickCheck.Testable a => a -> Test
fromQuickCheck = TestCase . assert . quickCheckResult

instance Assertable Result where
  assert (Success {}) = assert True
  assert _            = assert False

instance Semigroup Problem where
  p1 <> p2 = 
       Problem 
            { name         = name         p1 ++ name         p2
            , tests        = tests        p1 ++ tests        p2
            , requirements = requirements p1 +  requirements p2
            , style        = style        p1 +  style        p2
            , design       = design       p1 +  design       p2
            , testcases    = testcases    p1 +  testcases    p2 }

  
instance Monoid Problem where
  mempty          =
    Problem { name            = ""
            , tests           = []
            , requirements    = 0
            , style           = 0
            , design          = 0
            , testcases = 0 }

 


withName :: String -> Problem -> Problem
withName s p = p{name = s}

testPoints :: Problem -> Int
testPoints = sum . map snd . tests

manualPoints :: Problem -> Int
manualPoints p = requirements p
        + style p
        + design p
        + testcases p

-- total # of points available for the problem
total :: Problem -> Int
total p = testPoints p + manualPoints p


doTest :: (Test,Int) -> IO (String,Int)
doTest (test, points) = do
  Counts{..} <- runTestTT test
  let correct = cases - errors - failures
      pct     = fromIntegral correct / fromIntegral cases :: Double
      ptFrac  = fromIntegral points * pct
      ptRound = round ptFrac
      ptFloor = floor ptFrac
      -- Don't allow a perfect score unless all the test cases really pass
      pt | ptRound == points && correct /= cases = ptFloor
         | otherwise                             = ptRound
  return (name test ++ ": " ++ show pt , pt)
  where
    name (TestLabel s _) = s
    name _               = "unnamed test"

autoProblem :: Problem -> IO AutoScore
autoProblem p@Problem{..} = do
  Auto <$> mapM doTest tests


outOf :: Problem -> ManualScore
outOf p = ManualScore (requirements p) (style p) (design p) (testcases p) ""


createReport :: [Problem] -> IO Report
createReport ps = do
  auto <- mapM autoProblem ps
  let as = map (sum . map snd) (map auto_score auto) ::  [Int]
  return $ baseReport { problems     = ps,
                        maxTotal     = sum (map total ps),
                        autoScores   = auto,
                        autoTotals   = as,
                        manualScores = map outOf ps,
                        manualTotal  = undefined }


writeScoresFile :: String -> [(Problem,AutoScore)] -> IO ()
writeScoresFile scoresFile problems =
  withFile scoresFile WriteMode $ \s -> do
      let saveAutoScore (problem, score) = hPutStrLn s
                                         $ show (auto_score score) ++ "|0|" ++ name problem
      mapM_ saveAutoScore problems

{-
writeFeedbackFile :: String -> [Problem] -> IO ()
writeFeedbackFile feedbackFile problems = do
  feedbackExists <- D.doesFileExist feedbackFile
  if feedbackExists then return
  withFile feedbackFile WriteMode $ \f -> do

testerMain2 :: [Problem] -> IO ()
testerMain2 problems = do
  args <- getArgs
  case args of
    [ scoresFile, feedbackFile ] -> do
        results <- mapM autoProblem problems
        writeScoresFile scoresFile results

      withFile scoresFile WriteMode $ \s -> do

        feedbackExists <- D.doesFileExist feedbackFile
        when feedbackExists .
          D.renameFile feedbackFile $ feedbackFile ++ ".bak"
        withFile feedbackFile WriteMode $ \f -> do
          report <- createReport
-}

-----------------------------------------------------------

grade :: Handle -> Handle -> Problem -> IO Int
grade s f p@Problem{..} = do
  hPutStrLn f $ "* " ++ name ++ " [" ++ show totalPoints ++ " points]"
  autoScore <- if null tests
    then do
      saveAutoScore (0 :: Int)
      return 0
    else do
      (autoInfo, autoScore) <-  (intercalate "  " *** sum) . unzip
                            <$> mapM doTest tests
      hPutStrLn  f $  "** Auto grade: " ++ autoInfo
                   ++ "  (Total: "
                   ++ show autoScore ++ "/" ++ show (testPoints p)
                   ++ ")"
      saveAutoScore autoScore
      return autoScore
  hPutStrLn f $ "** Manual grade:  /" ++ show (manualPoints p)
  scoreSection "Correctness"  requirements
  scoreSection "Test case"    testcases
  scoreSection "Style"        style
  scoreSection "Design"       design
  hPutStrLn f "** Comments:"
  hPutStrLn f ""
  return autoScore
  where
    totalPoints = total p

    saveAutoScore score = hPutStrLn s $ show score ++ "|0|" ++ name

    scoreSection what points = when (points > 0) .
        hPutStrLn f $ "*** " ++ what ++ " points: /" ++ show points

------------------------------------------

gradescopeProblem :: Problem -> IO G.AGTest
gradescopeProblem p@Problem{..} = do

  (autoInfo, autoScore) <-  (intercalate "  " *** sum) . unzip
                            <$> mapM doTest tests
  
  return $ G.AGTest {
      G.score      = Just (fromIntegral autoScore)
    , G.max_score  = Just (fromIntegral (testPoints p))
    , G.number     = T.pack ("(" ++ name)
    , G.output     = T.pack autoInfo
    , G.visibility = G.AfterPublished
    }

gradeScopeMain :: [Problem] -> IO ()
gradeScopeMain problems = do
  ts <- mapM gradescopeProblem problems
  let scores = Prelude.map (G.score :: G.AGTest -> Maybe Double) ts
  let record = G.AGResult {
      G.result_score = Just (sum . catMaybes $ scores)
    , G.execution_time = Nothing
    , G.output      = "CIS 5520 HUnit test results"
    , G.visibility  = G.AfterPublished
    , G.tests = ts
  }
  G.recordResult record
  
---------------------------------------------------------------


testerMain :: [Problem] -> IO ()
testerMain problems = do
  args <- getArgs
  case args of
    [ scoresFile, feedbackFile ] ->
      withFile scoresFile WriteMode $ \s -> do
        feedbackExists <- D.doesFileExist feedbackFile
        when feedbackExists .
          D.renameFile feedbackFile $ feedbackFile ++ ".bak"
        withFile feedbackFile WriteMode $ \f -> do
          hPutStrLn f "Grade Report"
          hPutStrLn f "------------"
          hPutStrLn f ""
          autos <- mapM (grade s f) problems
          hPutStrLn f $ "* Final grade: /"  ++ show (sum $ map total problems)
          hPutStrLn f $ "** Total auto grade: " ++ show (sum autos) ++ "/" ++ show (sum $ map testPoints problems)
          hPutStrLn f $ "** Total manual grade: /" ++ show (sum $ map manualPoints problems)

    _ -> do
      hPutStrLn stderr "Usage: TestMain <scores-file> <feedback-file>"
      exitFailure
