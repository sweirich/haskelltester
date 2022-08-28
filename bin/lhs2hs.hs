#!/usr/bin/env runhaskell

-- Quick translation from '>' style lhs to vanilla haskell, preserving all text as comments.

import System.Environment
import Control.Monad

data Mode = Text | Code 

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    error "Usage: lhs2hs <input>\nProduces output to stdout"
  let [file] = args
  filecontents <- readFile file
  let loop Text [] = putStrLn "-}"
      loop Code [] = return ()
      loop Text ([]:('>':' ':line):lines) = putStrLn "-}" >> putStr "\n" >> putStrLn line >> loop Code lines      
      loop Text (('>':' ':line):lines) = putStrLn "-}" >> putStr "\n" >> putStrLn line >> loop Code lines
      loop Code (('>':' ':line):lines) = putStrLn line >> loop Code lines
      loop mode ([]:lines) = putStr "\n" >> loop mode lines
      loop Text (line:lines) = putStrLn line >> loop Text lines
      loop Code (line:lines) = putStrLn "{- " >> putStrLn line >> loop Text lines
        
  loop Code (lines filecontents)
      
