module Main where

import Solve
import System.Environment (getArgs)




main = do strategy <- getStrategy
          putStrLn $ msgWelcome strategy
          is <- initialSolver strategy
          solveTheGame is

getStrategy :: IO Solver
getStrategy = do args <- getArgs
                 case args of
                   [] ->  pure Naive                   
                   [arg]-> case arg of
                            "naive"  -> pure Naive
                            "clever" -> pure Clever
                            _        -> error msgBadArg
                   _ -> error msgBadArg

msgWelcome:: Solver -> String
msgWelcome Naive = "Naive Wordle Solver!"
msgWelcome Clever = "Clever Wordle Solver!"

msgBadArg :: String
msgBadArg = "Debe ingresar como argumento naive o clever"