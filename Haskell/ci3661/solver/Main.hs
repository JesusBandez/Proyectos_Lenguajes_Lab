module Main where

import Solve
import System.Environment (getArgs) -- Necesaria para obtener los argumentos
import Data.Char (toLower)


main = do strategy <- getStrategy
          putStrLn $ msgWelcome strategy
          is <- initialSolver strategy
          solveTheGame is

-- Usada para obtener los argumentos y conseguir la estrategia
-- que debe usar el Solver
getStrategy :: IO Solver
getStrategy = do args <- getArgs
                 case args of
                   [] ->  pure Naive                   
                   [arg]-> case map toLower arg of
                            "naive"  -> pure Naive
                            "clever" -> pure Clever
                            _        -> error msgBadArg
                   _ -> error msgBadArg

msgWelcome:: Solver -> String
msgWelcome Naive = "Naive Wordle Solver!"
msgWelcome Clever = "Clever Wordle Solver!"

msgBadArg :: String
msgBadArg = "Debe ingresar como argumento naive o clever"