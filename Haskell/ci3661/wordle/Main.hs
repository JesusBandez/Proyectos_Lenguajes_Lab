module Main where

import Play


main = do putStrLn "Kinda Wordle!"
          gs <- initialState          
          playTheGame gs
