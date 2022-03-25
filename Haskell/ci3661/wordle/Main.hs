module Main where

import Play ( initialState, playTheGame )


main :: IO ()
main = do putStrLn "Kinda Wordle!"
          gs <- initialState          
          playTheGame gs
