module Main where

import Play
import Util 
import Match

import System.IO (stdout, stdin, hSetBuffering, BufferMode (NoBuffering) )


main = do hSetBuffering stdout NoBuffering -- Importante para mostrar de manera correcto los putStr
          hSetBuffering stdin NoBuffering
          yesOrNo "Play Again?"



{-
main = do hSetBuffering stdout NoBuffering -- Importante para mostrar de manera correcto los putStr
          hSetBuffering stdin NoBuffering
          gs <- initialState
          dictionary <- loadDictionary dictionary
          playTheGame gs{dict = dictionary}

-}