module Main where

import Play
import Util ( dictionary, loadDictionary ) 
import Match

import System.IO (stdout, hSetBuffering, BufferMode (NoBuffering) )

main = do hSetBuffering stdout NoBuffering -- Importante para mostrar de manera correcto los putStr
          gs <- initialState
          dictionary <- loadDictionary dictionary
          playTheGame gs{dict = dictionary}

