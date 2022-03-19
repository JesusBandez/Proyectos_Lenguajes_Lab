module Main where

import Play
import Util 
import Match



main = do gs <- initialState
          dictionary <- loadDictionary dictionary
          playTheGame gs{dict = dictionary}
