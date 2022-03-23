module Main where


import Util 
import Match
import Solve 



main = do is <- initialSolver Naive     
          solveTheGame is
