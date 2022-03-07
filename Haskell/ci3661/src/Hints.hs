module Hints
    (
    ) where

data Target = Target String 

data Guess = Guess String 

data Match = Absent Char| Misplaced Char | Correct Char


match :: Guess -> Target -> [Match]
match g t = undefined