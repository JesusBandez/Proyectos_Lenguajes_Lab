{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


module Hints
    (
    ) where

import Text.Read
import Text.ParserCombinators.ReadPrec
import Data.Char

data Target = Target String deriving (Eq)
instance Show Target where 
    show (Target t) = "It was " ++ t

data Guess = Guess String deriving (Eq)
instance Show Guess where 
    show (Guess g) = "Your guess " ++ g

data Match = Absent Char| Misplaced Char | Correct Char deriving (Eq)
instance Show Match where 
    show (Absent m) = "\11035" ++ [m]
    show (Misplaced m) = "\129000" ++ [m]
    show (Correct m) = "\129001" ++ [m]

instance Read Match where
    readPrec = parens ( do                             
                            v <- get
                            c <- get
                            if (isLetter c)
                                then case v of 
                                        '\129000' -> pure (Misplaced c)
                                        '\11035' -> pure (Absent c)                                        
                                        '\129001' -> pure (Correct c)
                                        _ -> pfail
                                         
                                else pfail
                     )
                    

match :: Guess -> Target -> [Match]
match (Guess "") _ = []
match _ (Target "") = []
match (Guess (g : gs)) (Target (t: ts)) | g == t = Correct g : match (Guess gs) (Target ts)
                                        | g `elem` ts = Misplaced g : match (Guess gs) (Target ts)
                                        | otherwise = Absent g : match (Guess gs) (Target ts)

fullmatch :: [Match] -> Bool
fullmatch ms = foldr ((&&) . isCorrect) True ms
            where
                isCorrect (Correct _) = True
                isCorrect _ = False