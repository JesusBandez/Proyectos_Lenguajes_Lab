{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
 {-# LANGUAGE UnicodeSyntax #-}

module Hints
    (
    ) where

data Target = Target String deriving (Eq)
instance Show Target where 
    show (Target t) = "It was " ++ t

data Guess = Guess String deriving (Eq)
instance Show Guess where 
    show (Guess g) = "Your guess " ++ g

data Match = Absent Char| Misplaced Char | Correct Char deriving (Eq)
instance Show Match where 
    show (Absent m) = "⬛ " ++ (m: "-Absent")
    show (Misplaced m) = "🟨 " ++ (m: "-Misplaced")
    show (Correct m) = "🟩 " ++ (m: "-Correct")


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