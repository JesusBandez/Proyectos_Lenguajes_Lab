{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Match
-- Authors     :  Jesus Bandez 17-10046
--                Mariangela Rizzo 17-10538
-- Portability :  portable
-----------------------------------------------------------------------------


module Match (
      --Custom types
      Target(..),
      Guess(..),
      Match(..),          -- instance Eq,Show,Read


      -- * Construccion
      match,
      fullmatch

#if defined(TESTING)
      -- * Internals
      matchLoop
#endif
    
    ) where

import Text.Read ( Read(readPrec), parens, get, pfail )
import Data.Char ( isLetter )

{-Tipo de datos Target. Se permite el constructor Empty porque
un nuevo juego se debe inicializar sin una palabra Target-}
data Target = Target String | Empty 
    deriving (Eq)    
instance Show Target where 
    show (Target t) = "It was " ++ t
    show Empty = ""

data Guess = Guess String deriving (Eq)
instance Show Guess where 
    show (Guess g) = "Your guess " ++ g

data Match = Absent Char
             | Misplaced Char 
             | Correct Char deriving (Eq)
             
instance Show Match where 
    show (Absent m) = "\11035" ++ [m]
    show (Misplaced m) = "\129000" ++ [m]
    show (Correct m) = "\129001" ++ [m]
instance Read Match where
    readPrec = parens ( do                             
                            v <- get
                            c <- get
                            if isLetter c
                                then case v of 
                                        '\129000' -> pure (Misplaced c)
                                        '\11035' -> pure (Absent c)                                        
                                        '\129001' -> pure (Correct c)
                                        _ -> pfail
                                         
                                else pfail
                     )
                    
{-Comparan los caracteres de cada palabra y genera una lista de match con los resultados
de la comparacion
match (Guess "panic") (Target "poise") = [🟩p,⬛a,⬛n,🟨i,⬛c]
-}
match :: Guess -> Target -> [Match]
match (Guess g) Empty = []
match (Guess g) (Target t) = matchLoop g t t


matchLoop :: String  -> String -> String  -> [Match]
matchLoop "" _ _ = []
matchLoop _ "" _ = []
matchLoop (g : gs) (t: ts) fullTarget | g == t = Correct g : matchLoop  gs ts fullTarget
                                        | g `elem` fullTarget = Misplaced g : matchLoop  gs ts fullTarget
                                        | otherwise = Absent g : matchLoop  gs ts fullTarget

{-Reduce una lista de match para comprobar que todos sean Correct. 
Retorna true en tal caso, false en cualquier otro -}
fullmatch :: [Match] -> Bool
fullmatch = foldr ((&&) . isCorrect) True
            where
                isCorrect (Correct _) = True
                isCorrect _ = False