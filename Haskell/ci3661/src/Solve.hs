{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solve where
import Util ()
import AAtrees ( empty, lookup, AA, insert, fromList )
import Match

import System.Random (Random (randomRIO))
import Data.List ( group, sort )
import Prelude hiding (lookup)
import Data.Maybe

data Solver = Naive | Cleverdata

data SolverState = SS { suggestion :: String,
                        possible   :: [String],
                        remaining  :: Int,
                        dict       :: AA String String,
                        strategy   :: Solver }
instance Show SolverState where
    show (SS s p r _ _) | s == "" = "There are " ++ show r ++ " possible words."
                        | otherwise = "There are " ++ show r ++ " possible words. I suggest \171" ++ s ++ "\187"

{-Funcion que inicia el primer estado del asistente-}
initialSolver :: Solver -> IO SolverState
initialSolver s = pure (SS "" [] 0 empty s)

solveTheGame :: SolverState -> IO()
solveTheGame ss = undefined

{-Funcion que reduce una lista de posibilidades acorde al match pasado como parámetro
Ejemplo: sieve [Misplaced 'i', Absent 'r', Absent 'a', Correct 't', Absent 'e'] ["absme", "abste", "ugoto", "ogoti", "uimto", "impto"] = ["ogoti","uimto"] -}
sieve :: [Match] -> [String] -> [String]
sieve [] (s : ss) = s : ss
sieve m [] = []
sieve (m : ms) (ls : lss) = reduce (isValid (m : ms)) (ls : lss)
                            where
                                isValid [] _ _ = True
                                isValid _ "" _ = True
                                isValid (Correct cs : cms) (s : ss) fullWord    | cs == s = isValid cms ss fullWord
                                                                                | otherwise = False
                                isValid (Absent as: ams) (s : ss) fullWord      | as `elem` fullWord = False
                                                                                | otherwise = isValid ams ss fullWord
                                isValid (Misplaced mps: mpms) (s : ss) fullWord | mps /= s && mps `elem` fullWord = isValid mpms ss fullWord
                                                                                | otherwise = False

reduce :: (a -> a -> Bool) -> [a] -> [a]
reduce f xs = [x | x <- xs, f x x]

naive :: [Match] -> SolverState -> IO SolverState
naive [] ss = pure ss
naive (m : ms) (SS _ p r d st) = do
                                    s <- pickRandom possibilities
                                    return (SS s possibilities r d st)
                                    where
                                        possibilities = sieve (m : ms) p
pickRandom :: [a] -> IO a
pickRandom xs = (xs !!) <$> randomRIO (0, length xs - 1)

clever :: [Match] -> SolverState -> IO SolverState
clever _ _ = undefined

{-Indica la frecuencia de cada letra en una lista de palabras
Ejemplo: freqL ["ugito", "ogomo", "asbte", "absme"] = [('a',2),('b',2),('e',2),('g',2),('i',1),('m',2),('o',4),('s',2),('t',2),('u',1)]-}
freqL :: [String] -> AA Char Int
freqL ws = fromList $ freqL' $ concat ws
            where 
                freqL' a = map (\x -> (head x, length x)) $ group $ sort a

{-Calcula la puntuacion de cada palabra en una lista de palabras acorde a las letras que posee 
scoreWords ["ugito", "ogomo", "asbte", "absme"] = [("absme",10),("asbte",10),("ogomo",16),("ugito",10)]-}
scoreWords :: [String] -> AA String Int
scoreWords ws = fromList $ zip ws $ map (scoreWord (freqL $ ws)) ws

scoreWord :: AA Char Int -> String -> Int
scoreWord _ [] = 0
scoreWord aa (w : ws) = fromMaybe 0 (lookup w aa) + scoreWord aa ws