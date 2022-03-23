{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solve where
import Util (loadDictionary, dictionary)
import AAtrees ( empty, lookup, AA, insert, fromList )
import Match

import System.Random (Random (randomRIO))
import Data.List ( group, sort )
import Prelude hiding (lookup)
import Data.Maybe
import System.IO (stdout, stdin, hSetBuffering, hSetEcho,BufferMode (NoBuffering) )
import qualified Data.Foldable
import Text.Read (readMaybe)


data Solver = Naive | Clever

data SolverState = SS { suggestion :: String,
                        possible   :: [String],
                        remaining  :: Int,
                        dict       :: AA String String,
                        strategy   :: Solver }
instance Show SolverState where
    show (SS "" p r _ _) = "There are " ++ show r ++ " possible words."
    show (SS s p r _ _) 
                    | r > 1 = "There are " ++ show r ++ " possible words. I suggest \171" ++ s ++ "\187"
                    | otherwise = "It must be \171" ++ s ++ "\187"
{-Funcion que inicia el primer estado del asistente-}
initialSolver :: Solver -> IO SolverState
initialSolver s = do dict <- loadDictionary dictionary
                     pure (SS "" [] (length dict) dict s)

solveTheGame :: SolverState -> IO()
solveTheGame ss = do hSetBuffering stdout NoBuffering -- Importante para mostrar de manera correcta los putStr
                     hSetBuffering stdin NoBuffering
                     solveTheGameRec ss 1
-- Esta fallando porque la lista possible del SolverState SIEMPRE esta vacia. Cuando se llena?

solveTheGameRec :: SolverState -> Int -> IO ()
solveTheGameRec ss n = do print ss
                          putStr $ msgInsertHint n                          
                          hint <- readHint n
                          let ss' = loadPossibleList ss hint n in
                            do ss'' <- pickSuggest hint ss'                                                      
                               if n == 6
                                then print "You lost! \129325 "
                                else solveTheGameRec ss'' (n+1)
                          

pickSuggest :: [Match] -> SolverState -> IO SolverState
pickSuggest ms ss = case strategy ss of
    Naive -> naive ms ss
    Clever -> clever ms ss

loadPossibleList :: SolverState -> [Match] -> Int -> SolverState
loadPossibleList ss ms 1 = ss {possible = sieve ms (Data.Foldable.toList (dict ss))}
loadPossibleList ss ms n = ss
{-Funcion que reduce una lista de posibilidades acorde al match pasado como parámetro
Ejemplo: sieve [Misplaced 'i', Absent 'r', Absent 'a', Correct 't', Absent 'e'] ["absme", "abste", "ugoto", "ogoti", "uimto", "impto"] = ["ogoti","uimto"] -}

msgInsertHint :: Int -> String 
msgInsertHint 5 = "Hint 5? \128533 "
msgInsertHint 6 = "Hint 5? \129296 "
msgInsertHint n = "Hint " ++ show n ++ "? \129300 "

readHint :: Int -> IO [Match]
readHint n = do input <- getLine
                case readMaybe input :: Maybe [Match] of
                 Just hint -> pure hint
                 Nothing ->  do putStr $ msgInsertHint n
                                readHint n

    

sieve :: [Match] -> [String] -> [String]
sieve [] s = s
sieve m [] = []
sieve ms ls = reduce (isValid ms) ls
                            where
                                reduce f xs = [x | x <- xs, f x x]
                                isValid [] _ _ = True
                                isValid _ "" _ = True
                                isValid (Correct cs : cms) (s : ss) fullWord    | cs == s = isValid cms ss fullWord
                                                                                | otherwise = False
                                isValid (Absent as: ams) (s : ss) fullWord      | as `elem` fullWord = False
                                                                                | otherwise = isValid ams ss fullWord
                                isValid (Misplaced mps: mpms) (s : ss) fullWord | mps /= s && mps `elem` fullWord = isValid mpms ss fullWord
                                                                                | otherwise = False




naive :: [Match] -> SolverState -> IO SolverState
naive [] ss = pure ss
naive ms (SS _ p r d st) = do
                            s <- pickRandom possibilities
                            return (SS s possibilities (length possibilities) d st)
                            where
                                possibilities = sieve ms p
pickRandom :: [a] -> IO a
pickRandom [] = error "Lista vacia"
pickRandom (x:xs) = do tar <- foldr step (pure (x, 0)) xs
                       pure $ fst tar
                    where
                        step x acc = do (choosen, count) <- acc
                                        r <- randomRIO(0, count::Int)
                                        if r == 0
                                            then pure (x, count + 1)
                                            else pure (choosen, count + 1)


clever :: [Match] -> SolverState -> IO SolverState
clever [] ss = pure ss
clever ms (SS _ p r d st) = do
                            s <- maxWord possibilities
                            return (SS s possibilities (length possibilities) d st)
                            where
                                possibilities = sieve ms p

{-Indica la frecuencia de cada letra en una lista de palabras
Ejemplo: freqL ["ugito", "ogomo", "asbte", "absme"] = [('a',2),('b',2),('e',2),('g',2),('i',1),('m',2),('o',4),('s',2),('t',2),('u',1)]-}
freqL :: [String] -> AA Char Int
freqL ws = fromList $ freqL' $ concat ws
            where
                freqL' a = map (\x -> (head x, length x)) $ group $ sort a

-- !!! Comenté esta función porque sigue la idea de maxWord, solo que retorna un arbol que tiene la palabra como clave y la puntuacion de la palabra como valor. 
-- !!! Cuando hago maximum o un foldl para obtener el valor maximo, me lo da, pero solo el entero, no la clave. 
-- !!! Si consideras que se puede de alguna manera, puedes usar esta función, pero también vi innecesario pasar la lista a arbol si de la misma lista con la idea que llevaba puedo obtener lo que quiero.
{-Calcula la puntuacion de cada palabra en una lista de palabras acorde a las letras que posee 
scoreWords ["ugito", "ogomo", "asbte", "absme"] = [("absme",10),("asbte",10),("ogomo",16),("ugito",10)]-}
-- scoreWords :: [String] -> AA String Int
-- scoreWords ws = fromList $ zip ws $ map (scoreWord (freqL ws)) ws

maxWord :: [String] -> IO String
maxWord ws = pure $ fst $ maximumWord $ zip ws $ map (scoreWord (freqL ws)) ws
            where 
                maximumWord = foldr1 (\(w1, v1) (w2, v2) -> if v1 >= v2 then (w1, v1) else (w2, v2))

scoreWord :: AA Char Int -> String -> Int
scoreWord _ [] = 0
scoreWord aa (w : ws) = fromMaybe 0 (lookup w aa) + scoreWord aa ws