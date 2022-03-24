{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Solve
-- Authors     :  Jesus Bandez 17-10046
--                Mariangela Rizzo 17-10538
-- Portability :  portable
-----------------------------------------------------------------------------

module Solve (
    -- * Custom types
    Solver(..),
    SolverState(..),

    -- * Constructor 
    initialState,

    -- * Main
    solveTheGame,

#if defined(TESTING)
    -- * Internals
    solveTheGamePlayAgain,
    solveTheGameRec,
    pickSuggest,
    loadPossibleList,
    readHint, 
    msgInsertHint,
    sieve, 
    naive,
    pickRandom,
    clever, 
#endif
) where
import Util (loadDictionary, dictionary, yesOrNo)
import AAtrees ( empty, lookup, AA, insert, fromList )
import Match

import Control.Monad (unless, when)
import System.Random (Random (randomRIO))
import Data.List ( group, sort )
import Prelude hiding (lookup)
import Data.Maybe ()
import System.IO (stdout, stdin, hSetBuffering, hSetEcho,BufferMode (NoBuffering, LineBuffering) )
import qualified Data.Foldable
import Text.Read (readMaybe)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import Play (initialState)

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
solveTheGame ss = do hSetBuffering stdout NoBuffering -- Desactivar el buffering
                     hSetBuffering stdin NoBuffering
                     solveTheGameRec ss 1
                     solveTheGamePlayAgain ss
                     hSetBuffering stdout LineBuffering  -- Activar el buffering
                     hSetBuffering stdin LineBuffering

-- Funcion auxiliar que reinicia o culmina el solver acorde al input del usuario
solveTheGamePlayAgain :: SolverState -> IO ()
solveTheGamePlayAgain ss = do playAgain <- yesOrNo "Solve another"                              
                              if playAgain 
                                  then do solveTheGameRec ss 1
                                          solveTheGamePlayAgain ss
                                  else putStrLn "Bye!"

solveTheGameRec :: SolverState -> Int -> IO ()
solveTheGameRec ss n = do
    print ss
    if remaining ss == 1
        then pure()
        else if n == 7
              then putStrLn "You lost! \129325 "
              else do
               putStr $ msgInsertHint n
               hint <- readHint n
               let ss' = loadPossibleList ss hint n in
                do ss'' <- pickSuggest hint ss'
                   solveTheGameRec ss'' (n+1)

-- Funcion auxiliar para elegir una sugerencia a mostrar al usuario acorde al metodo pasado como parametro
pickSuggest :: [Match] -> SolverState -> IO SolverState
pickSuggest ms ss = case strategy ss of
    Naive -> naive ms ss
    Clever -> clever ms ss

-- Funcion auxiliar que crea la lista de todas las palabras posibles desde el diccionario (si se acaba de arrancar/reiniciar el solver)
loadPossibleList :: SolverState -> [Match] -> Int -> SolverState
loadPossibleList ss ms 1 = ss {possible = sieve ms (Data.Foldable.toList (dict ss))}
loadPossibleList ss ms n = ss

-- Funcion auxiliar para imprimir pistas
msgInsertHint :: Int -> String
msgInsertHint 5 = "Hint 5? \128533 "
msgInsertHint 6 = "Hint 6? \129296 "
msgInsertHint n = "Hint " ++ show n ++ "? \129300 "

-- Funcion auxiliar que lee la pista insertada en formato de Math por el usuario
readHint :: Int -> IO [Match]
readHint n = do hSetEcho stdin True
                input <- getLine
                case readMaybe input :: Maybe [Match] of
                 Just hint -> pure hint
                 Nothing ->  do putStr $ msgInsertHint n
                                readHint n

{-Funcion que reduce una lista de posibilidades acorde al match pasado como parámetro
Ejemplo: sieve [Misplaced 'i', Absent 'r', Absent 'a', Correct 't', Absent 'e'] ["absme", "abste", "ugoto", "ogoti", "uimto", "impto"] = ["ogoti","uimto"] -}
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

{-Funcion que hace uso de random para elegir una sugerencia al usuario de la lista de posibles palabras en el estado pasado como parametro-}
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
clever ms (SS _ p r d st) = let s = maxScoreWord possibilities in
                            do return (SS s possibilities (length possibilities) d st)
                            where
                                possibilities = sieve ms p

{- Obtiene la palabra de maximo puntaje de una lista
maxScoreWord ["Patas", "Carro", "Gatos", "Perro"] = "Carro"
-}
maxScoreWord :: [String] -> String
maxScoreWord ws = fst $ maximumBy (comparing snd) (scoreAllWords ws)

{- Toma una lista de palabras y las compara a todas entre ellas:
scoreAllWords ["Patas", "Carro", "Gatos", "Perro"] = [("Patas",3),("Carro",3),("Gatos",2),("Perro",2)]-}
scoreAllWords :: [String] -> [([Char], Int)]
scoreAllWords ws = map (scoreAWord ws) ws

{- Toma una palabra y consigue su puntacion comparandola con todas las demas palabras en una lista
scoreAWord ["Patas", "Carro", "Gatos", "Perro"] "Perro" = ("Perro",2)
-}
scoreAWord :: Foldable t => t String -> [Char] -> ([Char], Int)
scoreAWord ws w = foldr step (w, -1) ws
    where step w (wcomp, n) = (wcomp, n + compareWords w wcomp)

{- Toma dos palabras las compara de forma que si una palabra tiene un caracter en la posicion i
y la otra palabra tiene el mismo caracter en la posicion i entonces retorna 1, en caso contrario retorna 0
compareWords "Perro" "Gatos" = 0
compareWords "Perro" "Patas" = 1
compareWords "Perro" "Carro" = 1
-}
compareWords :: String -> String -> Int
compareWords w1 w2 = snd $ foldl score (w2, 0) w1
            where 
                score ("" , n) c1 = ("", n)
                score (c2:w2 , n) c1  = if c1 == c2
                                            then (w2, 1)
                                            else (w2, n)



{-
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



-}
