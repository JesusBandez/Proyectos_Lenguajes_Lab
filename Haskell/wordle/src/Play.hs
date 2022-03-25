{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Play
-- Authors     :  Jesus Bandez 17-10046
--                Mariangela Rizzo 17-10538
-- Portability :  portable
-----------------------------------------------------------------------------

module Play (
    -- * Construccion
    initialState,

    -- * Main
    playTheGame

#if defined(TESTING)
    -- * Internals
    readFive,
    recursiveReadFive,
    play,
    playLoop,
    turnStartMsg,
    wordNotValid,
    pickTarget
#endif
) where

import AA ( lookup, AA )
import Match ( fullmatch, match, Guess(Guess), Target(..) )
import Data.Char (toLower, isAlpha, isAscii)
import Util ( turns, yesOrNo, loadDictionary, dictionary )
import System.IO (stdout, stdin, hSetBuffering, hSetEcho,BufferMode (NoBuffering, LineBuffering), hGetBuffering, hGetEcho )
import System.Random (Random(randomRIO))

{-Tipo de dato que representa el estado actual del juego-}
data GameState = GS { played :: Int
                 , won :: Int
                 , lost :: Int
                 , streak :: Int
                 , target:: Target
                 , dict :: AA String String}
instance Show GameState where
    show (GS p w l s _ _) = "Played: " ++ show p ++ " Won: " ++ show w ++ " Lost: " ++ show l ++ " Streak: " ++ show s

{-Resultado de una partida-}
data Result = Win Target
            | Lose Target
instance Show Result where
    show (Win (Target t)) = "Got it! It was \171" ++ t ++ "\187 \128526"
    show (Lose (Target t)) = "Bummer! It was \171" ++ t ++ "\187 \128128"
    show _ = ""

{-Funcion que inicia el primer estado del juego-}
initialState :: IO GameState
initialState = do dictionary <- loadDictionary dictionary
                  pure (GS 0 0 0 0 Empty dictionary)

{-Lee la conjetura del usuario-}
readFive :: IO String 
readFive = do recursiveReadFive "" 0

recursiveReadFive :: String -> Int -> IO String
recursiveReadFive str i = do c <- getChar                     
                             case c of
                                '\0127'| i > 0     -> do putStr "\b \b" 
                                                         recursiveReadFive (init str) (i-1)
                                '\n'   | i == 5    -> pure str
                                c      | isAlpha c && isAscii c && i < 5 -> do putChar c
                                                                               recursiveReadFive (str ++ [toLower c]) (i+1)
                                _ -> recursiveReadFive str i


{- Funciones que ejecutan una sesion del juego-}
play :: GameState -> IO Result
play gs = do outBuff <- hGetBuffering stdout
             inBuff <- hGetBuffering stdin
             echo <- hGetEcho stdin
             hSetBuffering stdout NoBuffering
             hSetBuffering stdin NoBuffering
             hSetEcho stdin False

             res <- playLoop (dict gs) 1 (target gs)
             
             hSetBuffering stdout outBuff 
             hSetBuffering stdin inBuff
             hSetEcho stdin echo
             pure res

{- Bucle donde se piden las palabras al jugador y se comparan con el
target-}
playLoop :: AA String String -> Int -> Target -> IO Result
playLoop dict turn target = do putStr $ turnStartMsg turn
                               input <- readFive
                               putStr " "
                               case AA.lookup input dict of
                                   Nothing -> do putStrLn $ wordNotValid input
                                                 playLoop dict turn target

                                   Just _ ->  let hint = match (Guess input) target in
                                                do print hint
                                                   if fullmatch hint
                                                      then pure (Win target)
                                                      else if turn == Util.turns
                                                          then pure (Lose target)
                                                          else playLoop dict (turn+1) target
-- Funcion de soporte para imprimir string                                       
turnStartMsg :: Int -> String 
turnStartMsg turn = "Guess " ++ show turn ++ "? "
-- Funcion de soporte para imprimir string
wordNotValid :: String -> String
wordNotValid word = "Your guess '" ++ word ++ "' is not a valid word!"

{-Bucle principal del juego-}
playTheGame :: GameState -> IO()
playTheGame gs =  do targetWord <- pickTarget $ dict gs 
                     let f gs = do res <- play gs{target = targetWord, played = played gs+1}
                                   print res
                                   case res of
                                        Win _ -> pure gs {won = won gs + 1, streak = streak gs + 1}
                                        Lose _ -> pure gs {lost = lost gs + 1, streak = 0} in
                       do gs' <- f gs
                          print gs'
                          playAgain <- yesOrNo "Play again?"
                          if playAgain
                            then playTheGame gs'
                             else putStrLn "Bye!"

{- Selecciona una palabra secreta al azar del arbol recorriendolo una sola vez -}
pickTarget :: Ord k => AA k String -> IO Target
pickTarget tree =do tar <- foldr step (pure (Match.Empty, 0)) tree
                    pure $ fst tar
                where
                    step word acc = do  (choosen, count) <- acc
                                        r <- randomRIO(0, count::Int)
                                        if r == 0
                                            then pure (Target word, count + 1)
                                            else pure (choosen, count + 1)
