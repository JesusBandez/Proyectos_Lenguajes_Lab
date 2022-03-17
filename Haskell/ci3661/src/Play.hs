module Play where

import AAtrees
import Match
import Data.Char (toLower, isAlpha)
import Util

data GameState = GS { played :: Int
                 , won :: Int
                 , lost :: Int
                 , streak :: Int
                 , target:: Target
                 , dict :: AA String String}

data Result = Win Target
            | Lose Target

instance Show Result where
    show (Win (Target t)) = "Got it! It was " ++ t ++ " (TODO AGREGAR EMOTICON)"
    show (Lose (Target t)) = "Bummer! It was " ++ t ++ " (TODO AGREGAR EMOTICON)"
    show _ = ""


instance Show GameState where
    show (GS p w l s _ _) = "Played: " ++ show p ++ " Won: " ++ show w ++ " Lost: " ++ show l ++ " Streak: " ++ show s

initialState :: IO GameState
initialState = pure (GS 0 0 0 0 Empty empty)

readFive :: IO String 
readFive = recursiveReadFive "" 0

{-Tiene problemas al momento de cargarse como ejecutable. El getChar espera por toda una string en vez de tomar caracter a caracter
Esto trae problemas, si se escribe algo y se pulsa enter, no hay forma de borrar esos caracteres. Actualmente se resuelve reiniciando
la string. El otro problema consiste que cada vez que se pulsa enter se salta una linea, y en el enunciado no debe pasar eso
-}
recursiveReadFive :: String -> Int -> IO String
recursiveReadFive str i = do c <- getChar                             
                             case c of
                                '\0127' | i>0 -> recursiveReadFive (init str) (i-1)
                                '\n' | i==5 -> pure str
                                     | otherwise -> recursiveReadFive "" 0
                                c | isAlpha c -> recursiveReadFive (str ++ [toLower c]) (i+1)

                                _ -> recursiveReadFive str i

play :: GameState -> IO Result
play gs = playLoop (dict gs) 1 (target gs)

playLoop :: AA String String -> Int -> Target -> IO Result
playLoop dict turn target = do putStr $ turnStartMsg turn
                               input <- readFive
                               putStr $ input ++ " "
                               case AAtrees.lookup input dict of
                                   Nothing -> do putStrLn $ wordNotValid input
                                                 playLoop dict turn target

                                   Just _ ->  let hint = match (Guess input) target in
                                                if fullmatch hint
                                                    then pure (Win target)
                                                    else if turn == Util.turns
                                                        then pure (Lose target)
                                                        else do print hint
                                                                playLoop dict (turn+1) target
                                                
turnStartMsg :: Int -> String 
turnStartMsg turn = "Guess " ++ show turn ++ "? "

wordNotValid :: String -> String
wordNotValid word = "Your guess '" ++ word ++ "' is not a valid word!"


playTheGame :: GameState -> IO()
playTheGame gs = let targetWord = Target "abaft" -- Cambiar para que se consiga de forma aleatoria con la funcion pickTarget
                     f gs =  do res <- play gs{target = targetWord, played = played gs+1}
                                print res
                                case res of
                                    Win _ -> pure gs {won = won gs + 1, streak = streak gs + 1}
                                    Lose _ -> pure gs {lost = lost gs + 1, streak = 0}
                    in do gs' <- f gs
                          print gs'
                          playAgain <- yesOrNo "Play again?"
                          if playAgain
                            then playTheGame gs'
                             else putStrLn "Bye!"