{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Util
-- Authors     :  Jesus Bandez 17-10046
--                Mariangela Rizzo 17-10538
-- Portability :  portable
-----------------------------------------------------------------------------

module Util (
    -- * Const
    turns,
    dictionary,

    -- * Filtro
    fiveLetterWords,

    -- * Constructor
    loadDictionary,

    -- * Condicional
    yesOrNo,

#if defined(TESTING)
    -- * Internals
    yesOrNoLoop
#endif
) where
import Data.Char (isAlpha, isAsciiLower)
import AA ( empty, insert, AA )
import System.IO (stdout, stdin, hSetBuffering, hSetEcho,BufferMode (NoBuffering, LineBuffering), hGetBuffering, hGetEcho )

{-Funcion constante con el numero de turnos para el juego-}
turns :: Int
turns = 6

{-Ruta del archivo con las palabras a usar-}
dictionary :: FilePath 
dictionary = "/usr/share/dict/american-english"

{- Filtra todas las palabras quedandose con aquellas que sean de tamanio 5 y que no sean un sustantivo propio
fiveLetterWords ["Ramon", "hola", "zip's", "extravagante", "gatos"] = ["gatos"]
 -}
fiveLetterWords :: [String] -> [String]
fiveLetterWords = filter isValid
                where
                    isValid "" = False
                    isValid word = fst (reduce word) && snd (reduce word) == 5
                        where
                            reduce = foldl go (True, 0)
                            go (False, n) _ = (False, n)
                            go (True, 0) c                                
                                | isValidChar c = (True, 1)
                                | otherwise = (False, 1)
                            go (True, n) c
                                | n <= 5 && isValidChar c = (True, n+1)
                                | otherwise = (False, n+1)
                            isValidChar c = isAsciiLower c && isAlpha c



{-Carga un arbol AA con las palabras del diccionario que cumplan con el criterio de ser 
una palabra de cinco letras-}
loadDictionary :: FilePath -> IO (AA String String) 
loadDictionary x = do l <- readFile x
                      pure $ foldr step empty $ fiveLetterWords $ lines l
                        where
                            step word tree  = insert word word tree

{-Muestra un mensaje arbitrario y luego pide pide una decision afirmativa o negativa-}
yesOrNo :: String -> IO Bool 
yesOrNo mesagge = do outBuff <- hGetBuffering stdout
                     inBuff <- hGetBuffering stdin
                     echo <- hGetEcho stdin
                     hSetBuffering stdout NoBuffering
                     hSetBuffering stdin NoBuffering
                     hSetEcho stdin False

                     putStr $ mesagge ++ " (y/n)?"
                     ans <- yesOrNoLoop
                     
                     hSetBuffering stdout outBuff
                     hSetBuffering stdin inBuff
                     hSetEcho stdin echo
                     pure ans

yesOrNoLoop :: IO Bool
yesOrNoLoop = do c <- getChar 
                 case c of
                         'y' -> do putStrLn [c]
                                   pure True
                         'n' -> do putStrLn [c]
                                   pure False
                         _ -> yesOrNoLoop


