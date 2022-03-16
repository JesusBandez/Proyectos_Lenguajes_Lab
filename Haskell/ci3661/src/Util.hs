module Util where
import Data.Char (isUpper, isAlpha)
import AAtrees ( empty, insert, AA )

{-Funcion constante con el numero de turnos para el juego-}
turns :: Int
turns = 6

{-Ruta del archivo con las palabras a usar-}
dictionary :: FilePath 
dictionary = "/usr/share/dict/american-english" -- Holas

{- Filtra todas las palabras quedandose con aquellas que sean de tamanio 5 y que no sean un sustantivo propio
fiveLetterWords ["Ramon", "hola", "zip's", "extravagante", "gatos"] = ["gatos"]
 -}
fiveLetterWords :: [String] -> [String] -- Tiene problemas con las palabras: éclat
fiveLetterWords = filter isValid
                where
                    isValid "" = False
                    isValid word = fst (reduce word) && snd (reduce word) == 5
                        where
                            reduce = foldl go (True, 0)
                            go (False, n) _ = (False, n)
                            go (True, 0) c
                                | isUpper c && isAlpha c = (False, 1)
                                | otherwise = (True, 1)
                            go (True, n) c
                                | n <= 5 && isAlpha c = (True, n+1)
                                | otherwise = (False, n+1)



{-Carga un arbol AA con las palabras del diccionario que cumplan con el criterio de ser 
una palabra de cinco letras-}
-- Se esta guardando con clave y valor iguales, como deberia ser?
loadDictionary :: FilePath -> IO (AA String String) 
loadDictionary x = do l <- readFile x
                      pure $ foldl step empty $ fiveLetterWords $ lines l
                        where
                            step tree word = insert word word tree

{-Muestra un mensaje arbitrario y luego pide pide una decision afirmativa o negativa-}
yesOrNo :: String -> IO Bool 
yesOrNo mesagge = do putStr $ mesagge ++ " (y/n)"
                     yesOrNoLoop
yesOrNoLoop :: IO Bool
yesOrNoLoop = do c <- getChar 
                 case c of
                         'y' -> do putStrLn "c" 
                                   pure True
                         'n' -> do putStrLn "c"
                                   pure False
                         _ -> yesOrNoLoop
