module Util where
import Data.Char (isUpper, isAlpha)
import AAtrees ( empty, insert, AA )

turns :: Int
turns = 6

dictionary :: FilePath 
dictionary = "/usr/share/dict/american-english" -- Holas

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


-- Se esta guardando con clave y valor iguales, como deberia ser?
loadDictionary :: FilePath -> IO (AA String String) 
loadDictionary x = do l <- readFile x
                      pure $ foldl step empty $ fiveLetterWords $ lines l
                        where
                            step tree word = insert word word tree


