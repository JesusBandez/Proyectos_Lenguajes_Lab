module Play where
import AAtrees
import Match
import Data.Char (toLower, isAlpha)

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

{-Tiene problemas al momento de cargarse como ejecutable. El getChar espera por toda una string en vez de tomar solo un caracter 
Esto hace que se puedan borrar los caracteres ya introducidos luego de pulsar enter.
Por ahora, al pulsar enter y no ser aceptado, se borra lo que se lleva. No se esta respetando lo que pide el enunciado-}
recursiveReadFive :: String -> Int -> IO String
recursiveReadFive str i = do c <- getChar
                             case c of
                                '\0127' | i>0 -> recursiveReadFive (init str) (i-1)
                                '\n' | i==5 -> pure str
                                     | otherwise -> recursiveReadFive "" 0
                                c | isAlpha c -> recursiveReadFive (str ++ [toLower c]) (i+1)

                                _ -> recursiveReadFive str i

