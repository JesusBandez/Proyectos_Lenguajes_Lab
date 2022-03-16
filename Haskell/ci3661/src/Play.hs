module Play where
import AAtrees
import Match

data GameState = GS { played :: Int
                 , won :: Int
                 , lost :: Int
                 , streak :: Int
                 , target:: Target
                 , dict :: AA String String}

                 
instance Show GameState where
    show (GS p w l s _ _) = "Played: " ++ show p ++ " Won: " ++ show w ++ " Lost: " ++ show l ++ " Streak: " ++ show s

initialState :: IO GameState
initialState = pure (GS 0 0 0 0 Empty empty)

