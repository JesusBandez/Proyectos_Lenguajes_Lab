module Play where
import AAtrees
import Match

data GameState = GS { played :: Int
                 , won :: Int
                 , target:: Target
                 , dict :: AA String String}