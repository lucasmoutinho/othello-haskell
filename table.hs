import qualified Data.Map

data Cell = Empty | Black | White deriving(Eq, Show)
type Position = (Int,Int)

createTable :: Cell -> [(Position,Cell)]
createTable z = [((x,y),z) | x <- [0..7] , y <- [0..7]]