module Day06 where

import Data.Function ((&))
import qualified Data.List.Split as Split
import qualified Data.Map as M


main = do
    input <- readFile "day06input"

    let coords = readInput input

        (minX, maxX, minY, maxY) =
            foldl minMax (1000, 0, 1000, 0) coords

    print "ta-da"

readInput :: String -> [(Int, Int)]
readInput string =
    string
        & lines
        & map (Split.splitOn ", ")
        & map (\(x:y:_) -> (read x, read y))

minMax (minX, maxX, minY, maxY) (x, y) =
    (min x minX, max x maxX, min y minY, max y maxY)
