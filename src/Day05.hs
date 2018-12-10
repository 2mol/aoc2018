{-# LANGUAGE MultiWayIf #-}

module Day05 where

import Data.Function ((&))
import Data.Char (isUpper, isLower, toUpper, toLower)

main = do
    input : _ <- lines <$> readFile "day05input"

    -- print $ length $ fullReduce input
    let
        charEq c1 c2 = toLower c1 /= toLower c2

        filterChar str c = filter (charEq c) str

        variants =
            map (filterChar input) ['a'..'z']

        smallest =
            map (length . fullReduce) variants
                & minimum

    print smallest

findNeighbours :: Int -> Int -> String -> [Int]
findNeighbours idx _ [ ] = []
findNeighbours idx _ [c] = []
findNeighbours idx lastMatchIdx (c1:c2:cs) =
    if c1 `isComplement` c2 && idx /= (lastMatchIdx + 1)
       then idx : findNeighbours (idx+1) idx (c2:cs)
       else       findNeighbours (idx+1) lastMatchIdx (c2:cs)
    where
        isComplement a b =
            (&&)
            (isUpper a == isLower b)
            (toLower a == toLower b)

fullReduce chars =
    let idxs = findNeighbours 0 (-2) chars
    in
    case idxs of
      [] -> chars
      _ -> fullReduce $ reduce (zip [0..] chars) idxs

reduce [] _ = []
reduce chars [] = map snd chars
reduce ((j, c):cs) idxs@(i:is) =
    if | j == i     ->     reduce cs idxs
       | j == i + 1 ->     reduce cs is
       | otherwise  -> c : reduce cs idxs
    --zip [0..] chars
    --    & map undefined

--bla (idx, c) (i:is)
