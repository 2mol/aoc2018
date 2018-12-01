module Day01 where

-- SoundtrackL Hitori Tori

import qualified Data.Set as S

main = do
    input <- readFile "day01input"

    -- part 1

    let drifts = map signedRead (lines input)
    putStrLn $ "part 1: \n" <> show (sum drifts)

    -- part 2

    let drifts' = drifts <> drifts'
        freqs = scanl (+) 0 drifts'
        -- dupIndices = filter (check freqs) [0..]
        -- firstDup = freqs !! (head dupIndices)
        Just dup = duplicateFinder S.empty freqs

    putStrLn $ "part 2: \n" <> show dup

    -- where
        -- check xs n = (xs !! n) `elem` (take n xs)

signedRead :: String -> Int
signedRead s =
    if head s == '+'
       then read $ tail s
       else read s

duplicateFinder _ [] = Nothing
duplicateFinder seen (x:xs) =
    if S.member x seen
       then Just x
       else duplicateFinder (S.insert x seen) xs

