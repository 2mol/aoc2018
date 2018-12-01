module Day01 where

main = do
    input <- readFile "day01input"
    let drifts = map signedRead (lines input)
    putStrLn $ "part 1: \n" <> show (sum drifts)

    let drifts' = drifts <> drifts'
        freqs = scanl (+) 0 drifts'

    putStrLn $ "part 2: "

signedRead :: String -> Int
signedRead s =
    if head s == '+'
       then read $ tail s
       else read s
