{-# LANGUAGE MultiWayIf #-}

module Day02 where

import Data.Function ((&))
import qualified Data.Map as M
import qualified Data.Set as S

main = do
    input <- lines <$> readFile "day02input"

    let
        (c2, c3) =
            map sameCounts input
                & foldl aggregate (0, 0)

    print (c2 * c3)

count :: Ord a => M.Map a Int -> a -> M.Map a Int
count dict el = M.insertWith (+) el 1 dict

sameCounts :: String -> S.Set Int
sameCounts s =
    S.fromList . M.elems $ foldl count M.empty s

aggregate :: (Int, Int) -> S.Set Int -> (Int, Int)
aggregate (twos, threes) counts =
    ( twos   + (if 2 `elem` counts then 1 else 0)
    , threes + (if 3 `elem` counts then 1 else 0)
    )

--

diffAtMostOne c "" "" = c > 1
diffAtMostOne c (s1:s1') (s2:s2') =
    if | c > 1 -> False
       | s1 == s2 -> diffAtMostOne c s1' s2'
       | s1 /= s2 -> diffAtMostOne (c+1) s1' s2'
