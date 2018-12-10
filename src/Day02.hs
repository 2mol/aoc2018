{-# LANGUAGE MultiWayIf #-}

module Day02 where

import Data.Function ((&))
import qualified Data.Map as M
import qualified Data.Set as S

main = do
    input <- lines <$> readFile "day02input"

    let
        comb = [ (s1, s2, diffAtMostOne 0 s1 s2) | s1 <- input, s2 <- input, s1 /= s2]

        matches = filter (\(_, _, m) -> m) comb

        (m1, m2, _) = matches !! 0

        trimmed = trim m1 m2

    print $ matches !! 0
    print trimmed
    print "yay"

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

diffAtMostOne c "" "" = c <= 1
-- diffAtMostOne c "" _ = False
-- diffAtMostOne c _ "" = False
diffAtMostOne c (s1:s1') (s2:s2') =
    if | c > 1 -> False
       | s1 == s2 -> diffAtMostOne c s1' s2'
       | s1 /= s2 -> diffAtMostOne (c+1) s1' s2'

trim s1 "" = s1
trim "" s2 = s2
trim (s1:s1') (s2:s2') =
    if s1 == s2
        then s1 : (trim s1' s2')
        else trim s1' s2'
