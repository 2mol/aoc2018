module Day03 where

import Data.Function ((&))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (isSuffixOf)

main = do
  input <- lines <$> readFile "input"

  let
    claims = map parseClaim input

    counts =
      foldl markPoints M.empty claims

    overlapCount =
      M.filter (> 1) counts
        & M.size

    nonOverlapping =
      filter (isDisjoint counts) claims

    result = nonOverlapping !! 0

    (x1, y1, x2, y2) = result

    resultStr =
      show x1
      ++ ","
      ++ show y1
      ++ ": "
      ++ show (x2 - x1 + 1)
      ++ "x"
      ++ show (y2 - y1 + 1)

    bla = filter (isSuffixOf resultStr) input

  print (input !! 0)
  print overlapCount
  print $ result
  print $ resultStr
  print bla


parseClaim s =
  let
    margins =
      s
        & takeWhile (/= ':')
        & dropWhile (/= '@')
        & drop 2

    dimensions =
      s
        & dropWhile (/= ':')
        & drop 2

    left   = read $ takeWhile (/= ',') margins
    top    = read $ drop 1 $ dropWhile (/= ',') margins
    width  = read $ takeWhile (/= 'x') dimensions
    height = read $ drop 1 $ dropWhile (/= 'x') dimensions

  in
    ( left
    , top
    , left + width - 1
    , top + height - 1
    )


markPoints dict (x1, y1, x2, y2) =
  M.unionWith (+) dict rect
  where
  rect =
    M.fromList
    [((x, y), 1) | x <- [x1..x2], y <- [y1..y2]]


isDisjoint dict (x1, y1, x2, y2) =
  S.fromList counts == S.singleton (Just 1)
  where
    counts :: [Maybe Int]
    counts = map (\k -> M.lookup k dict) points

    points :: [(Int, Int)]
    points = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
