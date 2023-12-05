module Main where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List.Split
import Data.Maybe
import Flow

-- >>> evalLine "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- 4
evalLine =
  let trim = drop 1 .> dropWhile isSpace
      split = span (/= '|')
      splitPart = splitOn " " .> filter (not . null) .> map read :: String -> [Int]
      matches = dropWhile (/= ':') .> trim .> split .> second trim .> bimap splitPart splitPart .> uncurry (liftM2 (==)) .> filter id .> length
   in dropWhile (/= ':') .> trim .> split .> second trim .> bimap splitPart splitPart .> uncurry (liftM2 (==)) .> filter id .> length

-- >>> calc1 [4, 2, 2, 1, 0, 0]
-- 15
calc1 = map (\x -> if x == 0 then 0 else 2 ^ (x - 1)) .> sum

solve1 = lines .> map evalLine .> calc1 .> show

-- >>> folder 4 (0, [])
-- >>> folder 2 (1, [1, 1, 1, 1])
-- >>> folder 2 (3, [3, 3, 1])
-- >>> folder 1 (7, [7, 5])
-- >>> folder 0 (15, [13])
-- >>> folder 0 (29, [])
-- (1,[1,1,1,1])
-- (3,[3,3,1])
-- (7,[7,5])
-- (15,[13])
-- (29,[])
-- (30,[])
folder (acc, copies) item =
  let h : copies' = copies ++ repeat 0
      h' = h + 1
      new_copies = replicate item h' ++ repeat 0
      copies'' = zipWith (+) copies' new_copies |> takeWhile (/= 0)
   in (acc + h', copies'')

-- >>> calc2 [4, 2, 2, 1, 0, 0]
-- 9
calc2 = foldl folder (0, []) .> fst

solve2 = lines .> filter (not . null) .> map evalLine .> calc2 .> show

main :: IO ()
main = interact (flip ($) .> flip map [solve1, solve2] .> show)
