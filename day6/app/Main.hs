{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Data.Char
import Data.Functor
import Data.List.Split
import Flow
import System.TimeIt

-- >>> parseLine1 "Time:      7  15   30"
-- >>> parseLine1 "Distance:  9  40  200"
-- [7,15,30]
-- [9,40,200]
parseLine1 = dropWhile (/= ':') .> drop 1 .> splitOn " " .> filter (not . null) .> map read :: _ -> [Int]

-- >>> parseLine2 "Time:      7  15   30"
-- >>> parseLine2 "Distance:  9  40  200"
-- 71530
-- 940200
parseLine2 = filter isDigit .> read :: _ -> Int

-- >>> race 7 0
-- >>> race 7 1
-- >>> race 7 2
-- >>> race 7 3
-- >>> race 7 4
-- >>> race 7 5
-- >>> race 7 6
-- >>> race 7 7
-- 0
-- 6
-- 10
-- 12
-- 12
-- 10
-- 6
-- 0
race total hold = (total - hold) * hold

-- >>> wins 7 9
-- >>> wins 71530 940200
-- 4
-- 71503
wins time dist = [1 | hold <- [1 .. time - 1], race time hold > dist] |> sum

solve1 times dists =
  let times' = parseLine1 times
      dists' = parseLine1 dists
      races = zipWith wins times' dists'
   in product races

solve2 times dists =
  let time = parseLine2 times
      dist = parseLine2 dists
   in wins time dist

main :: IO ()
main = do
  times <- getLine
  dists <- getLine
  timeIt $ putStr $ show $ solve1 times dists
  putStr "\n"
  timeIt $ putStr $ show $ solve2 times dists
  putStr "\n"
