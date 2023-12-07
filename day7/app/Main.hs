{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Ord
import Flow

-- >>> parseLine "32T3K 765"
-- ("32T3K",765)
parseLine line =
  let (hand, bid) = span (/= ' ') line
      bid' = read $ drop 1 bid :: Int
   in (hand, bid')

-- >>> parse "32T3K 765\nT55J5 684"
-- [("32T3K",765),("T55J5",684)]
parse = lines .> map parseLine

shapeRank :: [Int] -> Int
shapeRank [1, 1, 1, 1, 1] = 1
shapeRank [2, 1, 1, 1] = 2
shapeRank [2, 2, 1] = 3
shapeRank [3, 1, 1] = 4
shapeRank [3, 2] = 5
shapeRank [4, 1] = 6
shapeRank [5] = 7

-- >>> card 'T'
card = flip elemIndex ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'] .> fromJust

card2 = flip elemIndex ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'] .> fromJust

-- >>> shape "32T3K"
-- 2
shape :: (Ord a) => [a] -> [Int]
shape = sort .> group .> map length .> sortBy (comparing Down) :: _ -> [Int]

shape2 hand =
  let hand' = sort hand
      (js, rest) = span (== card2 'J') hand'
      js' = length js
      sh : st = shape rest ++ [0]
      st' = filter (/= 0) st
   in (sh + js') : st'

-- >>> mapHand "32T3K" 1
-- >>> mapHand "T55J5" 1
-- (2,[3,2,10,3,13],1)
-- (4,[10,5,5,11,5],1)
mapHand hand bid =
  let rank = shapeRank $ shape hand
      cards = map card hand
   in (rank, cards, bid)

-- >>> mapHand2 "32T3K" 1
-- >>> mapHand2 "T55J5" 1
-- (2,[2,1,9,2,11],1)
-- (6,[9,4,4,0,4],1)
mapHand2 hand bid =
  let cards = map card2 hand
      rank = shapeRank $ shape2 cards
   in (rank, cards, bid)

-- >>> solve1 [("32T3K", 765), ("T55J5", 684), ("KK677", 28), ("KTJJT", 220), ("QQQJA", 483)]
-- 6440
solve1 =
  let bid (_, _, b) = b
   in map (uncurry mapHand) .> sort .> map bid .> zipWith (*) [1 ..] .> sum :: [(String, Int)] -> _

-- >>> solve2 [("32T3K", 765), ("T55J5", 684), ("KK677", 28), ("KTJJT", 220), ("QQQJA", 483)]
-- 5905
solve2 =
  let bid (_, _, b) = b
   in map (uncurry mapHand2) .> sort .> map bid .> zipWith (*) [1 ..] .> sum :: [(String, Int)] -> _

main :: IO ()
main = do
  input <- getContents
  let input' = parse input
  print $ solve1 input'
  print $ solve2 input'
