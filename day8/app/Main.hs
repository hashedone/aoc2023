{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Data.Function
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Flow

-- >>> parseLine "NNN = (TNT, XDJ)"
-- ("NNN",("TNT","XDJ"))
parseLine line =
  let line' = filter (/= ' ') line
      (from, to) = span (/= '=') line'
      to' = drop 2 to
      (left, rest) = span (/= ',') to'
      rest' = drop 1 rest
      right = takeWhile (/= ')') rest'
   in (from, (left, right))

next (l, _) 'L' = l
next (_, r) 'R' = r

road start path graph =
  let dirs pos = fromJust (pos `HM.lookup` graph)
      nextItem pos = next (dirs pos)
   in scanl nextItem start (cycle path)

solve1 path = road "AAA" path .> takeWhile (/= "ZZZ") .> length

checkpoints path len =
  let path' = zip path $ cycle [1 .. len]
      groups = groupBy (\_ (b, _) -> last b /= 'Z') path'
      dists = map length groups
      dests = drop 1 groups & map head
      buildCheckpoints (visited, (steps, next) : rest) =
        if next `elem` visited
          then Nothing
          else Just (steps, (next : visited, rest))
   in unfoldr buildCheckpoints ([], zip dists dests)

solve2 path graph =
  let inits = filter (last .> (== 'A')) (HM.keys graph) :: [String]
      buildCycle init = checkpoints (road init (cycle path) graph) (length path)
      cycles = map buildCycle inits |> concat
   in foldr lcm 1 cycles

main :: IO ()
main = do
  path <- getLine
  input <- getContents
  let input' = lines input & filter (not . null) & map parseLine
  let graph = HM.fromList input'
  --   let p1 = solve1 path graph
  --  print p1

  let p2 = solve2 path graph
  print p2
