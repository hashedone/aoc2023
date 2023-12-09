module Main where

import Data.List
import Data.List.Split (splitOn)
import Flow

reductions input =
  let reduce arr = zipWith (-) (drop 1 arr) arr
   in unfoldr (\arr -> let arr' = reduce arr in Just (arr', arr')) input |> takeWhile (any (/= 0)) |> (input :)

solve1 input =
  let solveLine = reductions .> map last .> sum
   in map solveLine input |> sum

solve2 input =
  let solveLine = reductions .> map head .> foldr (-) 0
   in map solveLine input |> sum

main :: IO ()
main = do
  input <- getContents
  let input' = lines input |> map (splitOn " " .> map read) :: [[Int]]
  let p1 = solve1 input'
  print p1
  let p2 = solve2 input'
  print p2
