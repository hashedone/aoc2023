module Main where

import Data.Char
import Data.List
import Data.List.Split
import Flow

-- Mapping would be convinently represented as (mappingRangeBegin, mappingRangeEnd, mappingOffset)
-- >>> buildMapping [50, 98, 2]
-- >>> buildMapping [52, 50, 48]
-- (98,99,-48)
-- (50,97,2)
buildMapping [dst, src, len] =
  (src, src + len, dst - src)

parse = splitOn ":" .> buildParts

buildParts (_ : seeds : maps) =
  let parseNumbers = splitWhen (not . isDigit) .> filter (not . null) .> map read :: String -> [Int]
      seeds' = parseNumbers seeds
      maps' = map (parseNumbers .> chunksOf 3 .> map buildMapping .> sort) maps
   in (seeds', maps')

-- >>> mapOnce1 79 (map buildMapping [[50, 98, 2], [52, 50, 48]])
-- >>> mapOnce1 14 (map buildMapping [[50, 98, 2], [52, 50, 48]])
-- >>> mapOnce1 55 (map buildMapping [[50, 98, 2], [52, 50, 48]])
-- >>> mapOnce1 13 (map buildMapping [[50, 98, 2], [52, 50, 48]])
-- 81
-- 14
-- 57
-- 13
mapOnce1 id ranges = [id + offset | (from, to, offset) <- ranges, id >= from, id < to] ++ [id] |> head

-- >>> mapAllOnce1 [79, 14, 55, 13] (map buildMapping [[50, 98, 2], [52, 50, 48]])
-- [81,14,57,13]
mapAllOnce1 ids ranges = map (`mapOnce1` ranges) ids

-- >>> buildRanges [79, 14, 55, 13]
-- [(79,93),(55,68)]
buildRanges = chunksOf 2 .> map (\[from, len] -> (from, from + len))

-- >>> mapRangePart (79, 93) (50, 97, 2)
-- >>> mapRangePart (55, 68) (50, 97, 2)
-- ([(81,95)],(0,0))
-- ([(57,70)],(0,0))
mapRangePart (from, to) (mfrom, mto, offset)
  | to <= mfrom = ([(from, to)], (0, 0))
  | from < mfrom && to < mto = ([(from, mfrom), (mfrom + offset, to + offset)], (0, 0))
  | from < mfrom = ([(from, mfrom), (mfrom + offset, mto + offset)], (mto, to))
  | to < mto = ([(from + offset, to + offset)], (0, 0))
  | from < mto = ([(from + offset, mto + offset)], (mto, to))
  | otherwise = ([], (from, to))

-- >>> foldOnce ([], (79, 93)) (50, 97, 2)
-- ([(81,95)],(0,0))
foldOnce (result, range) mapping =
  let (results', range') = mapRangePart range mapping
   in (result ++ results', range')

-- >>> foldRange (79, 93) (sort $ map buildMapping [[50, 98, 2], [52, 50, 48]])
-- [(81,95)]
foldRange :: (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
foldRange range mappings =
  let (mapped, unmapped) = foldl foldOnce ([], range) mappings
   in filter (/= (0, 0)) $ unmapped : mapped

--- >>> mapRangesOnce [(79, 93), (55, 68)] (sort $ map buildMapping [[50, 98, 2], [52, 50, 48]])
-- [(81,95),(57,70)]
mapRangesOnce ranges mapping = map (`foldRange` mapping) ranges |> concat

solve1 (seeds, maps) = foldl mapAllOnce1 seeds maps |> minimum

solve2 (seeds, maps) =
  let ranges = buildRanges seeds
   in foldl mapRangesOnce ranges maps |> map fst |> minimum

main :: IO ()
main = interact $ parse .> flip ($) .> flip map [solve1, solve2] .> show
