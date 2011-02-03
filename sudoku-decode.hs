-- Copyright © 2011 Bart Massey
-- Decode CNF SAT output for Sudoku encoding.
-- Use with sudoku-encode.hs .

import Prelude hiding (lookup)
import Data.Char (intToDigit)
import Data.Map hiding (map, filter)
import Data.Maybe (fromJust)

-- cell size

c :: Int
c = 3

-- range

r = [0 .. c^2 - 1]

-- decode atom

d x =
  let v = x > 0 in
  let x' = if v then x - 1 else (- x) - 1 in
  let k = x' `mod` c^2
      j = (x' `div` c^2) `mod` c^2
      i = x' `div` c^4 in
  (v, ((i, j), k))

-- display

showBoard m =
  unlines $ map showRow r
  where
    showRow i =
      map showCell r
      where
        showCell j =
          case lookup (i, j) m of
            Just k -> intToDigit $ 1 + k
            Nothing -> '.'

-- driver

main = do
  soln <- getContents
  let xs = concatMap (map (d . read) . words) $ lines soln
  let xps = map snd $ filter fst $ xs
  let m = fromList xps
  putStr $ showBoard m
