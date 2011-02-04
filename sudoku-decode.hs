-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | Decode CNF SAT output for Sudoku encoding.
-- The SAT encoding is presumed to correspond 
-- to that produced by `sudoku-encode.hs`.

import Prelude hiding (lookup)
import Data.Char (intToDigit)
import Data.Map hiding (map, filter)
import Data.Maybe (fromJust)

-- | We do Sudoku on 9x9 grids (cell size 3) for now. The type
-- constraint is contagious, and makes all the numbers in the 
-- program 'Int'.
c :: Int
c = 3

-- | The "range" of row and column indices, etc.
r = [0 .. c^2 - 1]

-- | Decode an atom number into its corresponding row,
-- column, number value, and truth value. The use of
-- tuples here is lazy and error-prone, but simple and
-- convenient.
d x =
  let v = x > 0 in
  let x' = if v then x - 1 else (- x) - 1 in
  let k = x' `mod` c^2
      j = (x' `div` c^2) `mod` c^2
      i = x' `div` c^4 in
  (v, ((i, j), k))

-- | Display the board resulting from decoding
-- all the atoms and finding the positive ones.
-- A 'Map' is used to track the value of each
-- row and column coordinate.
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

-- | This simple driver reads an assignment from `stdin`,
-- and writes the decoded solution to `stdout`.
main = do
  soln <- getContents
  let xs = concatMap (map (d . read) . words) $ lines soln
  let xps = map snd $ filter fst $ xs
  let m = fromList xps
  putStr $ showBoard m
