-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | This file produces a DIMACS-format CNF SAT encoding of
-- a 9x9 Sudoku instance. It is expected to be used with an
-- off-the-shelf SAT solver such as minisat2 or picosat, and
-- with `sudoku-decode.hs`, to solve Sudoku instances.

import Data.Char (isDigit, digitToInt)
import Data.List (zipWith)
import Data.Maybe (catMaybes)

-- | We do Sudoku on 9x9 grids (cell size 3) for now. The type
-- constraint is contagious, and makes all the numbers in the 
-- program 'Int'.
c :: Int
c = 3

-- | The "range" of row and column indices, etc.
r = [0 .. c^2 - 1]

-- | Encode a row and column index and a value into a small
-- integer representing a propositional atom, in an
-- easily-reversible way.
x i j k = 1 + c^4 * i + c^2 * j + k

-- | Axiom specifying the existence of some value
-- at a particular row and column.
exist i j = [x i j k | k <- r]

-- | Every coordinate position should have a value at it.
exists = [exist i j | i <- r, j <- r]

-- | Axiom specifying the uniqueness of value `k` at
-- coordinate `(i, j)`. This is essentially an implication,
-- read from left to right.
unique :: Int -> Int -> Int -> [[Int]]
unique i j k = [[- (x i j k), - (x i j k')] | k' <- r, k' /= k]

-- | There should never be more than one value assigned to
-- any coordinate.
uniques :: [[Int]]
uniques = concat [unique i j k | i <- r, j <- r, k <- r]

-- | Axiom specifying the uniqueness of value k within a row.
row i j k = [[- (x i j k), - (x i j' k)] | j' <- r, j' /= j]

-- | No row should have a duplicated value.
rows = concat [row i j k | i <- r, j <- r, k <- r]

-- | Axiom specifying the uniqueness of value k within a column.
col i j k = [[- (x i j k), - (x i' j k)] | i' <- r, i' /= i]

-- | No column should have a duplicated value.
cols = concat [col i j k | i <- r, j <- r, k <- r]

-- The little 3x3 boxes are a pain, since one has to figure
-- out what the neighbors are. This function returns an
-- irreflexive list of the box-neighbors of a given
-- coordinate.
boxIndices i j =
  let i0 = i - (i `mod` c)
      j0 = j - (j `mod` c) in
  [ (i', j') | i' <- [i0 .. i0 + 2], 
               j' <- [j0 .. j0 + 2],
               i' /= i || j' /= j ]

-- | Axiom specifying the uniqueness of value k within a box.
box i j k = [[- (x i j k), - (x i' j' k)] | (i', j') <- boxIndices i j]

-- | No box should have a duplicated value.
boxes = concat [box i j k | i <- r, j <- r, k <- r]

-- | This is just the list of all problem axioms from above.
problemAxioms = exists ++ uniques ++ rows ++ cols ++ boxes

-- | The instance axioms are all singleton clauses that
-- assert that a particular coordinate has a particular
-- value when the program starts.
fixValue i j k = [x i j k]

-- | Read in a Sudoku instance, sanity checking it a tiny
-- bit, and produce the propositional instance axioms for
-- that instance.
readInstance desc =
  let rows = lines desc in
  case length rows of
    9 -> concat $ zipWith makeRow r rows
    _ -> error "wrong number of rows"
  where
    makeRow i row =
      case length row of
        9 -> catMaybes $ zipWith makeEntry r row
        _ -> error "wrong number of cols"
      where
        makeEntry j cell 
          | isDigit cell = Just $ fixValue i j (digitToInt cell - 1)
          | otherwise = Nothing

-- | Given a CNF in list-of-list-of-literals form, produce
-- a 'String' encoding of the formula as per DIMACS format.
encodeFormula cnf =
  unlines $ map encodeClause cnf
  where
    encodeClause clause = unwords $ map show $ clause ++ [0]

-- | This simple driver reads in the instance, builds the
-- corresponding CNF formula, and outputs it in DIMACS
-- format.
main = do
  inst <- getContents
  let instanceAxioms = readInstance inst
  let axioms = instanceAxioms ++ problemAxioms
  putStrLn $ "p cnf " ++ show (c^6) ++ " " ++ show (length axioms)
  putStr $ encodeFormula axioms
