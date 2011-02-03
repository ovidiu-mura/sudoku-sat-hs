-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- CNF SAT encoding of Sudoku problems

import Data.Char (isDigit, digitToInt)
import Data.List (zipWith)
import Data.Maybe (catMaybes)

-- cell size

c :: Int
c = 3

-- range
r = [0 .. c^2 - 1]

-- atoms
x i j k = 1 + c^4 * i + c^2 * j + k

-- existence

exist i j = [x i j k | k <- r]

exists = [exist i j | i <- r, j <- r]

-- uniqueness

unique :: Int -> Int -> Int -> [[Int]]
unique i j k = [[- (x i j k), - (x i j k')] | k' <- r, k' /= k]

uniques :: [[Int]]
uniques = concat [unique i j k | i <- r, j <- r, k <- r]

-- rows

row i j k = [[- (x i j k), - (x i j' k)] | j' <- r, j' /= j]

rows = concat [row i j k | i <- r, j <- r, k <- r]

-- cols

col i j k = [[- (x i j k), - (x i' j k)] | i' <- r, i' /= i]

cols = concat [col i j k | i <- r, j <- r, k <- r]

-- boxes (what a pain)

boxindices i j =
  let i0 = i - (i `mod` c)
      j0 = j - (j `mod` c) in
  [ (i', j') | i' <- [i0 .. i0 + 2], 
               j' <- [j0 .. j0 + 2],
               i' /= i || j' /= j ]

box i j k = [[- (x i j k), - (x i' j' k)] | (i', j') <- boxindices i j]

boxes = concat [box i j k | i <- r, j <- r, k <- r]

-- all problem axioms

problemAxioms = exists ++ uniques ++ rows ++ cols ++ boxes

-- instance axioms

fixValue i j k = [x i j k]

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

-- encoder

encodeFormula cnf =
  unlines $ map encodeClause cnf
  where
    encodeClause clause = unwords $ map show $ clause ++ [0]

-- driver

main = do
  inst <- getContents
  let instanceAxioms = readInstance inst
  let axioms = instanceAxioms ++ problemAxioms
  putStrLn $ "p cnf " ++ show (c^6) ++ " " ++ show (length axioms)
  putStr $ encodeFormula axioms
