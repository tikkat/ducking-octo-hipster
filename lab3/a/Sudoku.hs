module Sudoku where

import Data.Maybe
import Data.Char
import Data.List

import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving (Show, Eq)

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku puzzle,
-- i.e. 9 x 9 digits in the range 1 - 9 (or nothing)
isSudoku :: Sudoku -> Bool
isSudoku s = length (rows s) == 9 && 
  and [length row == 9 && and [isNothing column || 
  (fromJust column > 0 && fromJust column < 10) | column <- row] | row <- rows s]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and [and [isJust column | column <- row] | row <- rows s]

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn $ unlines [[if isNothing column then '.' else 
        chr (48 + fromJust column) | column <- row] | row <- rows s]

-- readSudoku file reads from the file, and either delivers it, 
-- or stops if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku f =
  do
    content <- readFile f
    let s = Sudoku [[if a == '.' then Nothing else Just (ord a - 48) | a <- k] 
                   | k <- lines content]

    if isSudoku s then return s else do
      putStrLn "Program error: Not a valid Sudoku!"
      return allBlankSudoku

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell =
  do
    n <- choose (1, 9)
    frequency [(9, return Nothing), (1, return (Just n))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do
      rows <- sequence [sequence [cell | j <- [1..9]] | i <- [1..9]]
      return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

type Block = [Maybe Int]