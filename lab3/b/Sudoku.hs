module Sudoku where

import Data.Maybe
import Data.Char
import Data.List

import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
	deriving (Show, Eq)

example :: Sudoku
example = Sudoku [
	[Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing],
	[Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing],
	[Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing],
	[Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8],
	[Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9],
	[Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing],
	[Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing],
	[Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing],
	[Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]]

example2 :: Sudoku
example2 = Sudoku [
	[Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9],
	[Just 10, Just 11, Just 12, Just 13, Just 14, Just 15, Just 16, Just 17, Just 18],
	[Just 19, Just 20, Just 21, Just 22, Just 23, Just 24, Just 25, Just 26, Just 27],
	[Just 28, Just 29, Just 30, Just 31, Just 32, Just 33, Just 34, Just 35, Just 36],
	[Just 37, Just 38, Just 39, Just 40, Just 41, Just 42, Just 43, Just 44, Just 45],
	[Just 46, Just 47, Just 48, Just 49, Just 50, Just 51, Just 52, Just 53, Just 54],
	[Just 55, Just 56, Just 57, Just 58, Just 59, Just 60, Just 61, Just 62, Just 63],
	[Just 64, Just 65, Just 66, Just 67, Just 68, Just 69, Just 70, Just 71, Just 72],
	[Just 73, Just 74, Just 75, Just 76, Just 77, Just 78, Just 79, Just 80, Just 81]]

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = length (rows s) == 9 && and [length row == 9 && and [isNothing column || 
			 (fromJust column > 0 && fromJust column < 10) | column <- row] | row <- rows s]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and [and [isNothing column | column <- row] | row <- rows s]

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
		let s = Sudoku [[if a == '.' then Nothing else 
				Just (ord a - 48) | a <- k] | k <- lines content]

		if isSudoku s
			then
				return s
			else do
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
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock b = length b == 9 && length a == length (nub a)
	where a = [k | k <- b, isJust k]

blocks :: Sudoku -> [Block]
blocks s = undefined; --(rows s):(rows (transpose s))

{-threeByThreeBlocks :: [[Maybe Int]] -> [Block]
threeByThreeBlocks rs = [take 9 $ drop n ungroupped | n <- [0,9..72]]
  where ungroupped = concat [take 3 $ drop n (rs !! i) | n <- [0,3,6], i <- [0..8]]-}