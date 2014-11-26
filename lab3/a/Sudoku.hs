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
  and [length row == 9 && and [isNothing cell || 
  (fromJust cell > 0 && fromJust cell < 10) | cell <- row] | row <- rows s]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and [and [isJust cell | cell <- row] | row <- rows s]

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn $ unlines [[if isNothing cell then '.' else 
                chr $ 48 + fromJust cell | cell <- row] | row <- rows s]

-- readSudoku file reads from the file, and either delivers it, 
-- or returns a blank Sudoku if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku f =
  do
    content <- readFile f
    let s = Sudoku [[if cell == '.' then Nothing else Just (ord cell - 48) | cell <- row] 
                   | row <- lines content]

    if isSudoku s then return s else do
      putStrLn "Program error: Not a valid Sudoku!"
      return allBlankSudoku

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = do
  n <- choose (1, 9)
  frequency [(9, return Nothing), (1, return $ Just n)]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [sequence [cell | j <- [1..9]] | i <- [1..9]]
    return (Sudoku rows)

-- Checks that a given Sudoku is a proper Sudoku 
-- given the properies of the isSudoku function
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

type Block = [Maybe Int]

-- Checks that a given block is a okay block, 
-- i.e. has nine cells and no duplicates
isOkayBlock :: Block -> Bool
isOkayBlock b = length b == 9 && length onlyDigits == length (nub onlyDigits)
  where onlyDigits = filter isJust b

-- Function returning all "blocks" of a given Sudoku, 
-- i.e. all rows, columns and 9x9 blocks
blocks :: Sudoku -> [Block]
blocks s = rs ++ transpose rs ++ threeByThreeBlocks
  where
    rs                  = rows s
    inRightOrder        = concat [take 3 $ drop n row | n <- [0, 3, 6], 
                          row <- transpose rs]
    threeByThreeBlocks  = [take 9 $ drop n inRightOrder | n <- [0, 9..72]]

-- A property checking if the returned list of blocks of a given Sudoku is okay, 
-- i.e. there are 27 of them and the length of all of them is 9
prop_blocks :: Sudoku -> Bool
prop_blocks s = length bs == 27 && and [length block == 9 | block <- bs]
  where bs = blocks s

-- A function that checks if all blocks in a Sudoku are okay given the rules
-- in the isOkayBlock function
isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock block | block <- blocks s]

type Pos = (Int, Int)