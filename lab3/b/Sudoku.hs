module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s  = length (rows s) == 9 &&
             and [length row == 9 && and [isNothing column ||
             (fromJust column > 0 && fromJust column < 10) | column <- row] | row <- rows s]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and[ and [isJust cell | cell <- row] | row <- rows s]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn $ unlines [[if isNothing cell then '.' else
  chr $ 48 + fromJust cell | cell <- row] | row <- rows s]

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
                s <- readFile f
                let arr = Sudoku[[ if cell == '.' then Nothing else Just (ord cell - 48) | cell <- row ] | row <- lines s]
                if not (isSudoku arr) then error "Du e la go" else return arr

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = do
        n <- choose (1, 9)
        frequency [(9, return Nothing), (1, return $ Just n)]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock b = length b == 9 && length filtered == length (nub filtered)
  where filtered = filter isJust b

blocks :: Sudoku -> [Block]
blocks s = rws ++ transpose rws ++ threeByThree
  where rws           = rows s
        threeByThree  = [take 9 $ drop n allElem | n <- [0, 9..72]]
        allElem       = concat [take 3 $ drop n row | n <- [0,3,6], row <- transpose rws]

prop_blocks :: Sudoku -> Bool
prop_blocks s = length bs == 27 && and[length block == 9 | block <- bs ]
  where  bs = blocks s
isOkay :: Sudoku -> Bool
isOkay s = and[isOkayBlock block | block <- blocks s]

type Pos = (Int,Int)

-------------------------------------------------------------------------

-- PART B

-------------------------------------------------------------------------

-- Prints all the cells in the Sudoku that doesn't have a number on it
blanks :: Sudoku -> [Pos]
blanks s = [(row, cell) | row <- [0..8], cell <- [0..8], isNothing $ (rows s !! row) !! cell ]

prop_blanks :: Sudoku -> Bool
prop_blanks s = all isNothing (blanks s)

-- (!!=) :: [a] -> (Int,a) -> [a]

-------------------------------------------------------------------------
example :: Sudoku
example =
  Sudoku
  [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
  , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
  , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
  , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
  , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
  , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
  , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
  , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
  , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
  ]
