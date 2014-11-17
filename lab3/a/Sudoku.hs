import Data.Maybe
import Data.Char
import Test.QuickCheck

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
	deriving (Show, Eq)

{-example :: Sudoku
example = 	Sudoku [
				[Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing],
				[Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing],
				[Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8],
				[Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9],
				[Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing],
				[Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing],
				[Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
  			]-}

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = length (rows s) == 9 && and [length row == 9 && and [isNothing column || (fromJust column > 0 && fromJust column < 10) | column <- row] | row <- rows s]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and [and [isNothing column | column <- row] | row <- rows s]

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn $ unlines [[if isNothing column then '.' else chr (48 + fromJust column) | column <- row] | row <- rows s]

-- readSudoku file reads from the file, and either delivers it, 
-- or stops if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku f = undefined

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku