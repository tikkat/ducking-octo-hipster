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

blanks :: Sudoku -> [Pos]
blanks s = [(row, column) | row <- [0..8], column <- [0..8], 
           isNothing $ (rows s !! row) !! column]

prop_blanks :: Sudoku -> Bool
prop_blanks s = and [isNothing $ (rows s !! fst pos) !! snd pos | pos <- blanks s]

(!!=) :: [a] -> (Int, a) -> [a]
[]  !!= _           = error "Program error: Empty list."
xs  !!= (n, value)  | n + 1 > length xs = error "Program error: Too large n."
                    | n < 0 = error "Program error: Negative n."
                    | otherwise = take n xs ++ value:drop (n + 1) xs

{-
  Check that the value at the position are what is expected and
  that the two lists with the elements at that position removed are the same.
  Only test for integers, it is enough and correct.
-}
prop_changeCell :: NonEmptyList Int -> (NonNegative Int, Int) -> Property
prop_changeCell (NonEmpty xs) (NonNegative n, value) = 
  n < length xs ==> xs' !! n == value && withoutN xs n == withoutN xs' n
  where
    xs' = xs !!= (n, value)

    withoutN :: [a] -> Int -> [a]
    withoutN xs n = take n xs ++ drop (n + 1) xs

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (r, c) val = Sudoku $ take r rs ++ 
  ((rs !! r) !!= (c, val)):drop (r + 1) rs
  where rs = rows s

prop_update :: Sudoku -> Pos -> Maybe Int -> Property
prop_update s (r, c) val = r > -1 && r < 9 && c > -1 && c < 9 ==> 
  (rows s' !! r) !! c == val
  where s' = update s (r, c) val

-- Get current row, coulumn and block in the same list, 
-- add number to candidates if not in the list
candidates :: Sudoku -> Pos -> [Int]
candidates s (r, c) = [digit | digit <- [1..9], 
                      length bs' == length (delete (Just digit) bs')]
  where
    bs        = blocks s
    numBlock  = floor (toRational c / 3) + floor (toRational r / 3) * 3
    bs'       = bs !! r ++ bs !! (9 + c) ++ bs !! (18 + numBlock)


prop_candidates :: Sudoku -> Pos -> Property
prop_candidates s (r, c) = 
  isSudoku s && isOkay s && r > -1 && r < 9 && c > -1 && c < 9 ==> 
  and [isSudoku (newS cand) && isOkay (newS cand) | cand <- candidates s (r, c)]
  where newS cand = update s (r, c) (Just cand)

solve :: Sudoku -> Maybe Sudoku
solve s | not $ isSudoku s && isOkay s  = Nothing
        | otherwise                     = solve' s
  where
    solve' :: Sudoku -> Maybe Sudoku
    solve' s  | null allBlanks  = Just s
              | otherwise       = solve'' s (candidates s blank) blank
      where
        allBlanks = blanks s
        blank     = head allBlanks

        solve'' :: Sudoku -> [Int] -> Pos -> Maybe Sudoku
        solve'' _ [] _          = Nothing
        solve'' s (x:xs) blank  | isJust possibleSolution = possibleSolution
                                | otherwise               = solve'' s xs blank
          where possibleSolution = solve' $ update s blank (Just x)


readAndSolve :: FilePath -> IO ()
readAndSolve f =
  do
    s <- readSudoku f
    let solution = solve s

    maybe (putStrLn "Found no solution.") printSudoku solution

-- Make all cells unique with zip, count intersect and 
-- compare with number of solved cells in the original
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = isSolved s1 && isOkay s1 && 
                     length (rs1 `intersect` rs2) == s2NumSolved
  where
    rs1 = concat (rows s1) `zip` [1..81]
    rs2 = concat (rows s2) `zip` [1..81]
    s2NumSolved = length $ filter isJust $ concat $ rows s2

prop_solveSound :: Sudoku -> Property
prop_solveSound s = isJust solution ==> fromJust solution `isSolutionOf` s
  where solution = solve s

fewerChecks prop = quickCheckWith stdArgs {maxSuccess = 30} prop