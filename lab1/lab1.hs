import Test.QuickCheck

{-

Lab assignment: 1
By:             Tobias Tikka & Marcus Olsson
Course:         TDA452

-}

power :: Int -> Int -> Int
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

{-

PART 1:

It takes k+1 steps to calculate the function. (O(k))

-}

-- PART 2:

power1 :: Int -> Int -> Int
power1 n k 
            | k < 0         = error "power: negative argument"
            | otherwise     = product (replicate k n) 
                      -- (take k (repeat n)) would work as well

-- PART 3:

power2 :: Int -> Int -> Int
power2 n k 
            | k == 0        = 1
            | k < 0         = error "power: negative argument"
            | odd k         = n * power2 n (k-1)
            | even k        = power2 (n * n) (k `div` 2)

--PART 4:
{- 

A:

n = 5, k = 3    == 125
n = 3, k = 5    == 243
n = 0, k = 5    == 0
n = 5, k = 0    == 1
n = 5, k = -1   == error    
(According to information in the Google Group we shouldn't test negatives)
n = -2, k = 3   == -8
n = -2, k = 2   == 4

-}

-- B:

prop_powers :: Int -> Int -> Bool
prop_powers n k = (power n k) == (power1 n k) 
                && (power1 n k) == (power2 n k)

-- C:

-- We write a function that puts an element in a list if
-- a test case fails. Returns True if the array is 0, .e.g. 
-- all tests OK and False otherwise 

test_powers :: Bool
test_powers = length ["Test case failed"
                | n <- [5, 3, 0, 5, (-2), (-2)], 
                  k <- [3, 5, 5, 0, 3, 2],
                  not prop_powers n k] == 0


-- We reuse the prop_powers function and always makes sure that k is positive.

prop_powers' :: Int -> Int -> Bool
prop_powers' n k    
                    | k < 0 = prop_powers n (-k)
                    | otherwise = prop_powers n k
