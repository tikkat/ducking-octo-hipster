import Test.QuickCheck

{-

Lab assignment: 1
By: 			Tobias Tikka & Marcus Olsson
Course: 		TDA452

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
power1 n k | k == 0		= 1
	 	   | k < 0 		= error "power: negative argument"
		   | otherwise 	= product (replicate k n) 
		   			  -- (take k (repeat n)) would work as well

-- PART 3:

power2 :: Int -> Int -> Int
power2 n k | k == 0		= 1
	       | k < 0 		= error "power: negative argument"
		   | odd k		= n * power2 n (k-1)
		   | even k 	= power2 (n * n) (div k 2)

--PART 4:
{- 

A:

n = 5, k = 3	== 125
n = 3, k = 5	== 243
n = 0, k = 5	== 0
n = 5, k = 0	== 1
n = 5, k = -1	== error	
(According to information in the Google Group we shouldn't test negatives)
n = -2, k = 3 	== -8
n = -2, k = 2 	== 4

-}

-- B:

prop_powers :: Int -> Int -> Bool
prop_powers n k = (power n k) == (power1 n k) 
				&& (power1 n k) == (power2 n k)

-- C:

test_powers :: Bool 
test_powers =	(prop_powers 5 3) &&
				(prop_powers 3 5) &&
				(prop_powers 0 5) &&
				(prop_powers 5 0) &&
				(prop_powers (-2) 3) &&
				(prop_powers (-2) 2)

prop_powers' :: Int -> Int -> Bool
prop_powers' n k | k < 0 = True
				 | otherwise = (power n k) == (power1 n k) 
				   && (power1 n k) == (power2 n k)

