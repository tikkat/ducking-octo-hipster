module BlackJack where

import Cards
import Wrapper

import Test.QuickCheck

{-
	#1  (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
	#2  1 + (Add (Card Jack Spades) Empty)
	#3  1 + 1 + Empty
	#4  1 + 1 + 0
	#5	== 2
-}

-- We define a "variable" empty that returns an empty hand
empty :: Hand
empty = Empty

{-
The valueRank function checks the value of a specific rank that you give it.
We have added a guard for the numeric values that only accepts values between 2
and 10. We do however believe that it would be better to have this check when 
you create a Card.
-}
valueRank :: Rank -> Integer
valueRank (Numeric n) | 1 < n && n <= 10	= n
valueRank Ace           					= 11
valueRank _             					= 10

-- We only care about the rank when we evaluate the value of a Card, we therefore
-- call for the valueRank function
valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

-- Recursively count the number of aces in the Hand, stop when the hand is Empty
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r _) h) | r == Ace      = 1 + numberOfAces h
                                | otherwise     = numberOfAces h

{-
Count the value of the Hand, take in consideration that the Ace is evaluated to 11
if n =< 21 and 1 if n > 21, where n is the sum of all ranks
-}
value :: Hand -> Integer
value h | valueHelper h > 21 = valueHelper h - (numberOfAces h * 10)
		| otherwise = valueHelper h
	where
		-- Helper function to sum all the ranks of the cards
		valueHelper :: Hand -> Integer
		valueHelper Empty = 0
		valueHelper (Add c h) = valueCard c + valueHelper h

gameOver :: Hand -> Bool
gameOver h  | value h > 21 = True
            | otherwise = False

winner :: Hand -> Hand -> Player
winner g b  | gameOver g 			= Bank
            | gameOver b 			= Guest
            | value b >= value g 	= Bank
            | otherwise 			= Guest

-- Function that takes two hands and puts the first hand on top of the other one
infixr 2 <+
(<+) :: Hand -> Hand -> Hand
Empty   <+ h2 = h2
Add c h <+ h2 = Add c (h <+ h2)


{-------------	PROP TESTS	-------------}


-- Test to see if the function <+ is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Test to see if the size of the two hands added together is the same as the 
-- size of the combined hand
prop_onTopOf_size :: Hand -> Hand -> Bool
prop_onTopOf_size p1 p2 = size p1 + size p2 == size (p1 <+ p2)

fullSuit :: Suit -> Hand
fullSuit s = fullSuitHelper s 2
	where
		fullSuitHelper :: Suit -> Integer -> Hand
		fullSuitHelper s 14 = (Add (Card Ace s) Empty)
		fullSuitHelper s 13 = (Add (Card King s) (fullSuitHelper s 14))
		fullSuitHelper s 12 = (Add (Card Queen s) (fullSuitHelper s 13))
		fullSuitHelper s 11 = (Add (Card Jack s) (fullSuitHelper s 12))
		fullSuitHelper s n = (Add (Card (Numeric n) s) (fullSuitHelper s (n + 1)))

fullDeck :: Hand
fullDeck = fullSuit Hearts <+ fullSuit Spades <+ fullSuit Diamonds <+ fullSuit Clubs

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add c h) Empty = (h, (Add c Empty))
draw (Add d_c d_h) h = (d_h, (Add d_c h))