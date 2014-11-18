module BlackJack where

import Cards
import Wrapper

import Test.QuickCheck
import System.Random

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

-- We only care about the rank when we evaluate the value of a Card, 
-- we therefore call for the valueRank function
valueCard :: Card -> Integer
valueCard c = valueRank $ rank c

-- Recursively count the number of aces in the Hand, stop when the hand is Empty
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add c h) | rank c == Ace  = 1 + numberOfAces h
                       | otherwise		= numberOfAces h

-- Count the value of the Hand, take in consideration that the Ace is 
-- evaluated to 11 if n =< 21 and 1 if n > 21, where n is the sum of all ranks
value :: Hand -> Integer
value h | valueHand > 21 = valueHand - (numberOfAces h * 10)
		| otherwise		 = valueHand
	where valueHand 	 = valueHelper h
		where
			-- Helper function to sum all the ranks of the cards
			valueHelper :: Hand -> Integer
			valueHelper Empty 		= 0
			valueHelper (Add c h)	= valueCard c + valueHelper h

-- A function checking if a hand is busted, i.e. a value over 21
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- A function that return the winning player given the rules in the assignment
winner :: Hand -> Hand -> Player
winner g b  | gameOver g 			= Bank
            | gameOver b 			= Guest
            | value b >= value g 	= Bank
            | otherwise 			= Guest

-- A function that takes two hands and puts the first hand on top of 
-- the other one
(<+) :: Hand -> Hand -> Hand
Empty   <+ h2 = h2
Add c h <+ h2 = Add c (h <+ h2)

-- A function that returns a full deck, i.e. all 13 cards in all four suits
fullDeck :: Hand
fullDeck = fullSuit 2 Hearts <+ fullSuit 2 Spades <+ 
		   fullSuit 2 Diamonds <+ fullSuit 2 Clubs
	where
		-- A function that return a full hand suit given a suit
		fullSuit :: Integer -> Suit -> Hand
		fullSuit 14 s 	= Add (Card Ace s) Empty
		fullSuit 13 s 	= Add (Card King s) (fullSuit 14 s)
		fullSuit 12 s 	= Add (Card Queen s) (fullSuit 13 s)
		fullSuit 11 s 	= Add (Card Jack s) (fullSuit 12 s)
		fullSuit n  s 	= Add (Card (Numeric n) s) (fullSuit (n + 1) s)

-- A function that draw one card from a hand and putting it on top of 
-- the other hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ 		= error "draw: The deck is empty."
draw (Add c h1) h2 	= (h1, Add c h2)

-- A function that given a hand and a number n returns a tuple with the hand 
-- without the n:th card and the n:th card
drawCardFromDeck :: Hand -> Integer -> (Card, Hand)
drawCardFromDeck (Add c h) 0 = (c, h)
drawCardFromDeck (Add c h) n = drawCardFromDeck (h <+ Add c Empty) (n - 1)

-- A function that shuffles a hand
shuffle :: StdGen -> Hand -> Hand
shuffle g h = shuffleHelper (size h - 1) g h Empty
	where
		shuffleHelper :: Integer -> StdGen -> Hand -> Hand -> Hand
		shuffleHelper _ _ Empty h2 	= h2
		shuffleHelper size g h1 h2 	= shuffleHelper (size - 1) g1 
									  oldHand (Add choosenCard h2)
			where
				(rand, g1) 				= randomR (0, size) g
				(choosenCard, oldHand)	= drawCardFromDeck h1 rand

-- A function that given a deck draws cards given the rules of the bank in 
-- the assignment
playBank :: Hand -> Hand
playBank d = playBankHelper (d, Empty)
	where
		playBankHelper :: (Hand, Hand) -> Hand
		playBankHelper (d, h)	| value h < 16 	= playBankHelper $ draw d h
							  	| otherwise 	= h


{-------------	PROP TESTS	-------------}


-- Test to see if the function <+ is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Test to see if the size of the two hands added together is the same as the 
-- size of the combined hand
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size p1 + size p2 == size (p1 <+ p2)

-- Test to see if the size of the returned deck is 52, as it is supposed to be
prop_fullDeck_size :: Bool
prop_fullDeck_size = size fullDeck == 52

-- Test to see if the size of the two hands added together is the same as the 
-- size of the two new hands added together 
prop_draw_size :: Hand -> Hand -> Bool
prop_draw_size h1 h2 = size (h1 <+ h2) == size (uncurry (<+) (draw h1 h2))

-- Test to see if a card that exist (or not exist) in a deck also exist in the 
-- same but suffled deck
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h
	where
		belongsTo :: Card -> Hand -> Bool
		c `belongsTo` Empty = False
		c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Test to see if the hand is the same size as the shuffled hand
prop_shuffle_size :: StdGen -> Hand -> Bool
prop_shuffle_size g h = size h == size (shuffle g h)


{-------------	FINAL	-------------} 

implementation = Interface {
	iEmpty 		= empty,
	iFullDeck 	= fullDeck,
	iValue 		= value,
	iGameOver 	= gameOver,
	iWinner 	= winner,
	iDraw 		= draw,
	iPlayBank 	= playBank,
	iShuffle 	= shuffle
}

main :: IO ()
main = runGame implementation