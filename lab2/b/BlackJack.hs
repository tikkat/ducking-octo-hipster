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

-- Todo: Document and prop_testing

empty :: Hand
empty = Empty

valueRank :: Rank -> Integer
valueRank (Numeric n)   = n
valueRank Ace           = 11
valueRank _             = 10

valueCard :: Card -> Integer
valueCard (Card r _) = valueRank r

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r _) h) | r == Ace      = 1 + numberOfAces h
                                | otherwise     = numberOfAces h

value :: Hand -> Integer
value h | valueHelper h > 21 = valueHelper h - (numberOfAces h * 10)
		| otherwise = valueHelper h
	where
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

(<+) :: Hand -> Hand -> Hand
Empty              <+ h2 = h2
(Add (Card r s) h) <+ h2 = (Add (Card r s) (h <+ h2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

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
draw (Add (Card d_r d_s) d_h) Empty = (d_h, (Add (Card d_r d_s) Empty))
draw (Add (Card d_r d_s) d_h) (Add (Card h_r h_s) h_h) = (d_h, (Add (Card d_r d_s) (Add (Card h_r h_s) h_h)))