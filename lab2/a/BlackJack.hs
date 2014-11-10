module BlackJack where
import Cards
import Wrapper

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