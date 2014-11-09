module BlackJack where
import Cards
import Wrapper

-- Todo: Describe what happens in the size function.
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

-- Todo: Fix for aces when value > 21. Calculate total value of hand and remove 10 * numberOfAces if value > 21? 
value :: Hand -> Integer
value Empty = 0
value (Add c h) = valueCard c + value h

gameOver :: Hand -> Bool
gameOver h  | value h > 21 = True
            | otherwise = False

winner :: Hand -> Hand -> Player
winner g b  | gameOver g == True = Bank
            | gameOver b == True = Guest
            | value b >= value g = Bank
            | otherwise = Guest