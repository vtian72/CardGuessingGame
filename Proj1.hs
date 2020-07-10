--  File     : Proj1.hs
--  Author   : Haoyu (Vincent) Tian
--  Purpose  : An implementation of a guessing card game

-- | This code implements a guessing card game, where the user
--   continuously guesses a possible card combination until
--   the correct answer is achieved.

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

-- | Declare the GameState type as a list of remaining possible cards.
type GameState = [[Card]]

-- | Function that returns the feedback information based on a target
--   and an answer and returns the five feedback numbers, 
--   (number of correct cards, number of lower ranked cards, number of
--   correct ranks, number of higher ranked cards and number of correct suits)
--   as a tuple. 
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback [] _ = (0,0,0,0,0)
feedback _ [] = (0,0,0,0,0)
feedback target guess =
                  (numCorrect target guess 
                  ,rankLower (cardRanks target) (minimum (cardRanks guess))
                  ,removeCount (cardRanks target) (cardRanks guess)
                  ,rankHigher (cardRanks target) (maximum (cardRanks guess))
                  ,removeCount (cardSuits target) (cardSuits guess)
                  )

-- | Function that takes in the number of cards to guess and then returns
--   the initial guess, which is a list of cards as a guess and the combination
--   of all possible remaining cards to guess without repeats.
initialGuess :: Int -> ([Card], GameState)
initialGuess num = (initialOptions num, combinations num cards)
    where cards = ([minBound..maxBound] :: [Card])


-- | Function that takes in a list of cards as the guess, the current gamestate
--   and the current feedback and then returns a list of cards as the next guess,
--   and the new gamestate(possible card combinations that give same feedback as the
--   answer)
nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (guess, gamestate) fdback
    -- length newGameState > 1000 = (head newGameState, newGameState)
    -- otherwise = (newGuess, newGameState) 
    | length guess == 2 =  (newGuess, newGameState) 
    | otherwise = (head newGameState, newGameState)
    where newGameState = [t | t <- gamestate, feedback t guess == fdback]
          newGuess = bestGuess newGameState

-- *********************************************************
-- ************* HELPER FUNCTIONS FOR FEEDBACK**************
-- *********************************************************

-- | Function that takes in a list of cards as the guess, the current gamestate
--   and the current feedback and then returns a list of cards as the next guess,
--   and the new gamestate(possible card combinations that give same feedback as the
--   answer)
numCorrect :: [Card] -> [Card] -> Int
numCorrect [] _ = 0
numCorrect (t:ts) gs
        | t `elem` gs = 1 + numCorrect ts gs
        | otherwise = numCorrect ts gs

-- | Function that takes in the list of ranks of the target and the minimum rank in the guess
--   and returns the number of ranks in the target that is smaller than the minimum rank of 
--   the guess.
rankLower :: [Rank] -> Rank -> Int
rankLower [] _ = 0
rankLower (t:ts) r 
    | t < r = 1 + rankLower ts r
    | otherwise = rankLower ts r

-- | Function that takes in the list of ranks of the target and the maximum rank in the guess
--   and returns the number of ranks in the target that is bigger than the maximum rank of 
--   the guess.
rankHigher :: [Rank] -> Rank -> Int
rankHigher [] _ = 0
rankHigher (t:ts) r 
    | t > r = 1 + rankHigher ts r
    | otherwise = rankHigher ts r

-- | Function that takes in 2 lists of values, iterates through the first list, removes same
--   value if it exists in the same list and counts the number of removed values.
removeCount :: Eq a => [a] -> [a] -> Int
removeCount [] _ = 0
removeCount _ [] = 0
removeCount (t:ts) (g:gs)
    | t `elem` (g:gs) = 1 + removeCount ts (delete t (g:gs))
    | otherwise = removeCount ts (g:gs)

-- | Function that takes a list of cards and returns a list of ranks
cardRanks :: [Card] -> [Rank]
cardRanks [] = []
cardRanks (x:xs) = (rank x):(cardRanks xs)

-- | Function that takes a list of cards and returns a list of suits
cardSuits :: [Card] -> [Suit]
cardSuits [] = []
cardSuits (x:xs) = (suit x):(cardSuits xs)


-- *********************************************************
-- ********** HELPER FUNCTIONS FOR INITIAL GUESS************
-- *********************************************************

-- | Function that takes in the number of cards to guess and returns a 
--   pre-determined list of the cards in the guess list
initialOptions :: Int -> [Card]
initialOptions num 
    | num == 2 = [Card Diamond R6, Card Spade R10]
    | num == 3 = [Card Diamond R4, Card Spade R8, Card Club Jack]
    | num == 4 = [Card Diamond R4, Card Spade R7, Card Club R10, Card Heart King]

-- | Function that takes in a number and a list of values and returns
--   a list of lists containing all the possible combinations of the input size
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations num (g:gs) = map (g:) (combinations (num-1) gs) ++ combinations num gs

-- *********************************************************
-- ************ HELPER FUNCTIONS FOR NEXT GUESS*************
-- *********************************************************

-- | Function that takes in a list of values and returns a list of tupes containing
--   each distinct value and the number of occurences of that value
groupFeedback :: (Eq a, Ord a) => [a] -> [(a,Int)]
groupFeedback = map (\feedbacks -> (head feedbacks, length feedbacks)) . group . sort 

-- | Function that takes in a list of cards and list of list of cards and returns
--   all the possible feedback results in a list
listFeedback :: [Card] -> [[Card]] -> [(Int, Int, Int, Int, Int)]
listFeedback _ [] = []
listFeedback guess (t:ts) = feedback t guess:listFeedback guess ts

-- | Function that takes in a guess and a list of possible answers, and returns
--   the weight of that guess
weight :: [Card] -> GameState -> Int
weight guess gamestate = (sum (map (^2) size)) `div` (sum size)
    where   feedbacks = groupFeedback (listFeedback guess gamestate)
            size = map snd feedbacks

-- | Function that takes in a gamestate and itself and returns a list of all
--   the possible weights
allWeights :: GameState -> GameState -> [Int]
allWeights [] _ = []
allWeights _ [] = []
allWeights gs gamestate = [weight g gamestate| g <- gs]

-- | Function that takes in a gamestate and returns the best guess from the 
--   gamestate
bestGuess :: GameState -> [Card]
bestGuess [a] = a
bestGuess gamestate = gamestate!!index
    where expectedNumbers = allWeights gamestate gamestate
          index = toInt (elemIndex (minimum expectedNumbers) expectedNumbers)

-- | Function to convert Maybe Int to Int
toInt :: Maybe Int -> Int
toInt Nothing = -1
toInt (Just i) = i
