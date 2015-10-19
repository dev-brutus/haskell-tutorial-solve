{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches _ [] = 0
exactMatches [] _ = 0
exactMatches c1 c2
	| ch1 == ch2 = 1 + tailMatches
	| otherwise = tailMatches
	where
		ch1 = head c1
		ch2 = head c2
		ct1 = tail c1
		ct2 = tail c2
		tailMatches = exactMatches ct1 ct2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (countColor code) colors
	where
		countColor :: Code -> Peg -> Int
		countColor [] _ = 0
		countColor [c] peg
			| c == peg = 1
			| otherwise = 0
		countColor (c:codes) peg
			| c == peg = 1 + countColor codes peg
			| otherwise = countColor codes peg

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ map minInTuple zippedColors
	where
		cc1 = countColors c1
		cc2 = countColors c2
		zippedColors :: [(Int, Int)]
		zippedColors = zip cc1 cc2
		minInTuple :: (Int, Int) -> Int
		minInTuple (x, y) = min x y

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact (preNonExact - exact)
	where
		exact = exactMatches secret guess
		preNonExact = matches secret guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move secret = move == getMove secret guess
	where
		extractGuess :: Move -> Code
		extractGuess (Move g _ _) = g
		guess = extractGuess move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (\code -> isConsistent move code) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
