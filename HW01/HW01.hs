{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n
	| n  <= 0 = []
	| otherwise = (lastDigit n) : (toRevDigits (dropLastDigit n))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [i] = [i]
doubleEveryOther (i1:i2:t) = [i1, i2 * 2] ++ doubleEveryOther t

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
	| x < 10 = x
	| otherwise = sumDigits (toRevDigits x)
sumDigits (x:xs) = (sumDigits [x]) + sumDigits xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = 0 == lastDigit sd
	where sd = sumDigits $ doubleEveryOther $ toRevDigits x

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
