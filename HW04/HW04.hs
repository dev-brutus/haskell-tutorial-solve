{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P []) == (P []) = True
    (P []) == (P (_:_)) = False
    (P (_:_)) == (P []) = False
    (P (x1:x1s)) == (P (x2:x2s))
        | x1 == x2 = px1s == px2s
        | otherwise = False
        where
            px1s = P x1s
            px2s = P x2s

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P []) = ""
    show (P [0]) = "0"
    show (P ps) = foldl (\ acc s -> acc ++ " + " ++ s) (head strData) (tail strData)
        where
            strData = reverse revData
            revData = filterEmpty $ map makePart $ zip psStr [0..]
            psStr = map (\n -> show n) ps
            filterEmpty :: [String] -> [String]
            filterEmpty l = filter (\ i -> not ("" == i)) l
            makePart :: (String, Integer) -> String
            makePart ("0", _) = ""
            makePart ("1", 0) = "1"
            makePart ("-1", 0) = "-1"
            makePart (c, e) = (cnst c) ++ (expn e)
            cnst :: String -> String
            cnst "1" = ""
            cnst "-1" = "-"
            cnst c = c
            expn :: Integer -> String
            expn 1 = "x"
            expn (-1) = "-x"
            expn 0 = ""
            expn e = "x^" ++ show e


-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

