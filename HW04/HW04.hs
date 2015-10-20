{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
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
    show (P ps) = foldl (\ acc s -> concat [acc, " + ", s]) (head strData) (tail strData)
        where
            strData = reverse $ filterEmpty $ map polyPart $ zip ps [0..]
            filterEmpty :: [String] -> [String]
            filterEmpty l = filter (\ i -> not ("" == i)) l
            polyPart :: (Num a1, Eq a1, Show a1) => (a1, Integer) -> String
            polyPart (0, _) = ""
            polyPart (1, 0) = "1"
            polyPart ((-1), 0) = "-1"
            polyPart (c, e) = concat [cnst c, expn e]
                where
                    cnst 1 = ""
                    cnst (-1) = "-"
                    cnst n = show n
                    expn 1 = "x"
                    expn (-1) = "-x"
                    expn 0 = ""
                    expn m = "x^" ++ show m


-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus pa (P []) = pa
plus (P []) pb = pb
plus (P (pa:pas)) (P (pb:pbs)) = P (pa + pb : pcs)
    where
        P pcs = plus (P pas) (P pbs)


-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P mul1) pmul2 = sum $ map mul' $ zip mul1 pmul2Shifted
    where
        mul' (n, P p) = P (map ((*) n) p)
        pmul2Shifted = map (shift pmul2) [0..]
        shift :: Num a1 => Poly a1 -> Integer -> Poly a1
        shift m 0 = m
        shift (P m) 1 = P (0:m)
        shift (P m) n = shift pm1 n1
            where
                n1 = n - 1
                pm1 = P (0:m)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate p = p * P [-1]
    fromInteger n = P [n']
        where
            n' :: (Num b) => b
            n' = fromInteger n 
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P[]) _ = 0
applyP (P[x']) _ = x'
applyP (P (x':xs)) n = x' + n * (applyP  (P xs) n)


-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    nderiv :: Int -> a -> a
    nderiv 0 f = f
    nderiv 1 f = deriv f
    nderiv n f = nderiv (n - 1) $ deriv f
    deriv  :: a -> a

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P n) = P (tailOrZero newN)
        where
            newN = map (\(x1, x2) -> x1 * fromInteger x2) $ zip n [0..]
            tailOrZero ([]) = [0]
            tailOrZero ([_]) = [0]
            tailOrZero (_:ns) = ns

