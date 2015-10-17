-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Peg -> Peg -> Peg -> Integer -> [Move]
hanoi _ _ _ 0 = []
hanoi src _ trg 1 = [(src, trg)]
hanoi src tmp trg n = prepare ++ [(src, trg)] ++ finalize
	where
		n1 = n - 1
		prepare = hanoi src trg tmp n1
		finalize = hanoi tmp src trg n1

main = print $ map hanoiLen [0..10]
	where
		hanoiLen :: Integer -> (Integer, Int)
		hanoiLen n = (n, hanoiL n)
		hanoiN = hanoi "a" "b" "c"
		hanoiL = length . hanoiN
