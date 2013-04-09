{-# LANGUAGE StandaloneDeriving #-}

import System.Random

{-	http://www.cleveralgorithms.com/nature-inspired/stochastic/random_search.html
	Input: NumIterations, ProblemSize, SearchSpace
	Output: Best
	Best <- 0
	For (iter[i] in NumIterations)
	    candidate[i] <- RandomSolution(ProblemSize, SearchSpace)
	    If (Cost(candidate[i]) < Cost(Best))
	        Best <- candidate[i]
	    End
	End
	Return (Best)
-}

-- take 5 . randoms $ mkStdGen 1 :: [Int]

--newtype RandomValue v g = RandomValue (g -> (v, g))

square :: (Num n) => n -> n
square n = n*n

squares :: (Num n) => [n] -> [n]
squares = map square

sumOfSquares :: (Num n) => [n] -> n
sumOfSquares = sum . squares

randomSearch :: (Num n, RandomGen g) => (v -> n) -> (g -> (v, g)) -> Int -> g -> v
randomSearch score random n gen = fst $ random gen

--main :: IO ()
--main = show $ randomSearch

--instance (Random a) => (Random [a]) where
--	--randomR :: RandomGen g => ([a], [a]) -> g -> ([a], g)
--	randomR bounds@(minB, maxB) gen = do
--		(x, nextG) <- randomR (head minB, head maxB) gen
--		return ([x], nextG) --(a : (fst $ randomR bounds nextG))

--	--random :: RandomGen g => g -> ([a], g)
--	random gen = (random gen :: (a, g)) : (random gen :: ([a], g))

--deriving instance (Random a) => (Random [a])

--randomSearch :: (Num n, Random v, RandomGen g) => (v -> n) -> Int -> g -> v
--randomSearch score n g = from

--class
