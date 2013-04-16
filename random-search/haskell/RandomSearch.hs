{-# LANGUAGE StandaloneDeriving #-}

import System.Random
import Data.List

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

square :: (Num a) => a -> a
square n = n*n

squares :: (Num a) => [a] -> [a]
squares = map square

sumOfSquares :: (Num a) => [a] -> a
sumOfSquares = sum . squares

findBestMatching :: (Num n, Ord n) => (v -> n) -> [v] -> v
findBestMatching score xs = maximumBy (\a b -> compare (score a) (score b)) xs

randomList :: (Num n, Random n, RandomGen g) => g -> [n]
randomList g = map fst $ tail $ iterate (\(x, g) -> random g) (0, g)

randomLists :: (Num n, Random n, RandomGen g) => g -> [[n]]
randomLists g = map fst $ tail $ iterate (\(xs, g) -> let (ga, gb) = split g in (randomList ga, gb)) ([], g)

randomListsOfLength :: (Num n, Random n, RandomGen g) => Int -> g -> [[n]]
randomListsOfLength len g = map (take len) $ randomLists g

main :: IO ()
--main = putStrLn $ show 1
--main = putStrLn $ show $ findBestMatching square [1,4,2,19,-1]
main =
	let
		lists = randomListsOfLength 10 (mkStdGen 1) :: [[Integer]]
		best = findBestMatching sumOfSquares (take 10 $ lists)
	in do
		--putStrLn $ show $ take 10 $ lists
		putStrLn $ show $ sort $ map sumOfSquares $ take 10 $ lists
		putStrLn $ show $ sumOfSquares best
		putStrLn $ show $ best
	--putStrLn $ show $ (randomList 10 (mkStdGen 2) :: [Int])
--main = putStrLn $ show $ randomSearch sumOfSquares (randomList 5) 1 (mkStdGen 1)

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
