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

randomList :: (Num n, Random n, RandomGen g) => Int -> g -> [n]
randomList len g = take len $ randoms g

randomLists :: (Num n, Random n, RandomGen g) => Int -> g -> [[n]]
randomLists len g = map fst $ iterate (\(xs, g) -> randomListWithGen len g) ([], g)

randomListWithGen :: (Num n, Random n, RandomGen g) => Int -> g -> ([n], g)
randomListWithGen len g = (take len $ xs, g')
	where (xs, g') = mkRandomList [] g

mkRandomList :: (Random n, RandomGen g) => [n] -> g -> ([n], g)
mkRandomList xs g = (x:xs, g'')
	where
		(x, g') = random g
		(xs, g'') = mkRandomList xs g'

-- iterate :: (a -> a) -> a -> [a]

--randomList :: (Num n, RandomGen g, Random n) => Int -> g -> ([n], g)
--randomList n g = do
--	(x, g') <- random g
--	(xs, g'') <- randomList (n-1) g'
--	return (x:xs, g'')

main :: IO ()
--main = putStrLn $ show 1
--main = putStrLn $ show $ findBestMatching square [1,4,2,19,-1]
main = do
	putStrLn $ show $ take 2 $ (randomLists 10 (mkStdGen 1) :: [[Int]])
	--putStrLn $ show $ (randomList 10 (mkStdGen 2) :: [Int])
	putStrLn $ show $ findBestMatching square (randomList 10 (mkStdGen 1) :: [Int])
	putStrLn $ show $ findBestMatching square (randomList 10 (mkStdGen 2) :: [Int])
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
