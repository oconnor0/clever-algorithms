{-# LANGUAGE StandaloneDeriving #-}

import Data.Function
import Data.List
import Data.Ord
import GHC.Exts
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

square :: (Num a) => a -> a
square n = n*n

squares :: (Num a) => [a] -> [a]
squares = map square

sumOfSquares :: (Num a) => [a] -> a
sumOfSquares = sum . squares

randomLists :: (Num a, Random a, RandomGen g) => g -> [[a]]
randomLists g = randomListsBy randoms g

randomListsBy :: (Num a, Random a, RandomGen g) => (g -> [a]) -> g -> [[a]]
randomListsBy by g = map fst $ tail $ iterate (\(xs, g) -> let (ga, gb) = split g in (by ga, gb)) ([], g)

randomListsOfLength :: (Num a, Random a, RandomGen g) => Int -> g -> [[a]]
randomListsOfLength len g = map (take len) $ randomLists g

findBestMatching :: (Ord a) => (b -> a) -> [b] -> b
findBestMatching score = maximumBy (comparing score)

randomSearch :: (Ord a) => (b -> a) -> Int -> [b] -> b
randomSearch score n xs = findBestMatching score $ take n xs

main :: IO ()
main =
	let
		--lists = randomListsOfLength 10 (mkStdGen 1) :: [[Integer]]
		lists = randomListsOfLength 2 (mkStdGen 1) :: [[Float]]
		best = findBestMatching (Down . sumOfSquares) (take 100 $ lists)
	in do
		putStrLn $ show $ sort $ map sumOfSquares $ take 100 $ lists
		putStrLn $ show $ sumOfSquares best
		putStrLn $ show $ best
		putStrLn $ show $ randomSearch (Down . sumOfSquares) 100 lists
	--putStrLn $ show $ (randomList 10 (mkStdGen 2) :: [Int])

-- attempt at implementing instance of Random for lists to simplify everything else
--instance (Random a) => (Random [a]) where
--	--randomR :: RandomGen g => ([a], [a]) -> g -> ([a], g)
--	randomR bounds@(minB, maxB) gen = do
--		(x, nextG) <- randomR (head minB, head maxB) gen
--		return ([x], nextG) --(a : (fst $ randomR bounds nextG))

--	--random :: RandomGen g => g -> ([a], g)
--	random gen = (random gen :: (a, g)) : (random gen :: ([a], g))

--deriving instance (Random a) => (Random [a])
