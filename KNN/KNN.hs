---------------------------------------------------------------------------- 
-- Implementation of K-Nearest Neighbors
-- Authors: 
-- 	Chitrak Raj Gupta <chitrak711988@gmail.com>
--	Shaswat kar <>
----------------------------------------------------------------------------

module KNN where
	import DS.Dataset
	
	knnClassifier :: Dataset -> X -> Y
	knnClassifier ds x = [ y | pt <- x, let  (z,y) = get_nearest ds pt]

	get_nearest :: Dataset -> [Int] -> (Int, Int) 
	get_nearest (Dataset (a,b)) pt = min1 [y | i <- [0.. length a -1], let y = (euclid (a!!i) pt, b!!i)]

	euclid :: [Int] -> [Int] -> Int
	euclid [] [] = 0
	euclid (x:xs) (y:ys) = (x-y)*(x-y) + euclid xs ys  

	min1 :: [(Int, Int)] -> (Int, Int)
	min1 (x:[]) = x
	min1 ((a,b):xs) = let (y,z) = min1 xs
			 in if (a<y) then (a,b) else (y,z)


