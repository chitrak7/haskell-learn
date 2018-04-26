---------------------------------------------------------------------------- 
-- Implementation of K-Nearest Neighbors
-- Authors: 
-- 	Chitrak Raj Gupta <chitrak711988@gmail.com>
--	Shaswat kar <>
----------------------------------------------------------------------------

module KNN.KNN where
import DS.Dataset
	
knnClassifier :: Dataset -> X -> Int -> Y
knnClassifier ds x n = [ y | pt <- x, let  y = solve (get_nearest n ds pt)]

solve :: [(Float, Float)] -> Float
solve ab = if (a>b) then 1 else -1
		where (a,b) =  solveHelp ab

solveHelp :: [(Float, Float)] -> (Float, Float)
solveHelp [] = (0,0)
solveHelp (x:xs) = if (snd x == -1) then (a,b+1) else (a+1,b)
			where (a,b) = solveHelp xs

get_nearest :: Int -> Dataset -> [Float] -> [(Float, Float)] 
get_nearest n (Dataset (a,b)) pt = minn n [y | i <- [0.. length a -1], let y = (euclid (a!!i) pt, b!!i)]

euclid :: [Float] -> [Float] -> Float
euclid [] [] = 0
euclid (x:xs) (y:ys) = (x-y)*(x-y) + euclid xs ys  

minn :: Int -> [(Float, Float)] -> [(Float, Float)]
minn n a = take n (quicksort a)

quicksort :: [(Float, Float)] -> [(Float,Float)]
quicksort [] = []
quicksort (x:xs) = (quicksort [y| y<-xs, fst y <= fst x]) ++ [x] ++ (quicksort [y| y<-xs, fst y > fst x]) 


