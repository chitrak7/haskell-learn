{-# LANGUAGE TemplateHaskell #-}

module SVM.SVM where

import System.IO
import System.IO.Unsafe
import System.Random
import Control.Monad
import DS.Dataset
import Data.List 

data SVMSolution = SVMSolution [Float] Float deriving (Show)

data Kernel = Linear | Quadratic

fit :: Dataset -> Kernel -> SVMSolution

fit (Dataset (x,y)) kernel = SVMSolution w b
		where   (w,b) = solveSMO x y kernel n alpha max_iter False 
			max_iter = 1000
			alpha = take n [0 ..] 
			np = length (x!!0)
			n = length x 

solveSMO :: X -> Y -> Kernel -> Int -> [Float] -> Int -> Bool -> ([Float], Float)

solveSMO x y kernel _ alpha _ True = returnVectors x y kernel alpha

solveSMO _ _ _ _ _ 0 _ = error "Maximum iterations exceeded"

solveSMO x y kernel n alpha max_iter False = solveSMO x y kernel n alpha_n (max_iter - 1) stop
		where stop = (norm (diff alpha_n alpha) < 0.01)
		      alpha_n = calcNewAlpha x y kernel n n alpha

predict :: X -> SVMSolution -> Y
predict xs (SVMSolution w b) = [y | x<-xs, let y = calcH x w b]

returnVectors :: X -> Y -> Kernel -> [Float] -> ([Float], Float)
returnVectors x y kernel alpha = let
					w = return_w x y kernel alpha
					b = return_b x y kernel w
				 in (w,b)

return_w :: X -> Y -> Kernel -> [Float] -> [Float]
return_w x y kernel alpha = 
		let z = [i | (j,k) <- zip y alpha, let i = k*j]
		in dot_p (transpose x) z

return_b :: X -> Y -> Kernel -> [Float] -> Float
return_b x y kernel w = 
			let l = dot_p x w	
			    k = diff y l
			in ((sum k)/fromIntegral(length k))			    

calcNewAlpha :: X -> Y -> Kernel -> Int -> Int -> [Float] -> [Float]
calcNewAlpha x y kernel n 0 alpha = alpha
calcNewAlpha x y kernel n i alpha = ans
	where 	ans = calcNewAlpha x y kernel n (i-1) alpha_n
		alpha_n = sub alpha_t alpha_jp j
		alpha_t = sub alpha alpha_kp k
		alpha_kp = alpha_k + y_k*y_j*(alpha_j - alpha_jp) 
		alpha_jp = if (k_jk == 0) then alpha_j else (min h (max l alpha_j + t))
		t = if(k_jk==0) then 0 else (y_j  *  (e_k - e_j) / k_jk) 
		e_k = calcE x_k y_k w b
		e_j = calcE x_j y_j w b
		(w, b) = returnVectors x y kernel alpha
		(l, h) =  calcLH alpha_j alpha_k y_j y_k
		k_jk = (calcKernel kernel x_j x_j) + (calcKernel kernel x_k x_k) - 2*(calcKernel kernel x_j x_k)
		alpha_j =  alpha!!j
		alpha_k =  alpha!!k
		y_k = y!!k
		y_j = y!!j
		x_k = x!!k 
		x_j = x!!j
		k = rand n j
		j = n - i

norm :: [Float] -> Float
norm a = sqrt (dot a a) 

dot :: [Float] -> [Float] -> Float
dot a b = sum [j  |  (i,k) <- zip a b, let j = i*k]

diff :: [Float] -> [Float] -> [Float]
diff a b = [j |  (i,k) <- zip a b, let j = i - k]  

dot_p :: X -> [Float] -> [Float]
dot_p x y = [j | z<-x, let j = dot z y]

calcH :: [Float] -> [Float] -> Float -> Float
calcH x w b = sign (dot x w + b)

calcE :: [Float] -> Float -> [Float] -> Float -> Float
calcE x y w b = y - (calcH x w b)

calcLH :: Float ->  Float -> Float -> Float -> (Float, Float)
calcLH alpha_j alpha_k y_j y_k | y_j == y_k = (max 0 (alpha_k + alpha_j - 1), min 1 (alpha_k + alpha_j))
			       | otherwise = (max 0 (alpha_j - alpha_k), min 1 (1 - alpha_k + alpha_j))

calcKernel :: Kernel -> [Float] ->  [Float] -> Float
calcKernel Linear a b = dot a b
calcKernel Quadratic a b = (dot a b)  ^ 2  

rand :: Int -> Int ->  Int
rand n i = if y == i then rand n i else y
		where y = unsafePerformIO ( randomRIO (0, n-1) )
	
sub :: [a] -> a -> Int -> [a]
sub arr obj pos = take pos arr ++ [obj] ++ drop (pos+1) arr

sign :: Float -> Float
sign x | x<0 = -1.0
       | otherwise = 1.0
