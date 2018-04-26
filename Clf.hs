module Clf where

import DS.Dataset
import SVM.SVM
import KNN.KNN
import Graph.Graph
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

fitSVM :: Dataset -> Kernel -> SVMSolution
fitSVM = fit

fitKNN :: Dataset -> X -> Int -> Y
fitKNN = knnClassifier 

graphSVM t kernel = renderableToFile def "svm.png" (plotSVM t w b )
	where
		(SVMSolution w b) = fitSVM t kernel
