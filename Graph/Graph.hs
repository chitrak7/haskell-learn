module Graph.Graph where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import DS.Dataset
import SVM.SVM

gh :: [(Int,Int,Int)]
gh = [(1,2,3), (3,4,5)]

x = [[1,2], [1,3], [2,3], [2,4],[-1,0],[4,5],[9,9],[3,4],[0,0], [1,-4], [-5,-5]]
y = [1,1,-1,-1,1,-1,-1,-1,1,1,1]

t = fit (Dataset (x,y)) Linear 
(w,b) = getRes t

getRes (SVMSolution a b) = (a,b)

fourPoints :: [Float] -> Float -> X -> [(Float,Float)]
fourPoints w b x = [p1,p2,p3,p4]
	where
		p1 = (x1, ((-b-a1*x1)/b1))
		p2 = (x2, ((-b-a1*x2)/b1))
		p3 = ((-b-b1*y1)/a1, y1) 
		p4 = ((-b-b1*y2)/a1, y2)
		(a1,b1) = (w!!0, w!!1)
		(x1,x2) = minmax [i!!0 | i <- x]
		(y1,y2) = minmax [i!!1 | i <- x]

minmax :: (Ord a, Num a) => [a] -> (a,a)
minmax x = (minimum x - 1, maximum x + 1)

plotSVM :: Dataset -> [Float] -> Float -> Renderable()
plotSVM (Dataset (x,y)) w b = toRenderable layout
	where
		a       = [(t!!0, t!!1) | i <- [0..length x - 1], let t = x!!i, y!!i==1 ]
                d       = [(t!!0, t!!1) | i <- [0..length x - 1], let t = x!!i, y!!i== -1 ]
	 	c       = [fourPoints w b x]
		pointsb = plot_points_style .~ filledCircles 2 (opaque red)
		        $ plot_points_values .~ a
		        $ def
		pointsa = plot_points_style .~ filledCircles 2 (opaque green)
                        $ plot_points_values .~ d
                        $ def
		line    = plot_lines_values .~ c
		        $ plot_lines_style  . line_color .~ opaque blue
		        $ def
		layout  = layout_plots .~ [toPlot pointsa, toPlot pointsb, toPlot line] 
			$ def

main = renderableToFile def "example1_big.png" (plotSVM (Dataset(x,y)) w b )

