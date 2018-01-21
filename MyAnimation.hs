module MyAnimation where

import Animation

-- MoQ
-- Illusionistic Animation

picture :: Animation
picture = 
	--background
	translate(always(0,0))
		(withPaint (cycleSmooth 7[maroon,olive,green,teal,navy,purple])
			(rect(always 800)(always 600)))   `plus`

	backgroundRect  `plus`  --comment this line if your eyes dont feel right :D

	--using functions defined below to create recursive circles
	translate(always (380, 300))(
		(rotate(spinner 11)(
			recursion 3)))

--creates background bars 
backgroundRect :: Animation
backgroundRect = 
	combine [ 
		translate (always (x,0))
		(withPaint (cycleSmooth 7[red,yellow,aqua,lime,fuchsia,white,silver])
			(rect(always 2.5)(always 600))) | x <- takeWhile (<=800) [0,5..]]

myShape :: Animation	
myShape = 
	-- 3 Rotating rect forming circle (120 degrees from one another)
	(combine [(rotate(always x)
		(withPaint (always black)	
			(rect(always 2.5)(always 150)))) | x <- (map (*2) [60,120,180])]) `plus`
	
	--Creates three circles, zips lists circleX and circleY to form axis
    combine [translate(always y)
    	(withBorder (always black) (always 2)
    		(withoutPaint
				(circle(always 130)))) | y <- zip circleX circleY]

--recursive call to create looping animation 
recursion :: Int -> Animation
recursion n
  | n <= 0 = myShape
  | otherwise = recursion (n-1) `plus` 
  					combine[translate(always w)
						(scale(always (0.45,0.45))
							(rotate(spinner 10)(recursion (n-1)))) | w <- zip circleX circleY]

--x and y coordinates for circles
circleX :: [Double]
circleX = [-131,1,129]

circleY :: [Double]
circleY = [-74,150,-76]

test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture)

