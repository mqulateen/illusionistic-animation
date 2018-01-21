module MyAnimation where

import Animation

picture :: Animation
picture = 
	background `plus`

	--defining recursive level
	translate(always (380, 300))(
		(rotate(spinner 13)(
			recur 2)))

shape :: Animation	
shape = 
	-- 3 Rotating rect forming circle (120 degrees from one another)
	(combine [(rotate(always i)
		(withPaint (always white)	
			(rect(always 3)(always 160)))) | i <- (map (*2) [60,120,180])]) `plus`
	
	--base triangles, x/y axis zipped together
    combine [translate(always out)
    	(withBorder (always white) (always 3)
    		(withGenPaint (cycleSmooth 10[maroon,green,teal,navy]) (always 0.75)
				(polygon [(120,-30), (0,110), (240, 110)]))) | out <- zip outerTriX outerTriY]

--recursive inner triangle animation 
recur :: Int -> Animation
recur n
  | n <= 0 = shape
  | otherwise = recur (n-1) `plus` 
  					combine[translate(always inn)
						(scale(always (0.5,0.5))
							(rotate(spinner 15)(recur (n-1)))) | inn <- zip innerTriX innerTriY]

background :: Animation
background = translate(always(0,0))
				(withPaint (always black)
					(rect(always 800)(always 600)))

--triangle coordinates 
outerTriX :: [Double]
outerTriX = [-250,-120,10]

outerTriY :: [Double]
outerTriY = [-134,90,-134]

innerTriX :: [Double]
innerTriX = [-130,0,130]

innerTriY :: [Double]
innerTriY = [-74,150,-74]

test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture)

