module MyAnimation where

import Animation

picture :: Animation
picture = 
	background `plus`

	--circle in centre of animation
	(combine [translate(always (380,300))
		(withPaint (cycleSmooth 10[teal,navy,maroon,green])	
			(circle (always 150)))])

	`plus`

	--defining recursive level
	translate(always (380, 300))(
		(rotate(spinner 13)(
			recur 3)))

shape :: Animation	
shape = 
	-- three rectangles making a circle
	(combine [(rotate(always i)
		(withPaint (always white)	
			(rect(always 3)(always 160)))) | i <- (position)]) `plus`
	
	-- three outer triangles making a circle
    combine [rotate(always out)
    	(withBorder (always white) (always 3)
    		(withGenPaint (cycleSmooth 10[maroon,green,teal,navy]) (always 0.75)
				(polygon [(120,-30), (0,110), (240, 110)]))) | out <- (position)] 

--recursive inner triangle animation 
recur :: Int -> Animation
recur n
  | n <= 0 = shape
  | otherwise = recur (n-1) `plus` 
  					combine[rotate(always inn)
						(scale(always (0.5,0.5))
							(rotate(spinner 15)(recur (n-1)))) | inn <- (position)]

background :: Animation
background = translate(always(0,0))
				(withPaint (always black)
					(rect(always 800)(always 600)))
 
-- three points making circle (120, 240, 360)
position :: [Double]
position = map (*3) [40,80,120]

test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture)

