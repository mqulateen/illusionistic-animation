module Mill where

import Animation

picture :: Animation
picture = 
	translate(always(0,0)) --background
				(withPaint (rainbowDark)
					(rect(always 800)(always 600)))   `plus`

	backgroundRect  `plus`

	translate(always (380, 300))(
		(rotate(spinner 11)(
			threeCir `plus`
				innerNineCir `plus` 
					innerInnerCir `plus` 
						finalInner)))

backgroundRect :: Animation
backgroundRect = 
	combine [ 
		translate (always (x,0))
		(withPaint (rainbowLight)
			(rect(always 2.5)(always 600))) | x <- takeWhile (<=800) [0,5..]]

threeCir :: Animation	
threeCir = 
	(combine [(rotate(always x) -- 3 Rotating rect forming circle (120 degrees from one another)
		(withPaint (always black)
			(rect(always 2.5)(always 150)))) | x <- rectLocation]) `plus`
	
    combine [translate(always y) --Creates three circles, zips lists circlex and circley to form axis
    	(withBorder (always black) (always 2)
    		(withoutPaint
				(circle(always 130)))) | y <- zip circlex circley]

innerNineCir :: Animation 
innerNineCir = 
	combine[translate(always w)
		(scale(always (0.45,0.45))
			(rotate(spinner 10)(threeCir))) | w <- zip circlex circley]

innerInnerCir :: Animation
innerInnerCir = 
	combine[translate(always w)
		(scale(always (0.45,0.45))
			(rotate(spinner 10)(innerNineCir))) | w <- zip circlex circley]

finalInner :: Animation
finalInner =
	combine[translate(always w)
		(scale(always (0.45,0.45))
			(rotate(spinner 10)(innerInnerCir))) | w <- zip circlex circley]

{--------------Experimenting with recursion------------------
rcursion :: Int -> Animation -> Animation
rcursion n ani | n <= 0 = ani
       		   | otherwise = ani `plus` rcursion (n - 1) ani
------------------------------------------------------------}

circlex :: [Double]
circlex = [-131,1,129]

circley :: [Double]
circley = [-74,150,-76]

rainbowLight = cycleSmooth 7[red,yellow,aqua,lime,fuchsia,white,silver]
rainbowDark = cycleSmooth 7[maroon,olive,green,teal,navy,purple]

rectLocation :: [Double]
rectLocation = map (*2) [60,120,180]

test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture)

