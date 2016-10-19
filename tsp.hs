----------------------------------
-- NOTES
---------------------------------

-- ook zorgen dat t goed wordt weergegeven.
-- mogelijke volgorde van dingen doen:
-- eerst initial population (LET OP DAT NIET 2 DEZELFDE COORDINATEN MOGELIJK!!) en search population doen, en een dummie next generation (jatten?)
-- dan zorgen dat alles wordt weergegeven 
-- verschillende next generation dingetjes implementeren
-- dit alles testen op verschillende dingen, zie ideeen
-- weergave optimaliseren?

-- ideeen:
-- Wat als niks muteert en je alleen selecteert? vergelijkingen maken met verschillende percentages mutatie
-- hoeveel individueen van vorige populatie naar volgende? ("elitism"?)--> ook varieren
-- wat voor soort "maps" worden sneller opgelost? Wat als de steden al soort van in een cirkel zitten bvb


-- misschien niet zoals echte evolutie gaan zoeken naar waarom iets een goede fit is (kijken, welk stukje van de tour is kort, vergeleken met anderen?)
-- maar hoe kan je dat vergelijken?
-- kan wel zoeken naar opeenvolgende steden die in de route erg dicht bij elkaar zitten
-- je kan een soort gemiddelde afstand berekenen en dan kijken wat daar (ver?) onder ligt




-- 3 functies? InitialPopulation, NextGeneration, SearchPopulation
-- SearchPopulation : blijft de volgende generatie berekenen tot aan eis is voldaan of terminatie conditie bereikt

-- InitialPopulation : maakt de begin populatie door random tours te genereren, net zoveel als de populatie grootte

-- NextGeneration : waar t om draait! artikel: selection/mutation/crossover cycle
-- onderdelen: calcFitness : de lengte van de route
-- verschillende selection manieren bekijken (+ implementeren?)


-- met dat framework: eigen showGeneration maken


-- bepalen: dimensies van kaart en hoe weer te geven?
-- functie displayRoute

----------------------------------------------------------------------------------------------------------------------



---------------------------------------------
-- Imports and types and starter variables
---------------------------------------------

import Data.List
import GA

type City = (Integer, Integer)
type Route = [City] -- gives the cities in order of the route
type Population = [Route]
type Map = [City] -- not necessarily a route, just the coordinates of the cities
type Path = [(Integer, Integer)] -- for the coordinates of the paths between the cities

--while :: (a -> Bool) -> (a -> a) -> a -> a
--while = until . (not.)

dim :: Integer
dim = 20

big :: Float
big = 100000

-- variabelen: population_size

dumMap :: Map
dumMap = [(1,5),(20,20),(6,4),(7,15),(12,4)]

-----------------------------------------------------------------------------------------------------------------------------


------------------------------
-- STUFF FOR DISPLAYING
------------------------------

------------------------------------------------------------------
-- for displaying a map with cities
------------------------------------------------------------------

-- helper for displayMap
-- display the row for a map. Cities are represented by an x 
displayRowMap :: Map -> Integer -> String
displayRowMap cities row = foldl (\y x -> if elem (row,x) cities then y ++ "x " else y ++ "- ") "" [0..dim]

-- display the map with cities
displayMap :: Map -> Integer -> IO()
displayMap _ (-1) = return ()
displayMap cities dim = putStrLn (displayRowMap cities dim) >> displayMap cities (dim - 1)


---------------------------------------------------------------------
-- functions for calculating the points on a path between two cities
---------------------------------------------------------------------

-- First we calculate the line between two points. 
-- Since we cannot display that with IO, the points are rounded as integers and displayed with a * on the map

-- if the cities are above each other (so the line between them is represented by the function x = a for some a)
calcVertical :: City -> City -> Path
calcVertical city1 city2 =
  let x = fst city1
      (hcity, lcity) = case (snd city1 > snd city2) of
        True -> (city1, city2)
        False -> (city2, city1)
  in foldl (\a b -> (x,b):a) [] [snd lcity + 1.. snd hcity - 1]


-- if the slope > 1 for the slope of the line between the two points
calcViaY :: City -> City -> Float -> Float -> Path
calcViaY city1 city2 slope b =
	let (hcity, lcity) = case (snd city1 > snd city2) of
			True -> (city1, city2)
			False -> (city2, city1)
	in foldl (\list y -> (round ((fromInteger y - b)/slope),y):list) [] [snd lcity + 1.. snd hcity - 1]

-- rest (if the slope <= 1 and they are not directly above each other)
calcViaX :: City -> City -> Float -> Float -> Path
calcViaX city1 city2 slope b =
	let (lcity, rcity) = case (fst city1 > fst city2) of
			True -> (city2, city1)
			False -> (city1, city2)
	in foldl (\list x -> (x, round (slope*(fromInteger x) + b)):list) [] [fst lcity + 1 .. fst rcity - 1]


-- calculates the slope and the "b" of a line between two coordinates
-- y = slope*x + b
findLine :: City -> City -> (Float, Float)
findLine city1 city2 = 
  let city1x = fromInteger (fst city1)
      city1y = fromInteger (snd city1)
      city2x = fromInteger (fst city2)
      city2y = fromInteger (snd city2)
      slope = case (city1x - city2x) of
          0 -> big
          _ -> (city1y - city2y)/(city1x - city2x)
      b = city1y - slope*city1x
  in (slope, b)

-- finds all the points of the paths between the cities in a route. 
-- this function requires that the first and the last element of the route are equal (in order to make a round trip)
-- it takes the first two elements of route and finds the points of the path between them
findPath :: Route -> Path
findPath [a] = []
findPath route = 
  let city1 = head route
      city2 = head (tail route)
      (slope, b) = findLine city1 city2
      rest = tail route
      in if slope == big
        then calcVertical city1 city2 ++ findPath rest
        else if slope > 1
          then calcViaY city1 city2 slope b ++ findPath rest
          else calcViaX city1 city2 slope b ++ findPath rest

------------------------------------------------------------------
-- for displaying a route 
------------------------------------------------------------------

-- helper function for displayRoute
-- displaying the row of a route. Cities are an x, points of the path are a *
displayRowRoute :: Route -> Path -> Integer -> String
displayRowRoute route path row = foldl (\y x -> if elem (row,x) route then y ++ "x " else if elem (row,x) path then y ++ "* " else y ++ "- ") "" [0..dim]

-- display a route!
displayRoute :: Route -> Path -> Integer -> IO()
displayRoute _ _ (-1) = return ()
displayRoute route path dim = putStrLn (displayRowRoute route path dim) >> displayRoute route path (dim - 1)

---------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------
-- GENERAL STUFF FOR THE GA ALGORITHM
-----------------------------------------

-- generating the pool:
allCoordinates :: [City]
allCoordinates = [(x,y) | x <- [0..dim], y <- [0..dim]]

-- fitness:
-- calculate length of route, returned as a float
-- requires that the first and the last element of route are equal (to make a circle)
-- NOG TESTEN
calcLength :: Route -> Float
calcLength [a] = 0
calcLength route = 
  let city1 = head route
      city2 = head (tail route)
      city1x = fromInteger (fst city1)
      city1y = fromInteger (snd city1)
      city2x = fromInteger (fst city2)
      city2y = fromInteger (snd city2)
      dy = abs (city1y - city2y)
      dx = abs (city1x - city1y)
      distance = sqrt (dx^2 + dy^2)
      rest = tail route
  in distance + calcLength rest

-----------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------
-- REQUIRED FUNCTIONS
--------------------------------------------

-- copied from the package: 
-- Minimal implementation should include 'genRandom', 'crossover', 'mutation', 
-- and either 'score'', 'score' or 'scorePop'.

-- score' function as in package
score' _ route = Just (calcLength route)

