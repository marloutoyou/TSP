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

import Data.List

type City = (Integer, Integer)
type Route = [City]
type Population = [Route]
type Map = [City]
type Path = [(Integer, Integer)] 

--while :: (a -> Bool) -> (a -> a) -> a -> a
--while = until . (not.)

dim :: Integer
dim = 20

-- variabelen: population_size

dumMap :: Map
dumMap = [(1,5),(20,20),(6,4),(7,15),(12,4)]

-- 3 functies? InitialPopulation, NextGeneration, SearchPopulation
-- SearchPopulation : blijft de volgende generatie berekenen tot aan eis is voldaan of terminatie conditie bereikt

-- InitialPopulation : maakt de begin populatie door random tours te genereren, net zoveel als de populatie grootte

-- NextGeneration : waar t om draait! artikel: selection/mutation/crossover cycle
-- onderdelen: calcFitness : de lengte van de route
-- verschillende selection manieren bekijken (+ implementeren?)


-- met dat framework: eigen showGeneration maken


-- bepalen: dimensies van kaart en hoe weer te geven?
-- functie displayRoute

displayRowMap :: Map -> Integer -> String
displayRowMap cities row = foldl (\y x -> if elem (row,x) cities then y ++ "x " else y ++ "- ") "" [0..dim]


displayMap :: Map -> Integer -> IO()
displayMap _ (-1) = return ()
displayMap cities dim = putStrLn (displayRowMap cities dim) >> displayMap cities (dim - 1)


-- slope == 100000
calcVertical :: City -> City -> Path
calcVertical city1 city2 =
  let x = fst city1
      (hcity, lcity) = case (snd city1 > snd city2) of
        True -> (city1, city2)
        False -> (city2, city1)
	in foldl (\a b -> (x,b):a) [] [snd lcity + 1.. snd hcity - 1]


-- slope > 1
calcViaY :: City -> City -> Float -> Float -> Path
calcViaY city1 city2 slope b =
	let (hcity, lcity) = case (snd city1 > snd city2) of
			True -> (city1, city2)
			False -> (city2, city1)
	in foldl (\list y -> (round ((fromInteger y - b)/slope),y):list) [] [snd lcity + 1.. snd hcity - 1]

-- rest
calcViaX :: City -> City -> Float -> Float -> Path
calcViaX city1 city2 slope b =
	let (lcity, rcity) = case (fst city1 > fst city2) of
			True -> (city2, city1)
			False -> (city1, city2)
	in foldl (\list x -> (x, round (slope*(fromInteger x) + b)):list) [] [fst lcity + 1 .. fst rcity - 1]


-- zorgen dat de 1e en laatste ook geconnect worden
findPath :: Route -> Path
findPath [a] = []
findPath route = 
  let city1 = head route
      city2 = head (tail route)
      city1x = fromInteger (fst city1)
      city1y = fromInteger (snd city1)
      city2x = fromInteger (fst city2)
      city2y = fromInteger (snd city2)
      slope = case (city1x - city2x) of
          0 -> 100000
          _ -> (city1y - city2y)/(city1x - city2x)
      b = city1y - slope*city1x
      -- function is now slope*x + b
      rest = tail route
      in if slope == 100000
        then calcVertical city1 city2 ++ findPath rest
        else if slope > 1
          then calcViaY city1 city2 slope b ++ findPath rest
          else calcViaX city1 city2 slope b ++ findPath rest


displayRowRoute :: Route -> Path -> Integer -> String
displayRowRoute route path row = foldl (\y x -> if elem (row,x) route then y ++ "x " else if elem (row,x) path then y ++ "* " else y ++ "- ") "" [0..dim]

displayRoute :: Route -> Path -> Integer -> IO()
displayRoute _ _ (-1) = return ()
displayRoute route path dim = putStrLn (displayRowRoute route path dim) >> displayRoute route path (dim - 1)
