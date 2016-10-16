-- ook zorgen dat t goed wordt weergegeven.
-- mogelijke volgorde van dingen doen:
-- eerst initial population en search population doen, en een dummie next generation (jatten?)
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

type City = (Float, Float)
type Route = [City]
type Population = [Route]
type Map = [City] 

dim :: Int
dim = 20

-- variabelen: population_size



-- 3 functies? InitialPopulation, NextGeneration, SearchPopulation
-- SearchPopulation : blijft de volgende generatie berekenen tot aan eis is voldaan of terminatie conditie bereikt

-- InitialPopulation : maakt de begin populatie door random tours te genereren, net zoveel als de populatie grootte

-- NextGeneration : waar t om draait! artikel: selection/mutation/crossover cycle
-- onderdelen: calcFitness : de lengte van de route
-- verschillende selection manieren bekijken (+ implementeren?)


-- met dat framework: eigen showGeneration maken


-- bepalen: dimensies van kaart en hoe weer te geven?
-- functie displayRoute

displayRow :: [Int] -> IO()
displayRow cities = 
	let str = foldl (\y x -> if elem x cities then y ++ "x" else y ++ "-") "" [0..20]
	in do putStrLn str



displayMap :: IO()
displayMap = do
	putStrLn "--+---------+---x-----*---"
	putStrLn "+------+----------x----*--"


--displayRoute :: Route -> IO()
--displayRoute route = 