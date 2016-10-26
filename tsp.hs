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


-- Monad IO gebruiken?? Dus overal waar m --> IO

----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

---------------------------------------------
-- Imports and types and starter variables
---------------------------------------------

import Data.List
import System.Random
import GA 
import Data.Ord (comparing)

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

-- variabelen: population_size enzo ook hier? dan kan je alles makkelijk vanaf bovenaan instellen

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
        else if abs slope > 1
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

-- to make a round trip of an entity, which is needed for several functions
makeRoundTrip :: Route -> Route
makeRoundTrip route = route ++ [head route]

-- all possible coordinates, needed for creating a pool (= map):
allCoordinates :: [City]
allCoordinates = [(x,y) | x <- [0..dim], y <- [0..dim]]

-- fitness:
-- calculate length of route, returned as a float
-- requires that the first and the last element of route are equal (to make a circle)
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
      dx = abs (city1x - city2x)
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

-- SCORE
-- score' function as in package
-- the lower, the better (as needed with this package)
score'' _ route = Just (calcLength (makeRoundTrip route))


-- GENRANDOM
-- genRandom: generate a random entity
-- a random entity is just a random order of the cities in our map
-- pool: a map
genRandom' pool seed = return $ randomize pool gen
  where gen = mkStdGen seed


-- NOG DIE RANDOM INDEX FUNCTIE IMPLEMENTEREN?
-- NU WORDT IEDERE KEER DE LENGTE VAN POOL UITGEREKEND?
randomize :: Map -> StdGen -> Route
randomize [] _ = []
randomize pool gen = 
  let (randomNr, newGen) = random gen
      index = randomNr `mod` (length pool)
      randomElt = pool !! index
  in randomElt: (randomize (pool\\[randomElt]) newGen)


-- hill-climbing mutatie (en ook crossover?) versnelt t gebeuren wel, maar zorgt er ook voor dat je werkt naar lokale optima..

-- 4 mutaties? RSM/PSM/ de meest basic en een eigen? 

--crossover' pool param seed ent1 ent2 =

-- parameter: % van mutaties die plaatsvinden --> GOED IDEE OM DIE STANDAARD TE ZETTEN OP PERCENTAGE VAN WAT VERANDERT?

-- TWORS MUTATIE: NU MAAK JE IEDERE KEER EEN NIEUWE ENTITY EN SWAP JE 1X PER KEER, MISSCHIEN HANDIGERE MANIER??

-- finds a random index
findRandomIndex :: StdGen -> Int -> (Int, StdGen)
findRandomIndex gen maxi =
  let (randomNr, newGen) = random gen
      index = randomNr `mod` maxi
  in (index, newGen)

-- makes a list of tuples of indexes of length n (those tuples are the indexes that get swapped with each other)
makeIndexList :: Int -> Int -> StdGen -> [(Int, Int)]
makeIndexList 0 _ _ = []
makeIndexList n maxi gen = [(index1,index2), (index2, index1)] ++ (makeIndexList (n-1) maxi newGen2)
  where (index1, newGen1) = findRandomIndex gen maxi
        (index2, newGen2) = findRandomIndex newGen1 maxi

-- swap two elements of an entity according to the given indices
--swap :: Int -> Int -> Route -> Route
--swap index1 index2 ent =
--  let len = length ent
--  in foldr (\x list -> if x == index1 then (ent!!index2):list else if x == index2 then (ent!!index1):list else (ent!!x):list) [] [0..len - 1]

-- given the list of indices that have to be swapped, calculate a new entity
--swapMutate :: [(Int, Int)] -> Route -> Route
--swapMutate [] ent = ent
--swapMutate indices ent = 
--  let (index1, index2) = head indices
--      mutatedEnt = swap index1 index2 ent
--  in swapMutate (tail indices) mutatedEnt

swapMutate :: [(Int, Int)] -> Route -> Route
swapMutate indices ent = foldr (\x list -> (find x):list) [] [0..len - 1]
  where len = length ent
        find x = case lookup x indices of
          Just a -> ent!!a
          Nothing -> ent!!x

-- MUTATION
-- mutation according to the twors scheme: randomly swap two elements of the entity
-- the param gives the percentage of the elements that get changed (if we swap one time, two elements get changed!)
-- NADENKEN OF N WEL EEN GOED GETAL IS
tworsMutation _ param seed ent = 
  let maxi = length ent
      -- we multiply by 0.5, because makeIndexList returns 2 indexes to swap for every n, so you "change" 2n element of the entity
      n = round (0.5*param*(fromIntegral maxi))
      gen = mkStdGen seed
      indices = makeIndexList n maxi gen
  in return $ Just (swapMutate indices ent)

-- CROSSOVER
-- ordered crossover: we take 2 random indices, and select from parent1 the part/slice that is between those 
-- if index1 = index2, we take the empty list. 
-- we let that slice be in the same position for the child, and before and after we fill it with the rest of the cities, 
-- in the same order as they appear in parent2
orderedCrossover _ _ seed ent1 ent2 = 
  let gen = mkStdGen seed
      maxi = length ent1
      (index', newGen) = findRandomIndex gen maxi
      (index'', _) = findRandomIndex newGen maxi
      -- we need to add 1 to the highest index in order to be able to take the end of ent1 
      -- (otherwise we can only select slices without the last element)
      (index1, index2) = case index' >= index'' of
        True -> (index'', index' + 1)
        _ -> (index', index'' + 1)
      partition = take (index2 - index1) . drop index1 $ ent1
      leftover = foldl (\newEnt x -> if elem x partition then newEnt else newEnt ++ [x]) [] ent2
  in return $ Just ((take index1 leftover) ++ partition ++ (drop index1 leftover))


--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------


instance Entity Route Float () Map IO where

  genRandom = genRandom'

  score' = score''

  mutation = tworsMutation

  crossover = orderedCrossover


main :: IO()
main = do
        let cfg = GAConfig 
                    200 -- population size
                    25 -- archive size (best entities to keep track of)
                    100 -- maximum number of generations
                    0.8 -- crossover rate (% of entities by crossover)
                    0.2 -- mutation rate (% of entities by mutation)
                    0.0 -- parameter for crossover (not used here)
                    0.2 -- parameter for mutation (% of changed cities)
                    False -- whether or not to use checkpointing
                    False -- don't rescore archive in each generation

            g = mkStdGen 0 -- random generator
        -- Do the evolution!
        -- Note: if either of the last two arguments is unused, just use () as a value

            pool = dumMap

        es <- evolveVerbose g cfg pool ()
        let e = snd $ head es :: Route
        
        --putStrLn $ "best entity (GA): " ++ (show e)

        -- Compare with random search with large budget
        -- 100k random entities, equivalent to 1000 generations of GA
        es' <- randomSearch g 100000 pool ()
        let e' = snd $ head es' :: Route
       
        putStrLn $ "best entity (random search): " ++ (show e')
