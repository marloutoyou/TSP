----------------------------------
-- NOTES AND STUFF TO BEGIN WITH
---------------------------------
-- FSA: Marlou Gijzen, 10430695

-- this contains code for solving TSP with a genetic algorithm.
-- For this, the package GA is used.
-- there are two crossover and two mutation operators
-- one of them is random, and the other is selective (in both cases)
-- it also contains code for displaying the routes in the terminal
-- at the top of this code (below the imports), you can set a dimension and a map
-- at the bottom of this code, you can set the crossover and mutation functions 
-- if you want to use a very big map, that cannot be displayed,
-- then you can uncomment the showGen = .. in the entity declaration
-- then the routes will not be displayed in a grid but as a list of tuples

----------------------------------------------------------------------------------------------------------------------
-- needed for the instance declaration

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

big :: Float
big = 100000

-- needed for one of the functions
noCity :: City
noCity = (-1, -1)

------------------------
-- these you can adjust: 

-- the dimension of the map
dim :: Integer
dim = 1750

-- the map used as a pool
dummyMap :: Map
dummyMap = [(1,1),(11,1),(19,1),(19,9),(19,19),(10,19),(1,19),(1,3)]

-- dimension: 1750
berlin52 :: Map
berlin52 = [(565, 575),(25, 185), (345, 750), (945, 685), (845, 655), (880, 660), (25, 230), (525, 1000), (580, 1175), (650, 1130), (1605, 620), (1220, 580), (1465, 200), (1530, 5), (845, 680), (725, 370), (145, 665), (415, 635), (510, 875), (560, 365), (300, 465), (520, 585), (480, 415), (835, 625), (975, 580), (1215, 245), (1320, 315), (1250, 400), (660, 180), (410, 250), (420, 555), (575, 665), (1150, 1160), (700, 580), (685, 595), (685, 610), (770, 610), (795, 645), (720, 635), (760, 650), (475, 960), (95, 260), (875, 920), (700, 500), (555, 815), (830, 485), (1170, 65), (830, 610), (605, 625), (595, 360), (1340, 725), (1740, 245)]


-----------------------------------------------------------------------------------------------------------------------------
------------------------------
-- STUFF FOR DISPLAYING
------------------------------

------------------------------------------------------------------
-- for displaying a map with cities


-- helper for displayMap
-- display the row for a map. Cities are represented by an x 
displayRowMap :: Map -> Integer -> String
displayRowMap cities row = foldl (\y x -> if (x,row) `elem` cities then y ++ "x " else y ++ "- ") "" [0..dim]

-- display the map with cities
-- not used in the algorithm, but can be useful for yourself
displayMap :: Map -> IO()
displayMap cities = displayMap' cities dim

displayMap' :: Map -> Integer -> IO()
displayMap' _ (-1) = return ()
displayMap' cities dim = putStrLn (displayRowMap cities dim) >> displayMap' cities (dim - 1)


---------------------------------------------------------------------
-- functions for calculating the points on a path between two cities
-- these points are needed for displaying the routes


-- First we calculate the line between two points (cities). The line is y=slope*x + b
-- Since we cannot display that with IO, the points are rounded as integers and displayed with a * on the map

-- if the cities are above each other (so the line between them is represented by the function x = a for some a)
-- find out which city is the higher one and which the lower, and then create a list of the coordinates of the path between them
calcVertical :: City -> City -> Path
calcVertical city1 city2 =
  let x = fst city1
      (hcity, lcity) = if snd city1 > snd city2 then (city1, city2) else (city2, city1)
  in foldl (\a b -> (x,b):a) [] [snd lcity + 1.. snd hcity - 1]


-- if the |slope| > 1 for the slope of the line between the two points
-- then we want to calculate the path via y values (so we get every different x value)
calcViaY :: City -> City -> Float -> Float -> Path
calcViaY city1 city2 slope b =
	let (hcity, lcity) = if snd city1 > snd city2 then (city1, city2) else (city2, city1)
      -- create a list of (x,y), where x is found using the y=slope*x + b
	in foldl (\list y -> (round ((fromInteger y - b)/slope),y):list) [] [snd lcity + 1.. snd hcity - 1]

-- rest (if the |slope| <= 1 and they are not directly above each other)
-- then we want to calculate the path from the x values, so we get every different y value
calcViaX :: City -> City -> Float -> Float -> Path
calcViaX city1 city2 slope b =
	let (lcity, rcity) = if fst city1 > fst city2 then (city2, city1) else (city1, city2)
      -- create a list of (x,y), where y is found using the y=slope*x + b
	in foldl (\list x -> (x, round (slope*fromInteger x + b)):list) [] [fst lcity + 1 .. fst rcity - 1]


-- calculates the slope and the "b" of a line between two coordinates
-- y = slope*x + b
findLine :: City -> City -> (Float, Float)
findLine city1 city2 = 
  let city1x = fromInteger (fst city1)
      city1y = fromInteger (snd city1)
      city2x = fromInteger (fst city2)
      city2y = fromInteger (snd city2)
      slope = case city1x - city2x of
          0 -> big -- actually nearing infinity.. but this is good enough, we just need to differentiate between vertical lines and the rest
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


-- helper function for displayRoute
-- displaying the row of a route. Cities are an x, points of the path are a *
displayRowRoute :: Route -> Path -> Integer -> String
displayRowRoute route path row = foldl (\y x -> if (x,row) `elem` route then y ++ "x " else if (x,row) `elem` path then y ++ "* " else y ++ "  ") "" [0..dim] ++ "\n"

-- display a route!
-- it is a string, so we can use it for the display functions that were in the GA algorithm
-- requires that the first and last element of route are the same
displayRoute :: Route -> Path -> Integer -> String
displayRoute _ _ (-1) = ""
displayRoute route path dim = displayRowRoute route path dim ++ displayRoute route path (dim - 1)


---------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------
-- GENERAL STUFF NEEDED FOR THE GA ALGORITHM 
---------------------------------------------

-- to make a round trip of an entity, which is needed for several functions
makeRoundTrip :: Route -> Route
makeRoundTrip route = route ++ [head route]

--------------------------------------------------
-- calculating the length of a route
-- for the fitness (in GA, the lower the fitness, the better!)

-- calculate length between two cities
calcLength :: City -> City -> Float
calcLength city1 city2 = 
  let city1x = fromInteger (fst city1)
      city1y = fromInteger (snd city1)
      city2x = fromInteger (fst city2)
      city2y = fromInteger (snd city2)
      dy = abs (city1y - city2y)
      dx = abs (city1x - city2x)
  in sqrt (dx^2 + dy^2)

-- calculate length of route, returned as a float
-- requires that the first and the last element of route are equal (to make a circle)
calcTotalLength :: Route -> Float
calcTotalLength [a] = 0 -- we're done if there's only one city left
calcTotalLength route = 
  let city1 = head route
      city2 = head (tail route)
      distance = calcLength city1 city2
      rest = tail route
  in distance + calcTotalLength rest


----------------------------------------------------------------------
-- for creating random routes

-- create a random order of the cities in our map 
-- this will be used to create random routes
randomize :: Map -> StdGen -> Route
randomize [] _ = []
randomize pool gen = 
  let maxi = length pool
      (index, newGen) = findRandomIndex gen maxi
      randomElt = pool!!index
  in randomElt: randomize (pool\\[randomElt]) newGen

-- finds a random index
findRandomIndex :: StdGen -> Int -> (Int, StdGen)
findRandomIndex gen maxi =
  let (randomNr, newGen) = random gen
      index = randomNr `mod` maxi
  in (index, newGen)

-------------------------------------------------------------------------
-- stuff with indices, needed for the mutation

-- makes a list of length 2n of tuples of (random) indexes (those tuples are the indexes that get swapped with each other)
-- so we get a list of 2n different indices that need to be swapped with each other
makeIndexList :: Int -> Int -> StdGen -> [(Int, Int)]
makeIndexList 0 _ _ = []
makeIndexList n maxi gen = [(index1,index2), (index2, index1)] ++ makeIndexList (n-1) maxi newGen2 -- we need them double to use `lookup`
  where (index1, newGen1) = findRandomIndex gen maxi
        (index2, newGen2) = findRandomIndex newGen1 maxi

-- given the list of indices that have to be swapped, calculate a new entity
-- if the index is one that needs to be swapped, use the other value
-- if not, use the old value
swapIndices :: [(Int, Int)] -> Route -> Route
swapIndices indices ent = foldr (\x list -> find x:list) [] [0..len - 1]
  where len = length ent
        find x = case lookup x indices of
          Just a -> ent!!a
          Nothing -> ent!!x

------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------
-- FUNCTIONS FOR THE SELECTIVE ALGORITHMS
-------------------------------------------------------------


-- calculates the average distance between two cities of a route
-- requires that the first and last element of the route are the same
averageDistance :: Route -> Float
averageDistance route =
  let n = length route
      totalDistance = calcTotalLength route
  in totalDistance / fromIntegral n

-- finds tuples of cities that appear after each other in a route,
-- and for which the distance between the two is smaller or equal than average 
-- (only smaller than average would give problems if all distances were equal)

-- let the tuples appear in the same order as the route!!
-- requires that the first and last element of the route are the same
findSmallDistances :: Route -> Float -> [(City,City)]
findSmallDistances [a] _ = []
findSmallDistances route average = 
  let city1 = head route
      city2 = head (tail route)
      distance = calcLength city1 city2
      rest = tail route 
      -- there is no empty tuple...
      tuple = if distance <= average then (city1, city2) else (noCity, noCity)
  in tuple:findSmallDistances rest average

-- calculates the number of adjecent tuples/cities that were given by findSmallDistances,
-- starting with city
-- that gives the length of a subpath (starting with city) from the route, for which all the distances between the cities are <= average 
-- the result is then "(length, leftover tuples from the small distances)"  
-- (leftover tuples are cities that were not adjecent to the previous one in the route)
lengthSlice :: [(City, City)] -> City -> Int -> (Int, [(City, City)])
lengthSlice [] city n = (n, [])
lengthSlice smalls city n =
  if fst (head smalls) == city then
    lengthSlice (tail smalls) (snd (head smalls)) (n + 1)
  else
    (n, smalls)


-- there has to be at least one tuple wich is not a noCity (pigeonhole principle) in the result of findSmallDistances
-- the tuples have appear in order of the route 
-- so now we find all small subpaths, we want to know the length and with which city they start
-- the result is a list of the above information
calcSmallLengths :: [(City, City)] ->  [(Int,City)]
calcSmallLengths [] = []
calcSmallLengths smalls =
  let tuple = head smalls
  in if tuple == (noCity, noCity) then 
      calcSmallLengths (tail smalls)
    else 
      let a = fst tuple -- a city, the starting point of a small subpath 
          b = snd tuple 
          -- we now look if the small subpath continues from b, aka, does the next tuple start with b?
          -- if yes, continue! if no, the subpath has length 1
          (n, rest) = lengthSlice (tail smalls) b 1
      in (n, a):calcSmallLengths rest

-- finds the longest small subpath! that is just the result from calsSmallLengths with the longest length
getLongestSmall :: Route -> (Int, City)
getLongestSmall route =
  let trip = makeRoundTrip route
      average = averageDistance trip
      smalls = findSmallDistances trip average
      lengths = calcSmallLengths smalls
  in maximum lengths



-----------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------
-- FUNCTIONS FOR THE GA (so no types)
--------------------------------------------

-- copied from the package: 
-- "Minimal implementation should include 'genRandom', 'crossover', 'mutation', 
-- and either 'score'', 'score' or 'scorePop'.
-- The 'isPerfect', 'showGeneration' and 'hasConverged' functions are optional."

--------------------------------------------
-- SHOWGENERATION

-- for displaying the route during the evolving. Same types as showGeneration in the package
showRoute gi (_, archive) = "best entity (gen. " ++ show gi ++ "): "
                            ++ "\n"
                            ++ displayRoute route path dim
                            ++ "\n"
                            ++ " [fitness: " ++ show fitness ++ "]"
                          where
                            (Just fitness, e) = head archive
                            route = makeRoundTrip e
                            path = findPath route

-- SCORE
-- score' function as in package
-- the lower, the better (as needed with this package)
-- the score of a route is then just it's length
score'' _ route = Just (calcTotalLength (makeRoundTrip route))

-----------------------------------------------
-- GENRANDOM

-- genRandom: generate a random entity
-- a random entity is just a random order of the cities in our map
-- thus: pool= a map
genRandom' pool seed = return $ randomize pool gen
  where gen = mkStdGen seed

-------------------------------------------------
-- MUTATION

-- Random mutation:
-- mutation according to the twors scheme: randomly swap elements of the entity (so more then two in this case)
-- the param gives the percentage of the elements that get changed (if we swap one time, two elements get changed!)
-- If we give n to makeIndexList, we get 2n indexes (that we swap with each other)
-- so, 2n elements get changed.
tworsMutation _ param seed ent = 
  let maxi = length ent
      -- we multiply by 0.5, so the parameter corresponds to the percentage of changed elements
      n = round (0.5*param*fromIntegral maxi)
      gen = mkStdGen seed
      indices = makeIndexList n maxi gen
  in return $ Just (swapIndices indices ent)

-- Selective mutation:
-- randomly swap two elements, but not elements that appeared in the longest small subpath!
selectiveMutation _ _ seed ent =
  let longestSmall = getLongestSmall ent
      -- the index of the city that is at the beginning of the longest small subpath
      Just a = elemIndex (snd longestSmall) ent
      -- the index of the city that ends the longest small subpath
      b = a + fst longestSmall
      len = length ent
      gen = mkStdGen seed
      -- indices that we are allowed to swap (so no indexes from the longest small subpath)
      leftoverIndices = [x | x <- [0..len- 1], x < a || x > b]
      -- we will now randomly select two indices from these leftovers:
      -- we select two random indices for the leftover list and take the corresponding elements
      maxi = length leftoverIndices
      indices = makeIndexList 1 maxi gen
      index1' = fst (head indices)
      index1 = leftoverIndices!!index1'
      index2' = snd (head indices)
      index2 = leftoverIndices!!index2'
      -- return the entity with the two indices swapped
  in return$ Just (foldr (\x list -> if x == index1 then (ent!!index2):list else if x == index2 then (ent!!index1):list else (ent!!x):list) [] [0..len - 1])

-----------------------------------------------------
-- CROSSOVER

-- Random crossover:
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
      (index1, index2) = if index' >= index'' then (index'', index' + 1) else (index', index'' + 1)
      -- the slice that we'll select from parent1/ent1:
      partition = take (index2 - index1) . drop index1 $ ent1
      -- the cities that do not appear in that slice, from parent2/ent2, in the order as they appear
      leftover = foldl (\newEnt x -> if x `elem` partition then newEnt else newEnt ++ [x]) [] ent2
      -- combine!
  in return $ Just (take index1 leftover ++ partition ++ drop index1 leftover)

-- Selective crossover:
-- as the ordered crossover, only now we select not a random partition from ent1, but the longest small subpath
-- we then fill it again with cities from ent2 in the same order
selectiveCrossover _ _ _ ent1 ent2 =
  let longestSmall = getLongestSmall ent1 -- == (length of slice, starting point)
      Just a = elemIndex (snd longestSmall) ent1
      b = a + fst longestSmall + 1
      partition = take (b - a) . drop a $ ent1 -- this is now precisely the small slice
      leftover = foldl (\newEnt x -> if x `elem` partition then newEnt else newEnt ++ [x]) [] ent2
  in return $ Just (take a leftover ++ partition ++ drop a leftover)


--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------


instance Entity Route Float () Map IO where

  genRandom = genRandom'

  score' = score''

  mutation = tworsMutation

  crossover = selectiveCrossover

  -- showGeneration = showRoute


main :: IO()
main = do
        let cfg = GAConfig 
                    200 -- population size
                    25 -- archive size (best entities to keep track of)
                    100 -- maximum number of generations
                    0.8 -- crossover rate (% of entities by crossover)
                    0.2 -- mutation rate (% of entities by mutation)
                    0.0 -- parameter for crossover (not used here)
                    0.2 -- parameter for mutation (% of changed cities) NOT used for selectiveMutation
                    False -- whether or not to use checkpointing
                    False -- don't rescore archive in each generation

            g = mkStdGen 0 -- random generator
            pool = berlin52

        es <- evolveVerbose g cfg pool ()
        let e = snd $ head es :: Route
        let eLen = calcTotalLength (makeRoundTrip e)
        
        putStrLn $ "best entity (GA): " ++ show e
        putStrLn $ "length: " ++ show eLen


        -- Compare with random search with large budget
        -- 100k random entities, equivalent to 1000 generations of GA
        --es' <- randomSearch g 100000 pool ()
        --let e' = snd $ head es' :: Route
        --let eLen' = calcTotalLength (makeRoundTrip e')
       
        --putStrLn $ "best entity (random search): " ++ show e'
        --putStrLn $ "length: " ++ show eLen'

