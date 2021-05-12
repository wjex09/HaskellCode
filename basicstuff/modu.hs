import Data.List 
numuniq :: (Eq a) => [a] -> Int
numuniq = length . nub  
-- nub removes the dupblicates in the list  unique 
-- import Data.List (nub ,sort) imports only selected functions 
-- import Data.List hiding (nub) imports all except the ones mentioned 
-- intercalate take a list and a list of list and inserts the list in those lists and returns
-- a single list

-- intersperse takes single element and inserts into the list and returns a single list
-- transpose transposes a list of list 
-- concat concatinates the single list elements and returns the single list
-- concatMap maps a function to the list and then concatinates 
-- and & or takes the boolean of the entire list and return a single boolean value
-- any and all  take a predicate and check if any element in the list satisfies the perdicate can be used instead of mapping and & or to list 
-- 
-- takeWhile and dropWhile
-- span and break
--  
--  lines -> takes multiple lines and returns list as each line
--  unlines -> takes a list of strings and returns in line seperated 
--  similarly words and unwords
-- // is like set difference 
--  union intersect
-- insert simliar to set insert

-- Data.Map
--


module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  
  
sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  
  
cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  
  
cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  


