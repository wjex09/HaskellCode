applytwice :: (a -> a) -> a -> a
applytwice f x = f (f x) 


-- signature of zipop if function f(that takes a ,b and returns c)list a list b and return a list c 
zipop :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipop _ [] _ = []
zipop _ _ [] = [] 
zipop f (x:xs) (y:ys) = f x y : zipop f xs ys   

-- flip the parameters passed to the functions   
flipop :: (a->b->c) -> b -> a -> c
flipop f y x = f x y 

-- foldl and foldr   foldl starts from leftmost and foldr from rightmost roughly
-- fold works similar to map but returns a single value
-- when needed to fold on empty list use foldl1 and foldr1
-- Example 

getSum :: (Num a) => [a] -> a  
getSum = foldl (+) 0  

-- scanl and scanr report all the values in intermeadiate states
-- example usage implementing prefix and suffix states of anyfunction on the list
--How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?  

getans :: Int 
getans = length(takeWhile(<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function composition 
-- fog(x) = f.g x can have lots of functions composed in this way


