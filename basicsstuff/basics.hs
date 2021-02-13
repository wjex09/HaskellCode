--Readings from Learnyouahaskell and Real World Haskell 

-- don't name variables with captial letter

doubleMe a = a + a
triplesum x y = 3*x + 3*y
fn a b = triplesum a b + doubleMe a + doubleMe b
fnf a b = 5*a + 5*b

-- if statement always returns ^<>^

chksmol x = if x > 10 then x else 2*x -- returns twice if smol 
protip = "Drink Water" 

-- let's try list operations

keepupcase :: [Char] -> [Char]
keepupcase st = [c | c<-st, elem c ['A'..'Z']]

-- type and return type 

add3 :: Int -> Int -> Int -> Int 
add3 a b c = a + b + c

-- :t provide return type of function 
-- factorial 

fact :: Integer -> Integer 
fact n = product[1..n]

-- double more precision than float ae
area :: Double -> Double 
area r = pi * r *r 

-- show and read do as their names take exper or return exper
-- for read specify return type
-- read  "5.0" :: Double


-- Syntax in Functions 
-- Pattern matching is done top to bottom 

lucky :: (Integral a) => a -> String
lucky 9 = "GG"
lucky x = "kekw"

--recursive factorial 

factorial :: (Integral a ) => a -> a
factorial 0 = 1 
factorial n = n * factorial(n-1)

charex :: Char -> String 
charex 'w' = "wjex loves kdot"
charex 'k' = "kekw"

addvec :: (Num a) => (a,a) -> (a,a) -> (a,a)
--addvec a b = (fst a + fst b , snd a + snd b)
addvec (a1,b1) (a2,b2) = (a1+b1,a2+b2)

first :: (a,b,c) -> a  
first (x,_,_) = x 
second :: (a,b,c) -> b
second (_,y,_) = y 
third :: (a,b,c) -> c 
third (_,_,z) = z 

addtrip ::(Num a) => (a,a,a) -> (a,a,a) ->(a,a,a)
addtrip p q  = (first p + first q , second p + second q ,third p + third q) 
--addtrip (p,q,r) (x,y,z) =(p+x,q+y,z+r)

-- x:xs bind head of list x to xs 

-- implement head 
hed :: [a] -> a 
hed [] = error "can't call head in empty list"
hed (x:_) = x

-- tell function 

tell :: (Show a) => [a] -> String 
tell [] = "The empty list"
tell (x:[]) = "The list has one element:" ++ show x
tell (x:y:[]) = "The list has two elements:" ++show x ++ "and " ++ show y
tell (x:y:_) = "This is a long list "

-- length function recursive  
len :: (Num b) => [a] -> b 
len [] = 0 
len (_:xs) = 1 + len xs

-- recursive sum and product 
  
agg :: (Num a) => [a] -> a 
agg [] = 0
agg (x:xs) = x + agg xs

pro :: (Num a) => [a] -> a 
pro [] = error "error" 
pro (x:[]) = x    
pro (x:xs) = x * pro xs 

pow2 = iterate(2*) 1 
-- take 15 pow2 

-- Guards similar to if but works nicely with patterns  

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normie"
    | bmi <=30 = "Welp" 
    | otherwise  = "KEKW"

absolute :: Int -> Int
absolute n  
    | n < 0    = -n 
    | otherwise = n 

mycomp :: (Ord a) => a ->a -> Ordering 
mycomp a b
    | a>b = GT
    | a==b = EQ
    | otherwise = LT 

-- bmi example usage of where nice also align 

bmi :: (RealFloat a) => a -> a -> String 
bmi w h 
    | bmi <= skinny = "pffff"
    | bmi <= normal = "normal"
    | bmi <= fat = "dang"
    | otherwise = "kekw"
    where bmi = w/h^2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
          
-- let bindings 

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

-- case expr of pattern -> result

showlist :: [a] -> String
showlist xs = "The list is " ++ case xs of [] -> "empty"
                                           [x] -> "singleton"
                                           xs -> "many elements"

-- alt 
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
        where what [] = "empty."  
              what [x] = "a singleton list."  
              what xs = "a longer list." 

-- Some more recursion 

-- max and min in list

mx :: (Ord a) => [a] -> a 
mx [] = error "empty"
mx [x] = x 
mx (x:xs)
    | x > maxTail = x 
    | otherwise = maxTail
    where maxTail = mx xs 


mi :: (Ord a) => [a] -> a 
mi [] = error "empty"
mi [x] = x
mi (x:xs) = min x (mi xs)


rep :: (Num i , Ord i) => i -> a -> [a]
rep n x 
    | n<=0 = []
    | otherwise = x : rep (n-1) x

-- infinite add tail 
repn :: a -> [a] 
repn x = x : repn x

zipr :: [a] -> [b] -> [(a,b)]
zipr ye [] = [] 
zipr [] ye = [] 
zipr (x:xs) (y:ys) = (x,y) : zipr xs ys --match heads and then recurse on tails  


-- copypasted
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs  

chkodd n = mod n 2 ==1
lastButOne x = last (take ((length x) - 1) x)


