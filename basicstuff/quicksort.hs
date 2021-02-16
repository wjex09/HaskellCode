-- quicksort 
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smol = quicksort [a|a <- xs , a <= x]
        largie = quicksort [a|a <- xs , a>x]
    in smol ++ [x] ++ largie 

