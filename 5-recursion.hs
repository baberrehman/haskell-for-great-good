
maximum' :: (Ord a) => [a] -> a
maximum' []     = error "maximum of empty list"
maximum' [x]    = x
maximum' (x:xs) = if x > (maximum' xs) then x else (maximum' xs)

maximum'' :: (Ord a) => [a] -> a
maximum'' []     = error "maximum of empty list"
maximum'' [x]    = x
maximum'' (x:xs)
    | x > maxTail = x
    | otherwise   = maxTail
    where maxTail = maximum'' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:(replicate' (n-1) x)

replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' n x
    | n <= 0    = []
    | otherwise = x:replicate'' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' t []     = False
elem' t (x:xs)
    | t == x    = True
    | otherwise = elem' t xs

quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
                       where smallerSorted = [y | y <- xs, y <= x]
                             biggerSorted  = [y | y <- xs, y > x]

quickSort' :: (Ord a) => [a] -> [a]
quickSort' []     = []
quickSort' (x:xs) = 
    let smallerSorted = [y | y <- xs, y <= x]
        biggerSorted  = [y | y <- xs, y > x]
    in smallerSorted ++ [x] ++ biggerSorted                        