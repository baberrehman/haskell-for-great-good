
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

quickSort :: (Ord a) => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = smallerSort ++ [x] ++ biggerSort
                   where smallerSort = quickSort (filter' (<x) xs)
                         biggerSort  = quickSort (filter' (>x) xs)

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
                   where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n  = n : chain (n*3 + 1)


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y xs = foldl (\acc x -> if x == y then True else acc) False xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs