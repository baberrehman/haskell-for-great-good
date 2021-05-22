
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number 7!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' []    = error "Can't call head on empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell []       = "The list is empty"
tell (x:[])   = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_)  = "The list is long. First two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' []     = 0
length' (_:xs) = 1 + (length' xs)

sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + (sum' xs)

capital :: String -> String
capital "" = "Empty String!"
capital all@(x:_) = "First letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 25.0 = "You're normal"
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 30.0 = "You're fat"
    | otherwise   = "You're a whale"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 25.0 = "You're normal"
    | weight / height ^ 2 <= 18.5 = "You're underweight"
    | weight / height ^ 2 <= 30.0 = "You're fat"
    | otherwise   = "You're a whale"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b      = GT
    | a == b     = EQ
    | otherwise  = LT

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi <= 25.0 = "You're normal"
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 30.0 = "You're fat"
    | otherwise   = "You're a whale"
    where bmi = weight / height ^ 2

bmiTell''' :: (RealFloat a) => a -> a -> String
bmiTell''' weight height
    | bmi <= skinny = "You're underweight"
    | bmi <= normal = "You're normal"
    | bmi <= fat = "You're fat"
    | otherwise   = "You're a whale"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what []   = "empty"
          what [x]  = "a singleton list"
          what xs = "a longer list"