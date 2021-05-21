
removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase xs = [x | x <- xs, x `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y +z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference r = 2 * pi * r