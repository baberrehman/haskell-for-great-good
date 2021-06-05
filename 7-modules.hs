import Data.List

numUniques :: (Ord a) => [a] -> Int
numUniques = length . nub