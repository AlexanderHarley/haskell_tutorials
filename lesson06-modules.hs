import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- i.e. numUniques = \xs -> length (nub xs)
