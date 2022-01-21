import Data.List

main :: IO()
main = do
    print $ willItFly [1, 4, 2, 3] == True -- |1-4|=3,|4-2|=2,|2-3|=1
    print $ willItFly [1, 4, 2, -1, 6] == False

makeAListOfSubtractedElements ::  [Int] -> [Int]
makeAListOfSubtractedElements xs  = tail $ snd $ mapAccumL (\a b -> (b, abs (b - a))) 0 xs

willItFly :: [Int] -> Bool
willItFly xs = all (\x -> elem x [1..length xs - 1]) (makeAListOfSubtractedElements xs)

