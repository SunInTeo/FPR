main :: IO()
main = do
    print $ addN [1, 2, 3, 4, 5] 9999999999999999999999 == [10000000000000000000000,10000000000000000000001,10000000000000000000002,10000000000000000000003,10000000000000000000004]
    print $ sqAddN [1, 2, 3, 4, 5] 5 == [36,49,64,81,100]
    print $ divByN [1, 2, 3, 4, 5] 5 == [0.2,0.4,0.6,0.8,1.0]
    print $ filterByN [1, 2, 3, 4, 5] 3 == [3,4,5]

addN :: [Int] -> Integer -> [Integer]
addN xs n =  map (+n) (map (\ d -> toInteger d) xs)

sqAddN :: [Int] -> Int -> [Int]
sqAddN xs n = map ((^2) . (+n)) xs

divByN :: [Int] -> Int -> [Double]
divByN xs n = map ( (/ fromIntegral n) . fromIntegral) xs

filterByN :: [Int] -> Int -> [Int]
filterByN xs n = filter (>= n) xs