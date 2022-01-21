main :: IO()
main = do
    print $ primesInRange 1 100 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 1 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n = n > 1 && length [ x | x <- [2 .. isqrt n], mod n x == 0] == 0

primesInRange :: Int -> Int -> [Int]
primesInRange x y = [n | n <- [min x y .. max x y], isPrime n]