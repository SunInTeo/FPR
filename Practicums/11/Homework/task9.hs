main :: IO()
main = do
    print $ naturalProduct [-1, 0, -2, -3] 5 == 0 -- There are no natural numbers
    print $ naturalProduct [5, 10] 5 == 0 -- Sum of the divisors for 5 is 1 and for 10 is 1+2+5=8
    print $ naturalProduct [95, 75, 15, 55, 11, 14, 18, 35, 25] 5 == 1330


divisors n = [x | x <- [1..n - 1], mod n x == 0]
sumOfDivisors n = sum $ divisors n

multiples n = map (* n) [0..n^2]

find _ [] = False
find n (x:xs)
  | x == n = True
  | otherwise = find n xs

isNumberNaturalAndMultipleOfK :: Int -> Int -> Bool
isNumberNaturalAndMultipleOfK n k = n > 0 && find (sumOfDivisors n) (multiples k) 

naturalProduct :: [Int] -> Int -> Int
naturalProduct xs k 
 | null resList = 0
 | otherwise = product resList
 where resList = filter (\ d -> isNumberNaturalAndMultipleOfK d k) xs 