main ::IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

divisors n = [x | x <- [1..n], mod n x == 0]
sumOfDivisors n = sum $ divisors n

areAmicable:: Int -> Int -> Bool
areAmicable x y = (sumOfDivisors x) == (sumOfDivisors y)