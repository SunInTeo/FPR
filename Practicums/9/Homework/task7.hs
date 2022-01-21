main ::IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True


divisors n = [x | x <- [1..n], mod n x == 0]
sumOfDivisors n = sum $ divisors n

isPerfect:: Int -> Bool
isPerfect x = x == (sumOfDivisors x) - x