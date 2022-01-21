main ::IO()
main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

isPrimeG:: Int -> Bool
isPrimeG x
 | x < 0 = error "x was negative"
 | x == 1 = False
 | otherwise = helper 2
 where
     helper:: Int -> Bool
     helper currentNum
      | currentNum >= x = True
      | mod x currentNum == 0 = False
      | otherwise = helper (currentNum + 1)

isPrimeLC:: Int -> Bool
isPrimeLC k = k > 1 && length [ x | x <- [2..k], mod k x == 0] == 1