main :: IO()
main = do
    print $ isPresent 0 [0, -1, 2] == True
    print $ isPresent 1 [0, 1, 2] == True
    print $ isPresent 2 [0, 1, -2] == False
    print $ isPresent 3 [0, 1, 2] == False

isPresent :: Int -> [Int] -> Bool
-- isPresent x xs = elem x xs
isPresent _ [] = False
isPresent n (x:xs) = x == n || isPresent n xs