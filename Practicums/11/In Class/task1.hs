main :: IO()
main = do
    print $ isNull [] == True
    print $ isNull [1 .. 5] == False
    print $ isNull ['a', 'b'] == False
    print $ isNull [1.75 .. ] == False

    print $ firstEl [1] == 1
    print $ firstEl [10, 5, 6] == 10
    print $ firstEl ["I am", "You are"] == "I am"

    print $ removeFirst [5, 2, 3] == [2, 3]
    print $ removeFirst ["OOpps"] == []

    print $ fromXs 5 [1, 2, 3, 4, 5, 6] == [6]
    print $ fromXs 5 ["A", "B", "C"] == []

    print $ concatenate [1, 2, 3] [5, 8, 9] == [1, 2, 3, 5, 8, 9]
    print $ concatenate [1] [5, 8, 9] == [1, 5, 8, 9]
    print $ concatenate ['1', '2', '3'] ['5'] == "1235"

    print $ join [[1, 2], [5, 6], [9]] == [1, 2, 5, 6, 9]
    print $ join [['H'], ['a', 's'], "kell"] == "Haskell"

    print $ joinFold [[1, 2], [5, 6], [9]] == [1, 2, 5, 6, 9]
    print $ joinFold [['H'], ['a', 's'], "kell"] == "Haskell"

joinFold :: [[a]] -> [a]
joinFold = foldl1 (++)

join :: [[a]] -> [a]
join [] = [] -- for Strings: ""
join (xs:xss) = xs ++ join xss

concatenate :: [a] -> [a] -> [a]
concatenate [] ys = ys
concatenate (x:xs) ys = x : concatenate xs ys

fromXs :: Int -> [a] -> [a]
fromXs _ [] = []
fromXs 0 xs = xs
fromXs n (_:xs) = fromXs (n - 1) xs

removeFirst :: [a] -> [a]
removeFirst [] = error "Empty list"
removeFirst (_:xs) = xs

firstEl :: [a] -> a
firstEl [] = error "Empty list"
firstEl (x:_) = x

isNull :: [a] -> Bool
isNull [] = True
isNull _ = False