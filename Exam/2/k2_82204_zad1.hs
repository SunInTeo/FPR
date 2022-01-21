import Data.List

main :: IO()
main = do
    print $ squareDigits 9119 == 811181
    print $ squareDigits (-9119) == -811181

intToList :: Int -> [Int]
intToList n
 | n == 0 = []
 | otherwise = intToList(div n 10) ++ [mod n 10]

listToInt :: [Int] -> Int
listToInt xs = sum (zipWith (*) (reverse xs) (iterate (*10) 1))

squareDigits :: Int -> Int
squareDigits n 
 | n < 0 = listToInt(map (^2) (intToList (n*(-1)))) * (-1)
 | otherwise = listToInt(map (^2) (intToList n))