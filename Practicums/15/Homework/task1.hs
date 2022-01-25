import Data.Char

main :: IO()
main = do
    print $ validate 1714 == False
    print $ validate 12345 == False
    print $ validate 891 == False
    print $ validate 123 == False
    print $ validate 2121 == True
    print $ validate 4736778291034 == True
    print $ validate 4485756008412 == True
    print $ validate 4214154976719 == True

val :: Int -> Int
val n = helper n 0
 where
     getValue :: Int -> Int
     getValue a
      | a * 2 >= 10 = mod (a * 2) 10 + div (a * 2) 10
      | otherwise = a * 2
     helper :: Int -> Int -> Int
     helper n result
      | n < 10 = result * 10 + n
      | n < 100 = (result * 10 + (mod n 10)) * 10 + getValue (mod (div n 10) 10)
      | otherwise = helper (div n 100) ((result * 10 + (mod n 10)) * 10 + getValue (mod (div n 10) 10))

addDigits :: Int -> Int
addDigits n = sum $ map digitToInt $ show n

validate :: Int -> Bool
validate number = mod (addDigits $ val number) 10 == 0