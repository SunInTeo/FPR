
main :: IO()
main = do
    print (rotate  5 ['a','b','c','d','e','f','g','h']) -- --> "fghabcde"
    print (rotate 8 ['a','b','c','d','e','f','g','h']) -- --> "abcdefgh"
    print (rotate 11 ['a','b','c','d','e','f','g','h']) -- --> "defghabc"
    print (rotate (-2) ['a','b','c','d','e','f','g','h'] ) -- --> "ghabcdef"

rotate :: Int -> [Char] -> [Char]
rotate _ [] = []
rotate n xs 
    | n > 0 && n <= (length xs) = (drop n xs) ++ take n xs 
    | n > (length xs) = rotate (mod n (length xs)) xs
    |otherwise =  drop ((length xs) -(abs n)) xs ++ take ((length xs) -(abs n)) xs