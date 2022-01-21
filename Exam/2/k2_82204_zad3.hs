import Data.List

main :: IO()
main = do
    print $ matching "1234" == []
    print $ matching ",[.[-],]" == [(3,5),(1,7)]
    print $ matching ",+[-.,+]" == [(2,7)]
    print $ matching "[][]" == [(0,1),(2,3)]

matching :: String -> [(Int, Int)]
matching str = zip (findIndices (`elem` "[") str) (findIndices (`elem` "]") str) 
