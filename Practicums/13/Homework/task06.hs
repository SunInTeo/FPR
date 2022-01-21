import Data.List

main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [3, 4, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]

getAllNodes :: [(Int, Int, Int)] -> [Int]
getAllNodes = nub . foldl (\ acc (x, y, z) -> acc ++ [x,y,z]) [] 

getAllParents :: [(Int, Int, Int)] -> [Int]
getAllParents = nub . foldl (\ acc (x, y, z) -> acc ++ [x]) []

listLeaves :: [(Int, Int, Int)] -> [Int]
listLeaves nodes = getAllNodes nodes \\ getAllParents nodes
