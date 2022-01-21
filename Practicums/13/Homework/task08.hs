import Data.List

main :: IO()
main = do
    print $ constructMaxBTree [3, 2, 1, 6, 0, 5] == t2

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq) 

t2 = Node 6 (Node 3 Nil (Node 2 Nil (Node 1 Nil Nil))) (Node 5 (Node 0 Nil Nil) Nil) 

constructMaxBTree :: [Int] -> BTree
constructMaxBTree nodes = helper nodes 
    where
        helper :: [Int] -> BTree
        helper [] = Nil
        helper xs = Node maxElement (helper leftXs) (helper rightXs)
            where 
                maxElement = maximum xs
                leftXs = takeWhile (\ x -> x /= maxEl) xs
                (m:rightXs) = dropWhile (\ x -> x /= maxEl) xs
