main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

data BTree a = Nil | Node a (BTree a) (BTree a)

numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

getLevel :: BTree Int -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

levelSum :: BTree Int -> Int -> Int
levelSum t k =  sum $ getLevel t k

cone :: (Num a, Eq a) => BTree a -> Bool 
cone Nil = True
cone (Node value left right) = countOfNodes left == countOfNodes right
    where
        countOfNodes :: (Num a) => BTree a -> a 
        countOfNodes Nil =   0
        countOfNodes (Node value left right) = 1 + countOfNodes left + countOfNodes right
