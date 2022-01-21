main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving Eq

tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

treeToList :: BTree a -> [a]
treeToList Nil = []
treeToList (Node value left right) = value : (treeToList left) ++ (treeToList right)

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree Nil _ = Nil
mapTree (Node value left right) f = (Node (f value) (mapTree left f) (mapTree right f))

convert :: (Num a, Ord a) => BTree a -> BTree a
convert bt = mapTree bt (\ x -> x * 0 + (sum (filter (\y -> y >= x) (treeToList bt))))