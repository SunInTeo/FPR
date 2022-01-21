import Data.List

main :: IO()
main = do
    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False

data BTree a = Nil | Node a (BTree a) (BTree a)

t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))
t3 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))
t4 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

leavesToList :: BTree a -> [a]
leavesToList Nil = []
leavesToList (Node value Nil Nil) = value : []
leavesToList (Node value left right) = (leavesToList left) ++ (leavesToList right)

leavesAreEqual :: (Ord a) => BTree a -> BTree a -> Bool
leavesAreEqual b1 b2 = sort (leavesToList b1) == sort (leavesToList b2)
