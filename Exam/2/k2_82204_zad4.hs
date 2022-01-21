main :: IO()
main = do
    print $ isPerfectlyBalanced t1 == True

data BTree a = Nil | Node a (BTree a) (BTree a)

t1 = Node 'H' (Node 'a' (Node 'k' Nil Nil) (Node 'e' Nil Nil)) (Node 's' (Node 'l' Nil Nil) (Node 'l' Nil Nil))

countNodes :: BTree a -> Int
countNodes Nil = 0
countNodes (Node _ left right) = 1 + countNodes left + countNodes right

treeHeight :: BTree a -> Int
treeHeight Nil = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

isPerfectlyBalanced :: BTree a -> Bool
isPerfectlyBalanced Nil = True
isPerfectlyBalanced (Node _ left right) = isSmallTreeBalanced left && isSmallTreeBalanced right
    where 
        isSmallTreeBalanced :: BTree a -> Bool
        isSmallTreeBalanced Nil = True
        isSmallTreeBalanced bt = countNodes bt == ((*2) $ treeHeight bt) - 1
