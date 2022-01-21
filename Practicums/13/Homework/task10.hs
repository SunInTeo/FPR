import Data.List

main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq) 

t1 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (4,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))
t2 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (7,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree Nil _ = Nil
mapTree (Node value left right) f = (Node (f value) (mapTree left f) (mapTree right f))

transform :: BTree (Int,Int) -> BTree Int
transform tree = mapTree tree (\(x, y) -> y - x)

traverseDFS :: BTree Int -> [Int]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

isBST :: BTree Int -> Bool
isBST tree = (traverseDFS tree) == sort(traverseDFS tree)

ordered :: BTree (Int,Int) -> Bool
ordered tree = isBST(transform tree) 


