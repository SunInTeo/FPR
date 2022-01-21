import Data.List
import Data.Maybe

main :: IO()
main = do
    print $ toBinaryIndexed tree == Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving Eq

tree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree Nil _ = Nil
mapTree (Node value left right) f = (Node (f value) (mapTree left f) (mapTree right f))

toBinaryIndexed :: (Num a, Eq a) => BTree a -> BTree (a, Int)
toBinaryIndexed bt = mapTree bt (\ x -> (x, fromJust(elemIndex x (traverseDFS bt))))