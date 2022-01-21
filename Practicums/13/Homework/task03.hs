main :: IO()
main = do
    print $ colourBTree
    print $ highest Red colourBTree == 4
    print $ highest Green colourBTree == 3
    print $ highest Blue colourBTree  == 4

data Colour = Red | Green | Blue
 deriving (Show, Eq)

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq) 

colourBTree = Node Blue (Node Green (Node Blue (Node Red Nil Nil) Nil) (Node Blue Nil Nil)) (Node Red (Node Green (Node Blue Nil Nil) Nil) (Node Red Nil Nil))

getLevel :: BTree Colour -> Int -> [Colour]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

highest :: Colour -> BTree Colour -> Int
highest colour ct = (last $ map (\ (level, _) -> level) $ filter (\ (_, nodes) -> elem colour nodes) myMap) + 1
    where
        myMap = takeWhile (\ (_, nodes) -> not $ null nodes) [(i, getLevel ct i) | i <- [0 ..]]
