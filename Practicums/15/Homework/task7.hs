

main :: IO()
main = do
    --print (height t1)
    print (deepestLeavesSum t1) -- 15
    print (deepestLeavesSum t2) -- 4
    print (deepestLeavesSum t3) -- 10
    
data BTree = Empty | Node Int BTree BTree 

t1 :: BTree 
t1 = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) 
                             Empty) 
                            (Node 5 Empty 
                                Empty)) 
            (Node 3 Empty (Node 6 Empty (Node 8 Empty Empty)))

t2 :: BTree 
t2 = Node 1 (Node 2 (Node 4 Empty Empty) 
                                Empty) 
            (Node 3 Empty Empty) 

t3 :: BTree
t3 = Node 1 (Node 2 (Node 4 (Node 10 Empty Empty) Empty) Empty) Empty

height :: BTree -> Int
height Empty = 0
height (Node v Empty Empty) = 1
height (Node v lt rt) = 1 + max (height lt) (height rt)

deepestLeavesSum :: BTree -> Int
deepestLeavesSum Empty = 0
deepestLeavesSum (Node v Empty Empty) = v
deepestLeavesSum bt@(Node v lt rt) 
    | (height lt == (height bt) - 1) && (height rt == (height bt) - 1) = deepestLeavesSum lt + deepestLeavesSum rt
    | (height lt == (height bt) - 1) = deepestLeavesSum lt
    | (height rt == (height bt) - 1) = deepestLeavesSum rt
    