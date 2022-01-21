main :: IO()
main = do
    print $ isGracefull t1 == True -- t1 = A
    print $ isGracefull t2 == True -- t2 = B
    print $ isGracefull t3 == False -- t3 = C

data NTree = Nil | Node Int [NTree]
 deriving (Show)

t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]
t2 = Node 7 [Node 3 [Node 9 [Node 5 [Nil], Node 1 [Nil]]]]
t3 = Node 1 [Node 3 [Nil], Node 5 [Node 42 [Nil]], Node 7 [Nil], Node 9 [Nil]]

isGracefull :: NTree -> Bool
isGracefull Nil = True
isGracefull (Node value children) = all isDifEven children && all isGracefull children
    where
        isDifEven :: NTree -> Bool
        isDifEven Nil = True
        isDifEven (Node v _) = even $ abs (v - value)