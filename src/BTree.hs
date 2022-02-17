module BTree where

data BTree a = Empty
    | Node a (BTree a) (BTree a) deriving (Show)

btreeBuild :: (Ord a) => [a] -> (BTree a)
btreeBuild lst = foldl btreeInsert Empty lst

btreeInsert :: (Ord a) => (BTree a) -> a -> (BTree a)
btreeInsert Empty x = Node x Empty Empty
btreeInsert (Node v left right) x
    | x <= v = Node v (btreeInsert left x) right
    | x > v  = Node v left (btreeInsert right x)

btreeDelete :: (Ord a) => (BTree a) -> a -> (BTree a)
btreeDelete Empty _ = Empty
btreeDelete n@(Node v left right) x
    | x < v  = Node v (btreeDelete left x) right
    | x > v  = Node v left (btreeDelete right x)
    | x == v = case n of 
        (Node _ Empty Empty) -> Empty
        (Node _ left Empty)  -> left
        (Node _ Empty right) -> right
        (Node _ left right)  -> Node v' left right'
            where (right', v') = btreeDeleteSmallest right

btreeDeleteSmallest :: (Ord a) => (BTree a) -> ((BTree a), a)
btreeDeleteSmallest (Node v Empty Empty) = (Empty, v)
btreeDeleteSmallest (Node _ left _) = btreeDeleteSmallest left
        
btreeHeight :: (Ord a) => (BTree a) -> Integer
btreeHeight Empty = 0
btreeHeight (Node _ left right) =
    1 + max (btreeHeight left) (btreeHeight right)

