module ZTree where

-- zipper tree

data ZTree a = Empty
    | Node (ZTree a) a [ZTree a] deriving (Show)

parent :: (Ord a) => ZTree a -> ZTree a
parent Empty = Empty
parent (Node p _ _) = p

insert :: (Ord a) => ZTree a -> a -> Int -> ZTree a
insert Empty w i = Node Empty w []
insert n@(Node p v c) w i = Node p v (take i c ++ newNode : drop i c)
    where newNode = Node n w []
    
