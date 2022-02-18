module ZTree where

-- zipper tree

type ZTree a = (Tree a, [Crumb a])

data Tree a = Empty
    | Node a (Tree a) (Tree a) deriving (Show)

data Crumb a = LeftCrumb a (Tree a)
    | RightCrumb a (Tree a) deriving (Show)

getValue :: (Ord a) => ZTree a -> Maybe a
getValue (Empty, _)      = Nothing
getValue (Node v _ _, _) = Just v

-- TODO error handling when invalid move??

goLeft :: (Ord a) => ZTree a -> ZTree a
goLeft z@(Node _ Empty _, crumbs) = z
goLeft (Node v l r, crumbs)       = (l, LeftCrumb v r : crumbs)

goRight :: (Ord a) => ZTree a -> ZTree a
goRight z@(Node _ _ Empty, crumbs) = z
goRight (Node v l r, crumbs)       = (r, RightCrumb v l : crumbs)

goUp :: (Ord a) => ZTree a -> ZTree a
goUp z@(_, [])                  = z
goUp (n, (LeftCrumb v r):rest)  = (Node v n r, rest)
goUp (n, (RightCrumb v l):rest) = (Node v l n, rest)

starterZTree = (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 Empty Empty), [])

-- starterZTree = (Empty, [])
