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

height :: (Ord a) => ZTree a -> Integer
height (n, _) = height' n

height' :: (Ord a) => Tree a -> Integer
height' Empty        = 0
height' (Node _ l r) = 1 + max (height' l) (height' r)

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

goRoot :: (Ord a) => ZTree a -> ZTree a
goRoot z@(_, []) = z
goRoot z = goRoot $ goUp z

createLeft :: (Ord a) => ZTree a -> a -> ZTree a
createLeft (Node v Empty r, crumbs) w = (Node v l' r, crumbs)
    where l' = Node w Empty Empty
createLeft z _ = z

createRight :: (Ord a) => ZTree a -> a -> ZTree a
createRight (Node v l Empty, crumbs) w = (Node v l r', crumbs)
    where r' = Node w Empty Empty
createRight z _ = z

truncateLeft :: (Ord a) => ZTree a -> ZTree a
truncateLeft z@(Node _ Empty _, _) = z
truncateLeft (Node v _ r, crumbs) = (Node v Empty r, crumbs)

truncateRight :: (Ord a) => ZTree a -> ZTree a
truncateRight z@(Node _ _ Empty, _) = z
truncateRight (Node v l _, crumbs) = (Node v l Empty, crumbs)

toString :: (Show a, Ord a) => ZTree a -> String
toString (n, _) = foldl (\a b -> a ++ "\n" ++ b) "" (toString' n)

toString' :: (Show a, Ord a) => Tree a -> [String]
toString' Empty        = [" "]
toString' (Node v l r) = (pad ++ show v ) : mergeStringList leftStr rightStr
    where
        leftStr  = toString' l
        rightStr = toString' r
        pad      = replicate (length leftStr) ' '
        

mergeStringList :: [String] -> [String] -> [String]
mergeStringList [] []         = []
mergeStringList a []          = a
mergeStringList [] b          = b
mergeStringList (a:at) (b:bt) = (a ++ b) : mergeStringList at bt

starterZTree = (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 5 Empty Empty), [])

-- starterZTree = (Empty, [])
