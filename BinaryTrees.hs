module BinaryTrees
(
  -- cbalTree,
  insert,
  Tree(Empty, Branch),
  symetric,
  construct,
  symetric,
  testSymetric,
  symCbalTrees,
  countLeaves,
)
where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Branch n left right) = Branch (f n) (fmap f left) (fmap f right)

--55. Construct completely balanced binary trees
insert :: (Integral a) => Tree a -> a -> Tree a
insert Empty e = Branch 1 Empty Empty
insert (Branch i left@Empty right@Empty) e = Branch (i + 1) (insert left e) right
insert (Branch i left@(Branch _ _ _) right@Empty) e = Branch (i + 1) left (insert right e)
insert (Branch i left@Empty right@(Branch _ _ _)) e = Branch (i + 1) (insert left e) right
insert (Branch i left@(Branch m _ _) right@(Branch n _ _)) e
              | m > n = Branch (i + 1) left (insert right e)
              | m <= n = Branch (i + 1) (insert left e) right

cbalTree :: Integral a => a -> Tree Char
cbalTree n = fmap (\t -> 'x') . foldr (\e t -> insert t e) Empty $ [1 .. n]

--56. Symmetric binary trees
symetric :: (Eq n) => Tree n -> Bool
symetric Empty = True
symetric (Branch n left right) = (mirror left) == right
      where
          mirror Empty = Empty
          mirror (Branch m l r) = Branch m (mirror r) (mirror l)

--57.  Binary search trees
add :: (Ord n) => Tree n -> n ->  Tree n
add Empty n = Branch n Empty Empty
add (Branch m left right) n
          | m == n = Branch m left right
          | n < m  = Branch m (add left n) right
          | n > m  = Branch m left (add right n)

construct :: (Ord n) => [n] -> Tree n
construct = foldl (\t n -> add t n) Empty

testSymetric :: (Ord n) => [n] -> Bool
testSymetric = symetric . fmap (\x -> 'x') . construct

--58. All symetric balance tree
symCbalTrees :: Integral a => a -> [Tree Char]
symCbalTrees = filter symetric . cbalTree'

cbalTree' :: Integral a => a -> [Tree Char]
cbalTree' 0 = [Empty]
cbalTree' 1 = [Branch 'x' Empty Empty]
cbalTree' n = if n `mod` 2 == 1 then
             [ Branch 'x' l r | l <- cbalTree' ((n - 1) `div` 2),
                                r <- cbalTree' ((n - 1) `div` 2) ]
             else
             concat [ [Branch 'x' l r, Branch 'x' r l] | l <- cbalTree' ((n - 1) `div` 2),
                                                         r <- cbalTree' (n `div` 2) ]

--  61A Count the leaves of a binary tree
countLeaves :: Tree n -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r
