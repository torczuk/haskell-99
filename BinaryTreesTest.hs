import Test.HUnit
import Trees

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f Branch n left right = Branch f n (fmap left) (fmap right)

--55. Construct completely balanced binary trees
size :: Tree -> Int
size Empty = 0
size Branch left right = 1 + (size left) + (size right)

insert :: Tree n -> n -> Tree n
insert Empty e = Branch 1 Empty Empty
insert Branch i left@(Branch _ _ _) right@Empty = Branch (i + 1) left (insert right e)
insert Branch i left@Empty right@(Branch _ _ _) = Branch (i + 1) (insert left e) right
insert Branch i left@(Branch m _ _) right@(Branch m _ _) e
              | m > n = Branch (i + 1) left (insert right e)
              | m <= n = Branch (i + 1) (insert left e) right

cbalTree :: Int -> Tree
cbalTree n = map (\t -> 'x') . foldr (\t n -> insert t n ) Empty $ [1 .. n]
