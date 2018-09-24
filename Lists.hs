module Lists
( myLast,
  butLast,
  elementAt,
  myLength,
  myReverse,
  isPalindrome,
  compress,
  pack,
  encode
) where

myLast :: [a] -> a
myLast []     = error "List can not be empty"
myLast [h]    = h
myLast (h:xs) = myLast xs

butLast :: [a] -> a
butLast []      = error "List can not be empty"
butLast [x]     = error "List must contain at least two element"
butLast [x, _]  = x
butLast (x: xs) = butLast xs

elementAt :: (Integral b) => [a] -> b -> a
elementAt [] _      = error "List can not be empty"
elementAt (x: _) 1  = x
elementAt (_: xs) n
  | n < 1       = error "Index out of bound"
  | otherwise   = elementAt xs (n - 1)

myLength :: [a] -> Int
myLength []      = 0
myLength (_:xs)  = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse []      = []
myReverse (x:xs)  = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (myReverse xs)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

compress :: (Eq a) => [a] -> [a]
compress (x:xs@(y:_))
    | x == y    = compress xs
    | otherwise = x : compress xs
compress ys = ys -- this will be matched only for [] and [x] ;)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x: takeWhile (== x) xs) : pack (dropWhile (== x) xs)

encode :: [[b]] -> [(Int, b)]
encode xs = map (\x -> (length x, head x)) xs
