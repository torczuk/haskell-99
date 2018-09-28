module Lists
( myLast,
  butLast,
  elementAt,
  myLength,
  myReverse,
  isPalindrome,
  compress,
  pack,
  encode,
  dupli,
  repli,
  Occurence(..),
  encodeModified,
  decodeModified,
  encodeDirect,
  dropEvery,
  split,
  slice,
  removeAt,
  rotate,
) where

-- 1
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

-- 11th Modified run-length encoding.
data Occurence a = Single a | Multiple a Int deriving (Show, Eq)
encodeModified :: (Eq a) => [a] -> [Occurence a]
encodeModified = map helper . encode . pack
          where
            helper (1, x) = Single x
            helper (n, x) = Multiple x n

-- 12th Decode a run-length encoded list.
decodeModified :: (Eq a) => [Occurence a] -> [a]
decodeModified =  foldl (++) [] . map helper
          where
            helper (Single x) = [x]
            helper (Multiple x n) = replicate n x

-- 14th Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x: x: dupli xs

--13th Run-length encoding of a list (direct solution). Without creating sublist
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = foldr helper []
        where
          helper e [] = [(1, e)]
          helper e (x@(n,c):xs) =
            if e == c then (n + 1, c):xs
            else (1, e):x:xs

encodeDirect :: (Eq a) => [a] -> [Occurence a]
encodeDirect xs = map helper (encode' xs)
          where
            helper (1, a) = Single a
            helper (n, a) = Multiple a n

-- 15th Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n
        | n < 1 = []
        | otherwise  = replicated ++ (repli xs n)
                        where replicated = map (\y -> x) [1..n]

-- 16th Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map snd . filter (\ (f, _) -> f `mod` n /= 0 ) $ zip [1..] xs

-- 17th Split a list into two ]parts; the length of the first part is given.
split :: [a] -> Int -> [[a]]
split xs n = [first, second]
      where
        indexed = zip [1..] xs
        first = map snd . filter (\ (i, e) -> i <= n) $ indexed
        second = map snd . filter (\ (i, e) -> i > n) $ indexed

-- 18th  Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs from to
  | from > to = error "Condition from <= to not fullfilled"
  | otherwise = (split sliced (from - 1)) !! 1
                  where sliced = (split xs to) !! 0

-- 19th Rotate a list N places to the left.

rotate :: [a] -> Int -> [a]
rotate xs n = (splited !! 1) ++ (splited !! 0)
    where
      size  = length xs
      index = if n < 0 then size + n else n
      splited = split xs index

-- 20th Drop every N'th element from a list.
removeAt :: Int -> [a] -> [a]
removeAt 1 (x:xs) = xs
removeAt n (x:xs)
  | n < 1 = error "Index Out of Bound"
  | otherwise = x: removeAt (n - 1) xs
