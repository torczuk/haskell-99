module  Logic
(
  not',
  and',
  or',
  xor',
  impl',
  equ',
  gray,
  huffman,
) where

import Control.Monad (replicateM)
import Data.List (sortBy)
import Data.Ord (comparing)

not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

xor' :: Bool -> Bool -> Bool
xor' True True  = False
xor' False False = False
xor' _ _ = True

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ =  True

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

-- 46. Write a predicate table/3 which prints the truth table of a given logical expression in two variables.
table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ concatMap (++ "\n" ) [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]

-- 47. Truth tables for logical expressions (2).
infixl 4 `or'`
infixl 6 `and'`

-- 48. Truth tables for logical expressions (3).
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr bools ++ " = " ++ show (f bools) | bools <- args n]
  where
    args n = replicateM n [True, False]
    toStr = unwords . map (\x -> show x ++ " ")

-- 49. Gray codes
gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = map ("0" ++ ) prev ++ reverse (map ("1" ++ ) prev)
        where prev = gray (n - 1)

--50.  Huffman codes
data HTree = HNode {counter :: Int, left :: HTree, right :: HTree} | HLeaf {counter :: Int, letter :: Char} deriving Show

flatHTree :: [(Char, Int)] -> [HTree]
flatHTree = sortByCnt . map (\p -> HLeaf {letter = fst p, counter = snd p})

sortByCnt :: [HTree] -> [HTree]
sortByCnt = sortBy (comparing counter)

buildHT :: [HTree] -> HTree
buildHT [x] = x
buildHT (x:y:xs) = buildHT . sortByCnt $ ((add x y) : xs)

add :: HTree -> HTree -> HTree
add l@HLeaf {counter = n, letter = _} r@HLeaf {counter = m, letter = _} = HNode {counter = n + m, left = l, right = r}
add l@HLeaf {counter = n, letter = _} r@HNode {counter = m, left = _, right = _} = HNode {counter = n + m, left = l, right = r}
add l@HNode {counter = n, left = _, right = _} r@HLeaf {counter = m, letter = _} = HNode {counter = n + m, left = l, right = r}
add l@HNode {counter = n, left = _, right = _} r@HNode {counter = m, left = _, right = _} = HNode {counter = n + m, left = l, right = r}

all' :: HTree -> [(Char, String)]
all' HNode {counter = n, left = l, right = r} = (map (\p -> (fst p, "0" ++ snd p)) . all' $ l) ++ (map (\p -> (fst p, "1" ++ snd p)) . all' $ r)
all' HLeaf {counter = n, letter = c} = [(c, "")]

huffman :: [(Char, Int)] -> [(Char, String)]
huffman input = sortBy (comparing fst) $ all' . buildHT . flatHTree $ input
