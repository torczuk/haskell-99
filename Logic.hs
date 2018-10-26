module  Logic
(
  not',
  and',
  or',
  xor',
  impl',
  equ',
) where

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
