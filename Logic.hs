module  Logic
(
  not',
  and',
  or',
  xor'
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
