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
