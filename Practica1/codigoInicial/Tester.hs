module Tester where

import Interpreter
import Test.QuickCheck

test1 n m = eval (Plus (Num n) (Num m)) == Num (n+m)
test2 n m = eval (Mul (Num n) (Num m)) == Num (n*m)

-- Esto falla
test3F n m = eval (Plus (Num n) (Num m)) == Num (n*m)