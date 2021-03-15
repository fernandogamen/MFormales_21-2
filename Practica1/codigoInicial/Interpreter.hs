module Interpreter where

type Var = String

data Exp = Num Int |
           Boole Bool |
           Plus Exp Exp | 
           Mul Exp Exp |
           Gt Exp Exp |
           If Exp Exp deriving(Show,Eq)

isValue :: Exp -> Bool
isValue (Num _) = True
isValue (Boole _) = True
isValue _ = False

eval :: Exp -> Exp
eval (Num n) = Num n
eval (Boole b) = Boole b
eval (Plus e1 e2) = let e1' = eval e1
                        e2' = eval e2 in
                        case (e1',e2') of 
                            (Num n,Num m) -> Num (n+m)
eval (Mul e1 e2) = let e1' = eval e1
                       e2' = eval e2 in
                       case (e1',e2') of 
                        (Num n,Num m) -> Num (n*m)
eval (Gt e1 e2) = let e1' = eval e1
                      e2' = eval e2 in
                      case (e1',e2') of 
                        (Num n,Num m) -> Boole (n > m)