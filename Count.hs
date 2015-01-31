data Op = Add | Sub | Mul | Div

valid :: Op->Int->Int->Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply  :: Op->Int->Int->Int
apply Add x y = x+y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr
values :: Expr -> [Int]
values (Val n) = [n]
