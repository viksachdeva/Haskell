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
values (App  _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n ) = [n|n>0]
eval (App o l r) = [ apply o x y | x <- eval l, y <- eval r, valid o x y ]

subs :: [a] -> [[a]]
subs[] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a ->[a]->[[a]]
interleave x[] = [[x]]
interleave x(y:ys) = (x:y:ys):map(y:)(interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat(map(interleave x)(perms xs))

choices :: [a] -> [[a]]
choices xs = concat(map perms(subs xs))

solution :: Expr->[Int]->Int->Bool
solution e ns n = elem (values e) (choices ns)  eval e == [n]

