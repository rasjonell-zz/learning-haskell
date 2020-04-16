compose f g x = f (g x)

always7' = const 7

pass x f = f x
pass3 = pass 3
