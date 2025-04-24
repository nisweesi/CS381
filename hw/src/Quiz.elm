module Quiz exposing (..)


rev: List Int -> List Int
rev l = case l of
    [] -> []
    x::xs -> rev xs ++ [x]

hi: comparable -> comparable -> comparable
hi s g  = if s > g then s else g

sign: Float -> Float
sign num = 
    if num < 0 then
        -1
    else if num == 0 then
        0
    else
        1

fac: Int -> Int
fac i =
    case i of
        0 -> 1
        1 -> 1
        n -> n * fac(n-1)

sum: List Int -> Int
sum s =
    case s of
        [] -> 0
        m::ms -> m + sum(ms)

isEven: Int -> Bool
isEven e =
    case e of
        0 -> True
        1 -> False
        ee -> isEven(ee-2)

fib: Int -> Int
fib f = 
    case f of
        0 -> 0
        1 -> 1
        ff -> fib(ff-1) + fib(ff-2)
