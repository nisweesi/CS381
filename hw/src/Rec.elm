module Rec exposing (..)

fac: Int -> Int
fac x = case x of
    1 -> 1
    n -> n * fac(n-1)


-- type T = Con | Bin S T

-- f: T -> U

-- f x = case x of
--     Con -> e1
--     Bin s t -> e2{s, f t}


length : List a -> Int
length l = case 1 of
        [] -> 0
        x:xs -> 1 + length xs