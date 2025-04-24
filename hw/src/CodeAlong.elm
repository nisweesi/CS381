module CodeAlong exposing (..)
-- import String exposing (fromInt)

-- -- -- replace: a -> List a -> List a
-- -- -- replace x l =
-- -- --     case l of
-- -- --         [] ->
-- -- --             []
-- -- --         y::ys -> x::ys

-- -- -- threshold : Int -> List Int -> List Int
-- -- -- threshold n l =
-- -- --     case l of
-- -- --         [] ->
-- -- --             []
-- -- --         x::xs ->
-- -- --             if x > n then
-- -- --                 x::threshold n xs
-- -- --             else
-- -- --                 threshold n xs


-- -- type Nat = Zero | Suc Nat

-- -- add : Nat -> Nat -> Nat
-- -- add n m =
-- --     case n of
-- --         Zero ->
-- --             m
-- --         Suc k ->
-- --             add k (Suc m)

-- -- one = Suc Zero
-- -- two = Suc one

-- -- type Tree = Node Int Tree Tree | Leaf

-- -- lft = Node 3 (Node 1 Leaf Leaf)
-- --              (Node 5 Leaf Leaf)

-- -- t = Node 6 lft (Node 9 Leaf Leaf)

-- -- inorder : Tree -> List Int
-- -- inorder tr =
-- --     case tr of
-- --         Leaf ->
-- --             []
-- --         Node k l r -> inorder l ++ [k] ++ inorder r

-- -- find : Int -> Tree -> Bool
-- -- find f tre =
-- --     case t of
-- --         Leaf ->
-- --             False
-- --         Node k l r -> f == k || find f l || find f r
                

-- -- head : List a -> Maybe a
-- -- head l =
-- --     case l of
-- --         [] ->
-- --             Nothing
-- --         x::xs -> Just x

-- -- hello : String -> String

-- -- hello who = "Hello " ++ who

-- -- n : Float
-- -- n = 12.1

-- -- s : String
-- -- s =  "hi"

-- -- hey
-- {-  multiple line comment
-- hi
-- ad
-- -}

-- -- why : Int -> Int
-- -- why y = 2*y


-- -- rev : List Int -> List Int
-- -- rev l = case l of
-- --     [] -> []
-- --     x::xs -> rev xs ++ [x]


-- -- mmin : Int -> Int -> Int
-- -- mmin x y  = if x < y
-- --             then x
-- --             else y


-- -- db = \x -> 2^x

-- -- mult : Int -> (Int -> Int)
--     -- mult \x y -> x^y
--     -- or
--     -- mult x = \y -> x^y

-- -- mult x y = x^y


-- -- length : List a -> Int
-- -- length l = case l of
-- --         [] -> 0
-- --         x::xs -> 1 + length xs

-- {-
--     length 1 = if isEmpty 1 then 0 else 1 + length
-- -}

-- -- isEmpty: List a -> Bool
-- -- isEmpty l = case l of
-- --     [] -> True
-- --     -- or _
-- --     _::_ -> False


-- -- sum: List a -> Int
-- -- sum ss = case ss of
-- --     [] -> 0
-- --     x::xs -> x + x
-- --             sum ss

-- -- isMember : Int -> List Int -> Bool
-- -- isMember n l = case l of
-- --             [] -> False
-- --             x::xs -> if n == x then
-- --                         True
-- --                     else
-- --                        isMember n xs
--                 -- x::xs -> n == s || member n xs

-- -- index: Int -> List a -> Maybe a
-- -- index n l = case n of
-- --         0 -> case l of
-- --             [] -> Nothing
-- --             x::_ -> Just x
        
-- --         k -> case l of
-- --             [] -> Nothing
-- --             _::xs ->  index (k-1) xs


-- -- g l1 l2 = case l1 of
-- --     [] -> l2
-- --     x::xs -> g xs (x::l2)

-- -- sum : List Int -> Int
-- -- sum l = case l of
-- --         [] -> 1
-- --         x::xs -> x * sum xs

-- -- type Maybe a = Just a | Nothing

-- -- map : (... -> ...) -> List ... -> List ...

-- -- isEven : Int -> Bool
-- -- isEven n =
-- --     case n of
-- --         0 ->
-- --             True
-- --         1 ->
-- --             False
-- --         k -> isEven (k-2)


-- -- map : ( a -> b) -> List a -> List b
-- -- map f l =
-- --     case l of
-- --         [] ->
-- --             []
-- --         x::xs -> f x::map f xs


-- -- comp : (b -> c) -> (a -> b) -> (a -> c)
-- -- comp f g = \x -> f (g x)

-- -- succ = (+) 1
-- -- dbl = (*) 2

-- -- andThen : Maybe a -> (a -> b) -> Maybe b
-- -- andThen m f =
-- --     case m of
-- --         Nothing ->
-- --             Nothing
-- --         Just x ->
-- --             Just (f x)


-- -- count : List Int -> Int
-- -- count l =
-- --     case l of
-- --         [] ->
-- --             0
-- --         x::xs -> 1 + count xs

-- -- type Term = Tru
-- --             | Fls
-- --             | Not Term
-- --             | If Term Term Term
-- --             | Var String

-- -- myTerm : String -> Term
-- -- myTerm a = 
-- --     If (Not (Var a)) (Var "y") (Var "z")


--     -- num   ::=  (numbers, represented by Elm type Int)
--     -- var   ::=  (names, represented by Elm type String)
--     -- exp   ::=  num  |  var  |  exp * exp
--     -- stmt  ::=  var := exp
--     --        |   stmt ; stmt
--     --        |   FOR var := exp TO exp DO stmt END
--     --  PROC var (var*) {stmt}
--     -- CALL car (exp*)

-- -- type alias Num   = Int
-- -- type alias Var   = String
type Exp = Num  Int
            | Var String
            | Times Exp Exp

type Stmt =  Assign String Exp
            | Seq Stmt Stmt
            | For String Exp Exp Stmt
            | Proc String (List String) Stmt
            | Call String (List Exp)

-- -- switch ::= on | off | flip switch
type Switch = On 
            | Off 
            | Flip Switch

eval : Switch -> Bool
eval s = case s of
        On ->
            True
        Off ->
            False
        Flip ss ->
            not (eval ss)

-- -- PROC dbl (x result )(result := 2*x); CALL dbl (2 x)
-- dbl : Stmt
-- dbl = Proc "dbl" ["x" , "result"]
--         (Assign "result" (Times (Num 2) x))

-- dbl2 : Stmt
-- dbl2 = Seq dbl (Call "dbl" [Num 3, x])


-- x: Exp
-- x = Var "x"
-- y : Exp
-- y = Var "y"
-- --   x := 1; FOR y := x+1 TO 7 DO x := x*y END
-- a : Stmt
-- a = Assign "x" (Num 1)
-- b : Stmt
-- b = Assign "x" (Times x y)

-- -- b = Assign "x" (Times (Var "x") (Var "y") ) 
-- for : Stmt
-- for = For "y" (Times (Num 2) x) (Num 7) b

-- prog : Stmt
-- prog = Seq a for


-- ppE : Exp -> String
-- ppE o =
--     case o of
--         Num n ->
--             fromInt n
--         Var s ->
--             s
--         Times e1 e2 ->
--             ppE e1 ++ "*" ++ ppE e2

-- ppS : Stmt -> String
-- ppS s =
--     case s of
--         Assign v e ->
--             v ++ " := " ++ ppE e
--         _ ->
--             "undef"

-- -- fac := 1; FOR x := 2 10 n fac := fac c

-- genFac : Int -> Stmt
-- genFac n = 
--         Seq (Assign "fac" (Num 1))
--             (For "x" (Num 2) (Num n)
--                 (Assign "fac" (Times (Var "fac")
--                 (Var "x"))))


-- type Expr
--     = Num1 Int
--     | Plus Expr Expr

-- optimize : Expr -> Expr
-- optimize e = case e of
--     Plus (Num1 0) e2 ->
--         optimize e2
--     Plus e1 (Num1 0) ->
--         optimize e1
--     Plus (Num1 n1) (Num1 n2) ->
--         Num1 (n1 + n2)
--     Plus e1 e2 ->
--         Plus (optimize e1) (optimize e2)
--     Num1 n ->
--         Num1 n