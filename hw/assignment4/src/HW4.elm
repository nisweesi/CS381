module HW4 exposing (..)

{- GROUP:
   Nadir Isweesi
   William Morton
-}

-- 1. Stack Language

-- A
-- stack operations
type Op = LD Int | ADD | MULT | DUP | DEC | SWAP | POP Int

-- a program of stack operations
type alias Prog = List Op

-- a stack is a list of integers
type alias Stack = List Int

-- in case stack fails
type alias D  = Stack -> (Maybe Stack)

-- number os elements in the stack
type alias Rank = Int

-- rank of operation  (popped, pushed)
type alias OpRank = (Int,Int)

semOp : Op -> D
semOp op stack = case op of 
        -- push one element (i)
        LD i -> Just (i::stack)
        -- pop two elements and push their sum
        ADD  -> case stack of
                 a::b::rs -> Just ((a+b)::rs)
                 _        -> Nothing
        -- same general form as ADD
        MULT -> case stack of 
                 a::b::rs -> Just ((a*b)::rs)
                 _        -> Nothing
        DUP  -> case stack of
                 a::rs    -> Just (a::a::rs)
                 _        -> Nothing
        DEC ->
            case stack of
                x :: rest -> Just ((x - 1) :: rest)
                _ -> Nothing

        SWAP ->
            case stack of
                x :: y :: rest -> Just (y :: x :: rest)
                _ -> Nothing
        -- pop k elements from the stack
        POP k ->
            if List.length stack >= k then
                Just (List.drop k stack)
            else
                Nothing


semProg : Prog -> D
semProg prog stack =
    case prog of
        [] ->
            Just stack -- base case, the stack is done
        -- recursively evaluates the operations and modifies stack
        op::rs ->
            case semOp op stack of
                -- handles errors
                Nothing ->
                    Nothing

                -- handles operations
                Just newStack ->
                    semProg rs newStack

rankOp : Op -> OpRank
rankOp op =
    case op of
        LD _     ->
            (0, 1) -- pushed one item
        ADD      ->
            (2, 1) -- pop two elements, push one
        MULT     ->
            (2, 1) -- pop two elements, push one
        DUP      ->
            (1, 2) -- pop one element, push two
        DEC      ->
            (1, 1) -- pop one, and push one
        SWAP     ->
            (2, 2) -- pop two elements, and push two
        POP k    ->
            (k, 0) -- pop k elements

rank : Prog -> Rank -> Maybe Rank
rank prog r =
    case prog of
        [] ->
            Just r -- base case, return the result

        op :: rest ->
            let
                (n, m) = rankOp op -- n popped elements, pushed elements 
            in
            if r < n then
                Nothing -- less elemenets than allowed
            else
                rank rest (r - n + m)

-- make sure the program is well typed when start
rankProg : Prog -> Maybe Rank
rankProg prog =
    rank prog 0

-- B

-- if valid then runs the program on an empty stack.
semTC : Prog -> Maybe Stack
semTC prog =
    case rankProg prog of
        Just _ ->
            semProg prog []
        Nothing -> 
            Nothing

-- 2. Shape Language

-- A
type Shape = X | LR Shape Shape | TB Shape Shape
type alias BBox = (Int,Int)

-- Define type checker
bbox : Shape -> BBox

bbox shape = 
    case shape of
        X ->
            (1, 1) -- base case
        LR shape1 shape2 ->
            let
                (w1, h1) = bbox shape1
                (w2, h2) = bbox shape2
            in
                (w1+w2, max h1 h2) -- total width and the maximum height
        TB shape1 shape2 ->
            let
                (w1, h1) = bbox shape1
                (w2, h2) = bbox shape2
            in
                (max w1 w2, h1+h2) -- total height and the total height

-- B
-- Define a type checker that uses the following declaration
rect : Shape -> Maybe BBox
rect shape = 
    case shape of
        X ->
            Just (1, 1) -- base case
        LR shape1 shape2 ->
            case (rect shape1, rect shape2) of
                (Just (w1, h1), Just (w2, h2)) ->
                    if h1 == h2 then -- both shapes are rectangular and same height
                        Just (w1 + w2, h1) -- or h2
                    else
                        Nothing
                _ ->
                    Nothing

        TB shape1 shape2 ->
            case (rect shape1, rect shape2) of
                (Just (w1, h1), Just (w2, h2)) ->
                    if w1 == w2 then -- both shapes are rectangular and same width
                        Just (w1, h1 + h2) -- or h1
                    else
                        Nothing
                _ ->
                    Nothing
