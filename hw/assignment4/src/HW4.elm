module HW4 exposing (..)

{- GROUP:
   William Morton
   Nadir Isweesi
-}

-- 1. Stack Language

-- A
type Op = LD Int | ADD | MULT | DUP | DEC | SWAP | POP Int
type alias Prog = List Op
type alias Stack = List Int

type alias D  = Stack -> (Maybe Stack)

type alias Rank = Int
type alias OpRank = (Int,Int)

semOp : Op -> D
semOp op stack = case op of 
        LD i -> Just (i::stack)
        ADD  -> case stack of
                 a::b::rs -> Just ((a+b)::rs)
                 _        -> Nothing
        MULT -> case stack of 
                 -- same general form as ADD
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

        POP k ->
            if List.length stack >= k then
                Just (List.drop k stack)
            else
                Nothing


semProg : Prog -> D
semProg prog stack =
    case prog of
        [] ->
            Just stack
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
        LD _     -> (0, 1)
        ADD      -> (2, 1)
        MULT     -> (2, 1)
        DUP      -> (1, 2)
        DEC      -> (1, 1)
        SWAP     -> (2, 2) 
        POP k    -> (k, 0)

rank : Prog -> Rank -> Maybe Rank
rank prog r =
    case prog of
        [] ->
            Just r

        op :: rest ->
            let
                (n, m) = rankOp op
            in
            if r < n then
                Nothing
            else
                rank rest (r - n + m)


rankProg : Prog -> Maybe Rank
rankProg prog =
    rank prog 0

-- B

semTC : Prog -> Maybe Stack
semTC prog =
    case rankProg prog of
        Just _ -> semProg prog []
        Nothing -> Nothing

