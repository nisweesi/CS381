module HW3 exposing (..)

{- GROUP:
   Nadir Isweesi
   William Morton
-}

-- 1. Stack Language
-- Abstract Syntax
type Op = LD Int | ADD | MULT | DUP
type alias Prog = List Op
type alias Stack = List Int

-- Semantic Domain
type alias D  = Stack -> (Maybe Stack)

-- Semantic Function
semOp : Op -> D
semOp op stack = case op of 
         LD i -> Just (i::stack)
         ADD  -> case stack of
                 -- if a and b are valid
                 a::b::rs -> Just ((a+b)::rs)
                 -- else throw error
                 _        -> Nothing
         MULT -> case stack of 
                 -- same general form as ADD
                 a::b::rs -> Just ((a*b)::rs)
                 _        -> Nothing
         DUP  -> case stack of
                 a::rs    -> Just (a::a::rs)
                 _        -> Nothing

semProg : Prog -> D
-- could not figure out how to remove the stack parameter requirement
semProg prog stack =
    case prog of
        [] ->
            Just stack

        -- recursively evaluates the operations and modifies stack
        op::rs ->
            case semOp op stack of
                Nothing ->
                    Nothing

                Just newStack ->
                    semProg rs newStack
