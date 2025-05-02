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
semProg prog stack =
    case prog of
        [] ->
            Just stack

        -- Recursively evaluates the operations and modifies stack
        op::rs ->
            case semOp op stack of
                -- Handles errors
                Nothing ->
                    Nothing

                -- Handles operations
                Just newStack ->
                    semProg rs newStack

-- 2. Mini Logo
-- Abstract Syntax
type alias Point = (Int,Int)
type Mode = Up | Down
type Cmd = Pen Mode | MoveTo Point | Seq Cmd Cmd
type alias State = (Mode,Point)

-- Semantic Domain
type alias Line = (Point,Point)
type alias Lines = List Line

-- Semantic Functions
semCmd : Cmd -> State -> (State,Lines)
semCmd cmd (mode, point) = case cmd of
           Pen m     -> ((m, point), [])           -- Set mode
           MoveTo p  -> case mode of               -- Move to point base on mode
                        Down -> ((mode, p), [(point, p)])
                        Up   -> ((mode, p), [])
           Seq c1 c2 -> let                        -- Recursively solve and combine
                                (state1, lines1) = semCmd c1 (mode, point)
                                (state2, lines2) = semCmd c2 state1
                        in
                                (state2, lines1 ++ lines2)

lines : Cmd -> Lines
lines cmd = let
                initState = (Up, (0,0))                 -- Set initial state 
                (_, drawnLines) = semCmd cmd initState  -- Build on initState
            in
                drawnLines
