{-
GROUP:
    Nadir Isweesi
    William Morton
-}

module HW2 exposing (..)

-- Problem 1

-- A
type alias Name = String
type alias Num = Int

type Cmd 
        = Pen Mode
        | MoveTo Pos Pos
        | Def Name Pars Cmd
        | Call Name Vals
        | Sequence Cmd Cmd

type Mode 
        = Up
        | Down

type Pos
        = NumPos Num
        | NamedPos Name

type Pars
        = ManyP Name Pars
        | SingleP Name

type Vals
        = ManyV Num Vals
        | SingleV Num

-- B
vector : Cmd
vector =
        Def "vector" ["x1", "y1", "x2", "y2"]
            (Seq
                (Pen Down)
                (Seq
                    (MoveTo (NamedPos "x1") (NamedPos "y1"))
                    (MoveTo (NamedPos "x2") (NamedPos "y2"))
                )
            )

-- Problem 2

-- A

type alias NonTerm = String
type alias Term = String

type alias Grammar = List Prod

type alias Prod = (NonTerm, List Rhs)

type Rhs = Rhs (List Symbol)

type Symbol = Nt NonTerm
            | T Term


-- B

condP : Prod
condP = 
        ( "cond",
        [ Rhs [T "T"]
        , Rhs [T "not", Nt "cond"]
        , Rhs [T "(", Nt "cond", T ")"]
        ]
        )

stmtP : Prod
stmtP = 
        ( "stmt",
        [ Rhs [T "skip"]
        , Rhs [T "while", Nt "cond", T "do", T "{", Nt "stmt", T "}"]
        , Rhs [Nt "stmt", T ";", Nt "stmt"]
        ]
        )

imp : Grammar
imp = 
    [ condP
    , stmtP
    ]

-- C

nonterminals : Grammar -> List NonTerm
nonterminals grammar = 
    List.map Tuple.first grammar

-- terminals : Grammar -> List Term


-- Problem 3

-- A
type RegEx 
        = Empty
        | Any
        | Letter Char -- can be named anything?
        | Optional RegEx
        | Star RegEx -- or mul?
        | Plus RegEx -- or add ?
        | Seq RegEx RegEx
        | Or RegEx RegEx

-- B
simplify : RegEx -> RegEx
simplify exp
        = case exp of
            Star inn ->
                case simplify inn of
                    Star e ->
                        Star e
                    simplified ->
                        Star simplified
            Plus inn ->
                case simplify inn of
                    Plus e ->
                        Plus e
                    Star e ->
                        Star e
                    simplified ->
                        Plus simplified
            Or e1 e2 ->
                let 
                    se1 = simplify e1
                    se2 = simplify e2
                in
                if se1 == se2 then
                    se1 -- or se2
                else
                    Or se1 se2
            Seq e1 e2 ->
                Seq (simplify e1) (simplify e2)
            
            Optional e1 ->
                Optional (simplify e1)

            -- Base cases
            Empty ->
                Empty
            
            Any ->
                Any

            Letter c ->
                Letter c


