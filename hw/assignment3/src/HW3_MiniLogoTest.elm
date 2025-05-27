module HW3_MiniLogoTest exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- Elm/SVG auxiliary definitions
--
scale = 30

xPt : Int -> String
xPt i = String.fromInt (scale+i*scale)

yPt : Int -> String
yPt i = String.fromInt (398-i*scale)

svgLine : Line -> Svg a
svgLine ((a,b),(c,d)) = line
    [ x1 (xPt a), y1 (yPt b), x2 (xPt c), y2 (yPt d)
    , stroke "green", strokeWidth "4", strokeLinecap "round"] []

main : Html msg
main = svg [viewBox "0 0 400 400", width "800", height "800"]
           (List.map svgLine logoResult)

----- BEGIN HW3 solution

type alias Point = (Int,Int)
type Mode = Up | Down

type Cmd = Pen Mode | MoveTo Point | Seq Cmd Cmd

type alias State = (Mode,Point)
type alias Line = (Point,Point)
type alias Lines = List Line

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

logoResult : Lines
logoResult = lines (Seq (Seq (Seq (Pen Up) (Seq (MoveTo (0,0)) (Seq (Pen Down) (MoveTo (0,1))))) (MoveTo (1,1))) (Seq (MoveTo (1,2)) (MoveTo (2,2))))
