module HW1_Def exposing (..)
import Dict exposing (Dict)
import Set exposing (fromList, toList)



-- Some abbreviations
--
fst = Tuple.first
snd = Tuple.second
map = List.map
sum = List.sum
filter = List.filter


-- Exercise 1
--

-- occurrences: List Int -> Dict Int Int
-- occurrences list =
--     List.foldl
--         (\num acc ->
--             Dict.update num
--                 (\maybeCount ->
--                     case maybeCount of
--                     Just count -> Just (count +1)
--                     Nothing -> Just 1
--                 )
--                 acc 
--         )
--         Dict.empty
--         list

size : List a -> Int
size l = case l of
        [] -> 0
        x::xs -> 1 + size xs

{-
    length 1 = if isEmpty 1 then 0 else 1 + length
-}

-- isEmpty: List a -> Bool
-- isEmpty l = case l of
--     [] -> True
--     -- or _
--     _::_ -> False

type alias Bag a = List (a, Int)

insert: a -> Bag a -> Bag a
insert item bag =
    case bag of
        [] ->
            [(item, 1)]

        (i, count) :: rest ->
            if i == item then
                (i, count + 1) :: rest
            else
                (i, count) :: insert item rest

remove: a -> Bag a -> Bag a
remove item_remove new_bag =
    case new_bag of
        [] ->
            []

        (i, count) :: rest ->
            if i == item_remove then
                if count > 1 then
                    (i, count - 1) :: rest
                else
                    rest
            else 
                (i, count) :: remove item_remove rest

mkBag: List a -> Bag a
mkBag new_list =
    case new_list of
        [] ->
            []
        n::nw ->
            insert n (mkBag nw)

getCount: a -> Bag a -> Int
getCount item bag =
    case bag of
        [] ->
            0
        (i, count) :: rest ->
            if i == item then
                count
            else
               getCount item rest

isSubbag: Bag a -> Bag a -> Bool
isSubbag ls1 ls2 = 
        case ls1 of
            [] ->
                True
            (item, count1) :: rest_list ->
                if count1 > getCount item ls2 then
                    False
                else
                    isSubbag rest_list ls2

-- Exercise 2
--
type alias Node  = Int
type alias Edge  = (Node, Node)
type alias Graph = List Edge

nodes: Graph -> List Node
nodes g =
        List.map (\(x,y) -> [x,y]) g -- map records and turn them into sets
            |> List.concat -- to flatten the list [x1, x2, x3 ...]
            |> fromList -- to make it a set (check import set) and remove duplicates
            |> toList -- make it a list again

suc: Node -> Graph -> List Node
suc s g = 
    List.filter(\(x,y) -> x == s) g -- if it matches
        |> List.map (\(item, succ) -> succ) -- get the value
        


detach: Node -> Graph -> Graph
detach n g =
    List.filter(\(x,y) -> x /= n && y/= n) g -- filter any coordinates that does not have n

    


asSet : List comparable -> List comparable
asSet = rmDup << List.sort

rmDup : List comparable -> List comparable
rmDup l = case l of
    x::y::zs -> if x==y then rmDup (y::zs) else x::rmDup (y::zs)
    xs       -> xs


-- Exercise 3
--
type alias Number = Int
type alias Point = (Number, Number)
type alias Length = Number
type Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length

type alias Figure = List Shape
type alias BBox = (Point, Point)

addPoint : Point -> Point -> Point
addPoint (x1, y1) (x2, y2) =
        (x1+x2, y1+ y2)

width : Shape -> Length
width shape = 
    case shape of
        Pt x ->
            0
        Circle x y ->
            y*2
        Rect x y z ->
            y

bbox : Shape -> BBox
bbox shape =
    case shape of
        Pt (x, y) ->
            ((x,y), (x,y))
        Circle (x, y) r ->
            ((x - r ,y - r), (x + r,y + r))
        Rect (x, y) w h ->
            ((x,y), (x + w ,y + h))

minX : Shape -> Number
minX shape = 
    case shape of
        Pt (x, y) ->
            x
        Circle (x, y) r ->
            x - r
        Rect (x, y) w h ->
            x

move : Point -> Shape -> Shape
move diff shape = 
    case shape of
        Pt point ->
            Pt (addPoint diff point)
        Circle center r ->
            Circle (addPoint diff center) r
        Rect corner w h ->
            Rect (addPoint diff corner) w h
