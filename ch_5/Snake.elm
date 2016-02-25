module Snake (Model, init, update, view, head, grow, collides) where

import Direction exposing (Direction)
import List exposing (repeat, indexedMap, head, reverse)
import Svg exposing (Svg, g)
import Utils exposing (rect)


type alias Point = (Int,Int)
type alias Model = {cells: List Point, dir:Direction}

init: Point -> Int -> Model
init head' length = Model (indexedMap (\ i (x,y) -> (x-i, y)) (repeat length head')) Direction.E

update: Maybe Direction -> Model -> Model
update dir model =
    let
        newDir =
            case dir of
                Nothing -> model.dir
                Just dir -> changeDirection model.dir dir
        head' = newHead (Maybe.withDefault (0,0) (List.head model.cells)) newDir
    in
        {model | dir=newDir , cells = List.append [head'] (cutOne model.cells)}


changeDirection: Direction -> Direction -> Direction
changeDirection from to =
    if from == Direction.N && to == Direction.S then
        from
    else if from == Direction.S && to == Direction.N then
        from
    else if from == Direction.E && to == Direction.W then
        from
    else if from == Direction.W && to == Direction.E then
        from
    else
        to

newHead: Point -> Direction -> Point
newHead (x, y) dir =
    case dir of
        Direction.N -> (x,y-1)
        Direction.S -> (x,y+1)
        Direction.W -> (x-1,y)
        Direction.E -> (x+1,y)

cutOne: List a -> List a
cutOne list=
    reverse list
        |> List.drop 1
        |> reverse

view: Model -> Svg
view model =
    g [] (List.map rect model.cells)

head: Model -> (Int,Int)
head {cells} =
    let
        h = List.head cells
    in case h of
        Nothing -> (0,0)
        Just p -> p

grow: Model -> Model
grow model =
    let
        cells = reverse model.cells
        tail = Maybe.withDefault (0,0) (List.head cells)
    in
        {model | cells = List.append model.cells [tail]}

collides: Model -> Bool
collides model =
    let
        head' = head model
        tail' = Maybe.withDefault [] (List.tail model.cells)
    in
        List.member head' tail'
