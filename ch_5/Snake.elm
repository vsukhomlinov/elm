module Snake (Model, init, update, view) where

import Direction exposing (Direction)
import List exposing (repeat, indexedMap, head, reverse)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Point = (Int,Int)
type alias Model = {cells: List Point, dir:Direction}

rectWidth = 20
rectHeight = 20


init: Point -> Int -> Model
init head length = Model (indexedMap (\ i (x,y) -> (x-i, y)) (repeat length head)) Direction.E

update: Maybe Direction -> Model -> Model
update dir model =
    let
        newDir =
            case dir of
                Nothing -> model.dir
                Just dir -> changeDirection model.dir dir
        head' = newHead (Maybe.withDefault (0,0) (head model.cells)) newDir
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
    g [] (List.map (\(x',y') -> rect [
        x (toString ((*) x' rectWidth)),
        y (toString ((*) y' rectHeight)),
        width (toString rectWidth),
        height (toString rectHeight)
    ] []) model.cells)
