module Board (Board, Action(..), Event(..), new, view, update) where

import Snake
import Food exposing (Food)
import Direction exposing (Direction)
import Utils exposing (rect)

import Random
import Svg exposing (Svg, svg)
import Svg.Attributes
import Html exposing (div)
import Html.Attributes
import String


type alias Board = {snake: Snake.Model, food: Food, size: (Int,Int)}
type Action = Step (Maybe Direction) | Start | Stop
type Event = None | Grow | End

type alias Point = (Int,Int)

mailbox: Signal.Mailbox Action
mailbox = Signal.mailbox Stop

new: Board
new =
    let
        snake = Snake.init (3,5) 3
        food = Food.init (2,7)
    in
        Board snake food (30,30)

view: Board -> Svg
view game =
    let
        pixelWidth = (toString ((*) (fst game.size) Utils.width))
        pixelHeight = (toString ((*) (snd game.size) Utils.height))
    in
        div [
            Html.Attributes.style [("border", "2px solid black"), ("display", "inline-block")]
        ] [
            svg [
                Svg.Attributes.width pixelWidth,
                Svg.Attributes.height pixelHeight,
                Svg.Attributes.viewBox (String.concat ["0 0 ", pixelWidth, " ", pixelHeight])

            ] [
                Snake.view game.snake,
                rect game.food.point
            ]
        ]





update: Action -> Board -> (Board, Event)
update action model =
    case action of
        Step dir ->
            let
                newSnake = Snake.update dir model.snake
                (x,y) = Snake.head newSnake
                (fx,fy) = model.food.point
            in
                if x < 0 || y < 0 || x >= (fst model.size) || y>= (snd model.size) || Snake.collides newSnake then --hit itself
                    (model, End)
                else if x==fx && y == fy then  --food eaten
                     let
                        food = Food.update model.food (freeCells model)
                    in
                        ({model| snake=Snake.grow newSnake, food = food}, Grow)
                else
                    ({model| snake=newSnake}, None)
        _ -> (model,None)





freeCells: Board -> List Point
freeCells {snake, size} =
    let
        maxY = (-) (snd size) 1
        maxX = (-) (fst size) 1
        allCells = List.concat (List.map (\a -> List.map (\b -> (a,b)) [0..maxX]) [0..maxY])
        snakeCells = snake.cells
    in
        List.filter (notMember snakeCells) allCells

notMember: List Point -> Point -> Bool
notMember list el = not (List.member el list)

