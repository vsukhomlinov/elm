module Board (Board, Action(..), new, view, update) where

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


type alias Board = {snake: Snake.Model, food: Food, size: (Int,Int), started: Bool}
type Action = Step (Maybe Direction) | Start | Stop

type alias Point = (Int,Int)

mailbox: Signal.Mailbox Action
mailbox = Signal.mailbox Stop

new: Board
new =
    let
        snake = Snake.init (3,5) 3
        food = Food.init (2,7)
    in
        Board snake food (30,30) False

view: Board -> Svg
view game =
    let
        pixelWidth = (toString ((*) (fst game.size) Utils.width))
        pixelHeight = (toString ((*) (snd game.size) Utils.height))
        textStyle =
            if game.started then
                [("display","none")]
            else
                [("left", "50%"), ("top", "50%"), ("position","absolute")]
    in
        div [] [
            div [
                Html.Attributes.style [("border", "2px solid black"), ("display", "inline-block"), ("position", "relative")]
            ] [
                Html.span [Html.Attributes.style textStyle] [Html.text "GAME OVER"],
                svg [
                    Svg.Attributes.width pixelWidth,
                    Svg.Attributes.height pixelHeight,
                    Svg.Attributes.viewBox (String.concat ["0 0 ", pixelWidth, " ", pixelHeight])

                ] [
                    Snake.view game.snake,
                    rect game.food.point
                ]
            ]
            , div [] [Html.text (toString game)]

        ]


update: Action -> Board -> Board
update action model =
    case action of
        Step dir ->
            if model.started then
                let
                    newSnake = Snake.update dir model.snake
                    (x,y) = Snake.head newSnake
                    (fx,fy) = model.food.point
                in
                    if x < 0 || y < 0 || x >= (fst model.size) || y>= (snd model.size) || Snake.collides newSnake then
                        {model | started=False}
                    else if x==fx && y == fy then  --food eaten
                         let
                            food = Food.update model.food (freeCells model)
                        in
                            {model| snake=Snake.grow newSnake, food = food}
                    else
                        {model| snake=newSnake}
            else
                model
        Start ->
            {model | started= True}
        _ -> model





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

