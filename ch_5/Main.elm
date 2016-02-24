import Random
import Time exposing (every, second)
import Keyboard
import Snake
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Direction exposing (Direction)
import Signal exposing (sampleOn)
import Html exposing (Html, text)

type alias Game = {snake: Snake.Model, food: Food, dir:Direction, width: Int, height: Int}
type alias Food = {x: Int, y: Int, seed: Random.Seed}

rectWidth = 20
rectHeight = 20

snake: Snake.Model
snake = Snake.init (3,5) 3

food: Food
food = Food 2 9 (Random.initialSeed 3332)

game: Game
game = Game snake food Direction.E 10 10

main: Signal Html
main =
    Signal.map view (Signal.foldp update game input)

update: Maybe Direction -> Game -> Game
update dir game =
    let
        newSnake = Snake.update dir game.snake

    in
        {game| snake=newSnake}

view: Game -> Svg
view game =
    let
        pixelWidth = (toString ((*) game.width rectWidth))
        pixelHeight = (toString ((*) game.height rectHeight))
    in
        svg [
            width pixelWidth,
            height pixelHeight,
            viewBox (String.concat ["0 0 ", pixelWidth, " ", pixelHeight])
        ] [
            Snake.view game.snake
        ]

input: Signal (Maybe Direction)
input =
    sampleOn (every second) Keyboard.arrows
        |> Signal.map direction

direction: {x:Int, y:Int} -> Maybe Direction
direction {x,y} =
    if x == -1 then
        Just Direction.W
    else if x == 1 then
        Just Direction.E
    else if y == -1 then
        Just Direction.S
    else if y == 1 then
        Just Direction.N
    else
        Nothing