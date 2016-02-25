import Random
import Time exposing (every, second)
import Keyboard
import Snake
import String
import Svg exposing (svg)
import Svg.Attributes
import Direction exposing (Direction)
import Signal exposing (sampleOn)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Utils exposing (rect, width, height)
import Array exposing (fromList)
import Random exposing (generate)

type alias Game = {snake: Snake.Model, food: Food, dir:Direction, width: Int, height: Int, started: Bool}
type alias Food = {point:(Int,Int), seed: Random.Seed}
type alias Point = (Int, Int)

boardWidth = 30
boardHeight = 30

boardPoints : List (Int, Int)
boardPoints =
    List.map2 (,) [0..boardWidth] [0..boardHeight]

snake: Snake.Model
snake = Snake.init (3,5) 3

food: Food
food = Food (2,9) (Random.initialSeed 3332)

game: Game
game = Game snake food Direction.E boardWidth boardHeight True

main: Signal Html
main =
    Signal.map view (Signal.foldp update game input)

update: Maybe Direction -> Game -> Game
update dir game =
    if game.started then
        let
            newSnake = Snake.update dir game.snake
            (x,y) = Snake.head newSnake
            (fx,fy) = game.food.point
        in
            if x < 0 || y < 0 || x >= game.width || y>= game.height || Snake.collides newSnake then
                {game | started=False}
            else if x==fx && y == fy then  --food eaten
                 let
                    food = updateFood game.food (freeCells game)
                in
                    {game| snake=Snake.grow newSnake, food = food}
            else
                {game| snake=newSnake}
    else
        game

view: Game -> Html
view game =
    let
        pixelWidth = (toString ((*) game.width Utils.width))
        pixelHeight = (toString ((*) game.height Utils.height))
        textStyle =
            if game.started then
                [("display","none")]
            else
                [("left", "50%"), ("top", "50%"), ("position","absolute")]
    in
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
            --, Html.text (toString game)
        ]


input: Signal (Maybe Direction)
input =
    sampleOn (every 100) Keyboard.arrows
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

freeCells: Game -> List Point
freeCells game =
    let
        maxY = (-) game.height 1
        maxX = (-) game.width 1
        allCells = List.concat (List.map (\a -> List.map (\b -> (a,b)) [0..maxX]) [0..maxY])
        snakeCells = game.snake.cells
    in
        List.filter (notMember snakeCells) allCells

notMember: List Point -> Point -> Bool
notMember list el = not (List.member el list)

updateFood: Food -> List Point -> Food
updateFood {seed} points =
    let
        pointArray = fromList points
        (randomIndex, newSeed) = generate (Random.int 0 (Array.length pointArray)) seed
        newPoint = Maybe.withDefault (0,0) (Array.get randomIndex pointArray)
    in
        Food newPoint newSeed
