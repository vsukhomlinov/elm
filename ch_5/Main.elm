import Random
import Time exposing (every, second)
import Keyboard
import Snake
import Signal
import Task exposing (Task)
import String
import Svg exposing (svg)
import Svg.Attributes
import Direction exposing (Direction)
import Signal exposing (sampleOn)
import Html exposing (Html, div, text, button, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Utils exposing (rect, width, height)
import Array exposing (fromList)
import Random exposing (generate)
import Player
import Board exposing (Board, Action, Event)
import Player exposing (Player)
import Food
import Dialog

type Event = Start String | Score String | Blank | Tick (Maybe Direction)
type State = New | Playing | GameOver

type alias Game = {board:Maybe Board, state: State, score:Int}

gameMailbox = Signal.mailbox Blank

newGame = Game Nothing New 0
main: Signal Html
main =
    Signal.map view (Signal.foldp update newGame gameMailbox.signal)

update: Event -> Game -> Game
update event game =
    case event of
        Tick dir ->
            if game.state == Playing then
                let
                    (newBoard, event) = Board.update (Board.Step dir) (justBoard game)
                    newGame = {game | board= Just newBoard}
                in
                    case event of
                        Board.End ->
                            {newGame| state=GameOver}
                        Board.Grow ->
                            {newGame| score=(+) game.score 1}
                        Board.None -> newGame
            else
                game

        Start playerName ->
            let
                (newBoard, _) = Board.update (Board.Start) Board.new
            in
                Game (Just newBoard) Playing 0

        _ -> game


view: Game -> Html
view game =
    case game.state of
        Playing ->
            div [ style [("display", "inline-block")]] [
                div [ style [("text-align", "right")]] [
                    text (String.concat ["Score: ", toString game.score])
                ]
                ,Board.view (justBoard game)
            ]
        New ->
            div [] [
                button [ onClick gameMailbox.address (Start "Aaaa")] [text "Start"]
            ]
        GameOver ->
            div [ style [("display", "inline-block")]] [
                div [] [
                    button [ onClick gameMailbox.address (Start "Aaaa"),  style [("float", "left")] ] [text "Start"]
                    ,span [ style [("float", "right")]] [
                        text (String.concat ["Score: ", toString game.score])
                    ]
                ]
                , div [style [("position", "relative"), ("display", "inline-block")]] [
                    Board.view (justBoard game)
                    , Html.span [style [("left", "50%"), ("top", "50%"), ("position","absolute")]] [
                        Html.text "GAME OVER"
                    ]
                ]
            ]


justBoard: Game -> Board
justBoard {board} =
    case board of
        Nothing -> Board.new
        Just b -> b

port keyboard: Signal (Task x ())
port keyboard =
    sampleOn (every 100) Keyboard.arrows
        |> Signal.map Direction.fromArrowKey
        |> Signal.map Tick
        |> Signal.map (Signal.send gameMailbox.address)




