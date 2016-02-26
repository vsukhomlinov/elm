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
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Utils exposing (rect, width, height)
import Array exposing (fromList)
import Random exposing (generate)
import Player
import Board exposing (Board, Action)
import Food

type Event = Start String | Score String | GameOver String | Blank | Tick (Maybe Direction)

gameMailbox = Signal.mailbox Blank

main: Signal Html
main =
    Signal.map view (Signal.foldp update Board.new gameMailbox.signal)

update: Event -> Board -> Board
update event game =
    case event of
        Tick dir ->
            Board.update (Board.Step dir) game
        Start _ ->
            Board.update (Board.Start) game
        Blank -> Board.new
        _ -> game


view: Board -> Html
view board =
    div [] [
        Board.view board
        , button [ onClick gameMailbox.address (Start "")] [text "Start"]
    ]

port keyboard: Signal (Task x ())
port keyboard =
    sampleOn (every 100) Keyboard.arrows
        |> Signal.map Direction.fromArrowKey
        |> Signal.map Tick
        |> Signal.map (Signal.send gameMailbox.address)




