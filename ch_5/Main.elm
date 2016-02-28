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
import Dialog exposing (Dialog, Action, view, update, new)

type Event = Start | Blank | Tick (Maybe Direction)
type State = New | Playing | GameOver

type alias Game = {board:Maybe Board, state: State, score:Int, dialog:Dialog}

gameMailbox = Signal.mailbox Blank

dialog = Dialog.new
newGame = Game Nothing New 0 dialog
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
                            let
                                (newDialog, _) = Dialog.update (Dialog.Open ("Your score: " ++ (toString game.score))) game.dialog
                            in
                                {newGame| state=GameOver, dialog=newDialog}
                        Board.Grow ->
                            {newGame| score=(+) game.score 1}
                        Board.None -> newGame
            else
                game

        Start ->
            let
                (newBoard, _) = Board.update (Board.Start) Board.new
            in
                {game | board = Just newBoard, state=Playing, score=0}

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
                button [ onClick gameMailbox.address Start] [text "Start"]
                ,Dialog.view game.dialog
            ]
        GameOver ->
            div [ style [("display", "inline-block")]] [
                div [] [
                    button [ onClick gameMailbox.address Start,  style [("float", "left")] ] [text "Start"]
                    ,span [ style [("float", "right")]] [
                        text (String.concat ["Score: ", toString game.score])
                    ]
                ]
                , div [style [("position", "relative"), ("display", "inline-block")]] [
                    Board.view (justBoard game)
                    ,Dialog.view game.dialog
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




