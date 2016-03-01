module Dialog(Dialog, SavePlayer(..), Action(..), update, view, new, Context) where

import Html exposing (Html, text, div, h3, input, button, form)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal
import Player exposing (Player)
import Graphics.Element exposing (Element, show, container, middle)

type alias Dialog = {name:String, open:Bool, score:Int}
type alias Context = {gameOver: Signal.Address SavePlayer}

type SavePlayer = Save (Maybe Player)
type Action = Open Int | Close | Input String | Submit

new: Dialog
new = Dialog "" False 0

mailbox: Signal.Mailbox Action
mailbox = Signal.mailbox Close

update: Action -> Dialog -> Dialog
update action dialog=
    case action of
        Open score -> {dialog | open=True, score=score}
        Close -> {dialog | open=False, score=0}
        Input name -> {dialog | name=name}
        Submit -> {dialog | open=False}

view: Context -> Dialog -> Html
view context dialog=
    div [style [
            ("position", "absolute"),
            ("left", "50%"),
            ("top", "40%"),
            ("border", "1px solid black"),
            ("box-shadow", "black 0 0 25px -3px"),
            ("width", "200px"),
            ("margin-left", "-100px"),
            ("text-align", "center"),
            ("padding", "30px")]] [
        h3 [] [text "GAME OVER"]
        ,text ("Your score: "++ (toString dialog.score))
        ,div [] [
            input [
                type' "text"
                 ,placeholder "Enter your name"
                 ,on "input" targetValue (\ name -> Signal.message mailbox.address (Input name))
            ] []
            , button [onClick context.gameOver (Save (Just (Player dialog.name dialog.score)))] [text "Save"]
            , button [
                type' "reset"
                ,onClick context.gameOver (Save Nothing)
            ] [text "Cancel"]
        ]
    ]