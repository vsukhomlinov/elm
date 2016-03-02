module Dialog(Dialog, SavePlayer(..), Action(..), update, view, new, Context) where

import Html exposing (Html, text, div, h2, h3, input, button, form)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal
import Player exposing (Player)
import Graphics.Element exposing (Element, show, container, middle)

type alias Dialog = {name:String, open:Bool, score:Int}
type alias Context = {gameOver: Signal.Address SavePlayer, step: Signal.Address Action}

type SavePlayer = Save (Maybe Player)
type Action = Open Int | Close | Input String | Submit

new: Dialog
new = Dialog "Guest" False 0

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
    div [class "dialogPosition"] [
        div [class "dialog"] [
            h2 [] [text "GAME OVER"]
            ,h3 [] [text ("Your score: "++ (toString dialog.score))]
            ,div [] [
                input [
                    type' "text"
                     ,placeholder "Enter your name"
                     ,on "input" targetValue (\ name -> Signal.message context.step (Input name))
                ] []
                , button [onClick context.gameOver (Save (Just (Player dialog.name dialog.score)))] [text "Save"]
                , button [
                    type' "reset"
                    ,onClick context.gameOver (Save Nothing)
                ] [text "Cancel"]
            ]
        ]
    ]
