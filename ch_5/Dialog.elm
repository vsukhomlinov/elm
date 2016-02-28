module Dialog(Dialog, Event(..), Action(..), update, view, new) where

import Html exposing (Html, text, div, h3)
import Html.Attributes exposing (..)
import Signal
import Graphics.Element exposing (Element, show, container, middle)

type alias Dialog = {name:String, open:Bool, message:String}

type Event = Name String | Cancel | None
type Action = Open String | Close | Input String

new: Dialog
new = Dialog "" False "aaa"

mailbox: Signal.Mailbox Action
mailbox = Signal.mailbox Close

update: Action -> Dialog -> (Dialog, Event)
update action dialog=
    case action of
        Open message -> ({dialog | open=True, message=message}, None)
        Close -> ({dialog | open=False}, Cancel)
        Input name -> ({dialog | name=name}, Name name)

view: Dialog -> Html
view dialog=
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
        ,text dialog.message
    ]