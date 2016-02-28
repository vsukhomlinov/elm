module Dialog(Dialog, Event(..), Action(..), update, view) where

import Html exposing (Html, text)
import Signal

type alias Dialog = {name:String, open:Bool, message:String}

type Event = Name String | Cancel | None
type Action = Open String | Close | Input String

mailbox: Signal.Mailbox Action
mailbox = Signal.mailbox Close

update: Action -> Dialog -> (Dialog, Event)
update action dialog=
    case action of
        Open message -> ({dialog | open=True, message=message}, None)
        Close -> ({dialog | open=False}, Cancel)
        Input name -> ({dialog | name=name}, Name name)

view: Dialog -> Html
view dialog= text (toString dialog)