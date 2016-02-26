module Player(Model, init, score, end, view) where

import Html exposing (Html)

type alias Model = {name:String, score:Int}

type Event = End

mailbox = Signal.Mailbox

init: String -> Model
init name = Model name 0

score: Model -> Model
score model = {model | score = (+) model.score 1}

end: Model -> Model
end model = model

view: Model -> Html
view model = Html.text (toString model)

