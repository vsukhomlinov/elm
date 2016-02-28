module Player(Player, init, score, end, view) where

import Html exposing (Html, text, div)
import String

type alias Player = {name:String, score:Int}

type Event = End

mailbox = Signal.Mailbox

init: String -> Int -> Player
init name score = Player name score

score: Player -> Player
score model = {model | score = (+) model.score 1}

end: Player -> Player
end model = model

view: Player -> Html
view model =
    div [] [
        text (String.concat [model.name, ": ", (toString model.score)])

    ]

