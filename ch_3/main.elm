import StartApp exposing (start)
import Bubble exposing (..)
import Time exposing (..)
import Random exposing (Seed, int, generate, initialSeed)
import Effects exposing (..)
import Html exposing (Html, text, button, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import String exposing (join)
import List exposing (map)
import Window exposing ( dimensions)
import Keyboard exposing (presses)
import Char exposing (fromCode)

type Action = Tick Time | Dimensions (Int, Int) | Pause | Reset | Noop
type alias Model = {bubbles: List(Bubble.Model)
    ,seed: Random.Seed
    ,dimensions: (Int, Int)
    ,paused: Bool}


init: (Model, Effects Action)
init = ({bubbles=[], seed = initialSeed 12345, dimensions=(2000, 1200), paused=False}, tick Tick)

update: Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Noop -> (model, none)
        Tick time ->
            let
                    (_, newSeed) = generate (int 0 100) model.seed
                    (width, height) = model.dimensions
                    bubble = Bubble.init newSeed width height
                    newModel = {model | bubbles= (model.bubbles ++ [bubble]), seed=newSeed}
            in
                if model.paused then
                    (model, none)
                else
                    (newModel, none)

        Dimensions (w,h) -> ({model | dimensions=(w,h)}, none)
        Pause ->
            if model.paused then
                ({model | paused=False}, none)
            else ({model | paused=True}, none)
        Reset -> ({model | bubbles = [], paused=False}, none)

view: Signal.Address Action -> Model -> Html
view address {bubbles, seed, dimensions} =
    let
        (w, h) = dimensions
    in
        div [] [
            div [Html.Attributes.style [("position", "absolute")]] [
                button [onClick address Pause] [Html.text "Pause"],
                button [onClick address Reset] [Html.text "Reset"]
            ],

            svg [width (toString w), height (toString h), viewBox (join " " ["0", "0", toString w, toString h])]
                ((radialGradient [id "light", cx "40%", cy "40%", r "50%", fx "30%", fy "30%"] [
                    stop [offset "5%", stopColor "rgba(120,150,255,30)"] [],
                    stop [offset "35%", stopColor "rgba(60,70,255,30)"] [],
                    stop [offset "75%", stopColor "rgba(20,25,255,30)"] [],
                    stop [offset "99%", stopColor "rgba(20,25,220,30)"] [],
                    stop [offset "100%", stopColor "rgba(20,25,230,15)"] []
                ]) :: (List.map Bubble.view bubbles))
        ]

codeToAction: Int -> Action
codeToAction code =
    let
        symbol = fromCode code
    in
        case symbol of
            'p' -> Pause
            'r' -> Reset
            _   -> Noop


app = start {
    init= init,
    update= update,
    view= view,
    inputs= [
        Signal.map Tick (fps 60),
        Signal.map Dimensions Window.dimensions,
        Signal.map codeToAction Keyboard.presses
        ]}

main = app.html
