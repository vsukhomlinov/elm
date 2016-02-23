import StartApp exposing (start)
import Html exposing (..)
import Html.Attributes exposing (type')
import Html.Events exposing (targetValue, on)
import Effects exposing (none, tick, Never, task)
import Time exposing (Time, second)
import Task exposing (Task)
import Person

type alias Model = {query:String, wait:Maybe(Wait), person:Maybe(Person.Model)}
type alias Wait = Float

type Action = Input String | Tick Time | Update (Maybe Person.Model)

duration = second

init = ({query="", wait=Nothing, person= Nothing}, Effects.none)

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
        Input query ->
            ({model | query = query, wait= Just 0}, tick Tick)
        Tick time ->
            case model.wait of
                Nothing -> (model, none)
                Just 0 -> ({model | wait = Just time}, tick Tick)
                Just eventStart ->
                    if eventStart + duration < time then
                        ({model | wait= Nothing}, fetch model.query)
                    else
                        (model, tick Tick)
        Update person -> ({model | person=person}, none)

fetch: String -> Effects.Effects Action
fetch name =
    Person.get name |> Task.toMaybe |> Task.map Update |>  Effects.task

view: Signal.Address Action -> Model -> Html
view address model =
    let
        contents =
            case model.person of
                Nothing -> h1 [] [ text "Use field above to lookup a github account" ]
                Just person -> Person.view person
    in
        div [] [
            input [
                on "input" targetValue (\value -> Signal.message address (Input value))
                , type' "search"
                ] []
            ,
            div [] [
                contents
            ]
        ]

app = start {init=init, update = update, view = view, inputs = []}

main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
