import Window
import Bubble
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, every, fps, second, millisecond)

main : Signal Svg
main =
--    Signal.map2 view Window.dimensions (Signal.foldp update [] (Signal.map2 merge (every 100) Window.dimensions))
    Signal.map2 merge (every 100) Window.dimensions
    |> Signal.foldp update []
    |> Signal.map2 view Window.dimensions

type Action = Tick
type alias Model = List (Bubble.Model)

view : (Int, Int) -> Model -> Svg
view (w, h) bubbles =
  svg [width (toString w), height (toString h), viewBox (join " " ["0", "0", toString w, toString h])]
    ((radialGradient [id "light", cx "40%", cy "40%", r "50%", fx "30%", fy "30%"] [
        stop [offset "5%", stopColor "rgba(120,150,255,30)"] [],
        stop [offset "35%", stopColor "rgba(60,70,255,30)"] [],
        stop [offset "75%", stopColor "rgba(20,25,255,30)"] [],
        stop [offset "99%", stopColor "rgba(20,25,220,30)"] [],
        stop [offset "100%", stopColor "rgba(20,25,230,15)"] []
    ]) :: (List.map Bubble.view bubbles))


update: (Time, (Int, Int)) -> Model -> Model
update (time, (w, h)) model =
     model ++ [(Bubble.init w h time)]

merge a b = (a,b)