module Bubble (Model, init, view) where

import Random exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (inSeconds, inMilliseconds, inHours, Time)


type alias Model = (Int, Int, Float, String)

init : Int -> Int -> Time -> Model
init maxWidth maxHeight time =
    let
        seed_ = initialSeed (round time)
        (_, seed0) = generate (int 100 255) seed_
        (x, seed1) = generate (int 0 maxWidth) seed0
        (y, seed2) = generate (int 0 maxHeight) seed1
        (radius, seed3) = generate (float 30 80) seed2
        (b, _) = generate (int 100 255) seed3

        color = "rgb(20,100," ++ (toString b) ++ ")"
     in
    (x, y, radius, color)

view : Model -> Svg
view (x, y, radius, color) = circle [cx (toString x), cy (toString y), r (toString radius), fill "url(#light)"] []


