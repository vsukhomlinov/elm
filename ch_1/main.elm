import StartApp.Simple exposing (start)
import Graphics.Element exposing (Element, show, middle, container, color, leftAligned)
import Color exposing (red, white)
import Text exposing (fromString)

import Window
import Mouse

whichPart : (Int, Int) -> (Int, Int) -> (Int, Int, String)
whichPart (w,h) (l,t) = if l < w//2 then (w,h,"Left" ) else (w,h,"Right")

main : Signal Element
main =
  Signal.map view (Signal.map2 whichPart Window.dimensions Mouse.position)


view : (Int, Int, String) -> Element
view (w, h, text) =
  container w h middle (color red  (leftAligned (Text.color white (fromString text))))

