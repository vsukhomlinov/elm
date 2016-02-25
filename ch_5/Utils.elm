module Utils (rect, width, height) where

import Svg
import Svg.Attributes as Attributes

width = 20
height = 20

rect: (Int, Int) -> Svg.Svg
rect (x,y)=
    Svg.rect [
            Attributes.x (toString ((*) x width)),
            Attributes.y (toString ((*) y height)),
            Attributes.width (toString width),
            Attributes.height (toString height)
        ] []