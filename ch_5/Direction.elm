module Direction (Direction (..), fromArrowKey) where

type Direction = N | S | W | E

fromArrowKey: {x:Int, y:Int} -> Maybe Direction
fromArrowKey {x,y} =
    if x == -1 then
        Just W
    else if x == 1 then
        Just E
    else if y == -1 then
        Just S
    else if y == 1 then
        Just N
    else
        Nothing
