module Food (Food, init, update) where

import Random
import Array


type alias Food = {point:(Int,Int), seed: Random.Seed}

init: (Int, Int) -> Food
init (x,y) = Food (x,y) (Random.initialSeed ((+) x y))

update: Food -> List (Int,Int) -> Food
update {seed} points =
    let
        pointArray = Array.fromList points
        (randomIndex, newSeed) = Random.generate (Random.int 0 (Array.length pointArray)) seed
        newPoint = Maybe.withDefault (0,0) (Array.get randomIndex pointArray)
    in
        Food newPoint newSeed
