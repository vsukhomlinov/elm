module Person (Model, get, view) where

import Html exposing (..)
import Html.Attributes exposing (src, width, href)
import Task exposing (Task, andThen)
import Http exposing (Error)
import String
import Json.Decode exposing (..)

type alias Model = {login:String, name:String, avatar:String, repos: List(Repo)}
type alias Repo = {url:String, description:String}

get: String -> Task Error Model
get name = Task.map2 (\ model repos -> model repos) (getProfile name) (getRepos name)

getProfile: String -> Task Error (List Repo -> Model)
getProfile name = Http.get decodeProfile (String.append "https://api.github.com/users/" name)

getRepos: String -> Task Error (List Repo)
getRepos name = Http.get decodeRepos (String.concat ["https://api.github.com/users/", name, "/repos"])

view: Model -> Html
view model =
    div [] [
        h1 [] [
            img [src model.avatar, width 100] [],
            text model.name
        ],

        dl [] (List.concat (List.map (\repo -> [dt [] [a [href repo.url] [text repo.url]], dd [] [text repo.description]]) model.repos))

    ]

decodeProfile: Decoder (List Repo -> Model)
decodeProfile =
    object3 Model
      ("login" := string)
      ("name" := string)
      ("avatar_url" := string)

decodeRepos: Decoder (List Repo)
decodeRepos = list decodeRepo

decodeRepo: Decoder Repo
decodeRepo = object2 Repo
    ("html_url" := string)
    ("description" := string)



