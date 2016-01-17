module NBack where

import Effects exposing (Effects)
import Html exposing (..)
import Time exposing (Time)


-- MODEL

type alias Model = { time : Time } 

init : (Model, Effects Action)
init =
    ( { time = 0 }
    , Effects.tick Tick
    )

-- UPDATE

type Action
    = Tick Time

update : Action -> Model -> (Model, Effects Action)
update msg model =
    case msg of
        Tick t ->
            ({ time = t }, Effects.tick Tick)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
    div [] [text (model |> .time |> toString)]
