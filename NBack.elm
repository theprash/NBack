module NBack where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Time exposing (Time)


-- MODEL

emptyGrid size = [1..size] |> List.map (\_ -> [1..size] |> List.map (\_ -> Nothing))

type alias StepPosition = Int
type alias StepNumber = Int

type alias Model =
    { gridSize : Int
    , grid : List (List (Maybe Int))
    , startTime : Time
    , stepInterval : Time
    , seed : Random.Seed
    , stepValues : List (StepPosition, StepNumber)
    , time : Time
    }

init : (Model, Effects Action)
init =
    let initialSize = 3
        (random, seed) = Random.generate (Random.int 1 9) (Random.initialSeed (floor 0.0))
    in
        ( { gridSize = initialSize
          , grid = emptyGrid initialSize
          , startTime = 0
          , stepInterval = 3 * Time.second
          , seed = Random.initialSeed 0
          , stepValues = []
          , time = 0
          }
        , Effects.tick Tick
        )

-- UPDATE

type Action
    = Tick Time
    | SetGridSize Int
    | Start

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of

        Tick t ->
            ( { model
              | time = t
              }
              , Effects.tick Tick
            )

        SetGridSize n ->
            let size = max 1 n
            in
                ( { model
                  | gridSize = size
                  , grid = emptyGrid size
                  }
                , Effects.none
                )

        Start ->
            let startingSeed = Random.initialSeed (model |> .time |> floor)
            in
                ( { model
                  | startTime = (model |> .time)
                  , seed = startingSeed
                  }
                , Effects.none
                )

-- VIEW

cellStyle =
    style [ ("float", "left")
          , ("width", "50px")
          , ("height", "50px")
          , ("font-size", "50px")
          , ("line-height", "50px")
          , ("text-align", "center")
          , ("vertical-align", "middle")
          , ("border", "1px solid black")
          ]

cellView cell =
    let content =
        case cell of
            Nothing -> []
            Just n -> [text (toString n)]
    in
        div [cellStyle] content

rowView row =
    (List.map cellView row) ++ [br [style [("clear", "left")]] []]

gridView grid =
    div [] (List.concatMap rowView grid)

view : Signal.Address Action -> Model -> Html
view address model =
    div [] [ gridView (.grid model)
           , button [onClick address Start] [text "Start"]
           , div [] [model |> toString |> text]
           ]
