module NBack where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Time exposing (Time)


-- MODEL

makeGrid size step =
    [0..size - 1]
    |> List.map (\r ->
        [0..size - 1]
        |> List.map (\c ->
            case step of
                Just (position, number) ->
                    if (r, c) == (position // size, position % size) then Just number
                    else Nothing
                Nothing ->
                    Nothing))

type alias StepPosition = Int
type alias StepNumber = Int

type alias Model =
    { gridSize : Int
    , grid : List (List (Maybe Int))
    , startTime : Time
    , stepTime : Time
    , stepInterval : Time
    , seed : Random.Seed
    , stepValues : List (StepPosition, StepNumber)
    , time : Time
    }

init : (Model, Effects Action)
init =
    let initialSize = 3
    in
        ( { gridSize = initialSize
          , grid = makeGrid initialSize Nothing
          , startTime = 0
          , stepTime = 0
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

updateTick time model =
    { model | time = time }

updateStep model =
    let positionsCount = (.gridSize model) ^ 2
        (position, seed1) = Random.generate (Random.int 0 (positionsCount - 1)) (.seed model)
        (number, seed2) = Random.generate (Random.int 1 positionsCount) seed1
    in
        { model
        | time = .time model
        , grid = makeGrid (.gridSize model) (Just (position, number))
        , seed = seed2
        , stepTime = .time model
        , stepValues = (position, number) :: (.stepValues model)
        }

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Tick time ->
            ( if (.startTime model) > 0 && (time - (.stepTime model) > (.stepInterval model))
                 then model |> updateTick time |> updateStep
                 else model |> updateTick time
            , Effects.tick Tick
            )
        SetGridSize n ->
            let size = max 1 n
            in
                ( { model
                  | gridSize = size
                  , grid = makeGrid size Nothing
                  }
                , Effects.none
                )
        Start ->
            let startingSeed = Random.initialSeed (model |> .time |> floor)
            in
                ( { model
                  | startTime = (model |> .time)
                  , stepTime = (model |> .time)
                  , seed = startingSeed
                  , stepValues = []
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
