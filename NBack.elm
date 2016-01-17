module NBack where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style)
import Time exposing (Time)


-- MODEL

emptyGrid size = [1..size] |> List.map (\_ -> [1..size] |> List.map (\_ -> Nothing))

type alias Model =
    { gridSize : Int
    , gridState : List (List (Maybe Int))
    , time : Time
    }

init : (Model, Effects Action)
init =
    let initialSize = 3
    in
        ( { gridSize = initialSize
          , gridState = emptyGrid initialSize
          , time = 0
          }
        , Effects.tick Tick
        )

-- UPDATE

type Action
    = Tick Time
    | SetGridSize Int

update : Action -> Model -> (Model, Effects Action)
update msg model =
    case msg of

        Tick t ->
            ({ model | time = t } , Effects.tick Tick)

        SetGridSize n ->
            let size = max 1 n
            in
                ( { model
                  | gridSize = size
                  , gridState = emptyGrid size
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

cellView cellState =
    let content =
        case cellState of
            Nothing -> []
            Just n -> [text (toString n)]
    in
        div [cellStyle] content

rowView rowState =
    (List.map cellView rowState) ++ [br [style [("clear", "left")]] []]

gridView gridState =
    div [] (List.concatMap rowView gridState)

view : Signal.Address Action -> Model -> Html
view address model =
    div [] [ gridView (.gridState model)
           , div [] [model |> toString |> text]
           ]
