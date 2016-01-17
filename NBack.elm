module NBack where

import Array
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Time exposing (Time)

-- MODEL

type TryMatchOutcome = Hit | Miss | FalseHit

type alias Step =
    { position : Int
    , number : Int
    , isPositionMatch : Bool
    , isNumberMatch : Bool
    , triedPositionMatch : Bool
    , triedNumberMatch : Bool
    }

type alias Model =
    { gridSize : Int
    , grid : List (List (Maybe Int))
    , startTime : Time
    , stepTime : Time
    , stepInterval : Time
    , seed : Random.Seed
    , stepHistory : List Step
    , time : Time
    }

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
          , stepHistory = []
          , time = 0
          }
        , Effects.tick Tick
        )

-- UPDATE

type Dimension = PositionDimension | NumberDimension

type Action
    = Tick Time
    | SetGridSize Int
    | Start
    | Stop
    | TryMatch Dimension

updateTick time model =
    { model | time = time }

nextStep model =
    let positionsCount = (.gridSize model) ^ 2
        (position, seed1) = Random.generate (Random.int 0 (positionsCount - 1)) (.seed model)
        (number, seed2) = Random.generate (Random.int 1 positionsCount) seed1
        nBackStep = model |> .stepHistory |> List.drop (2 - 1) |> List.head
        nextStep = { position = position
                   , number = number
                   , isPositionMatch = Just position == (nBackStep |> Maybe.map .position)
                   , isNumberMatch = Just number == (nBackStep |> Maybe.map .number)
                   , triedPositionMatch = False
                   , triedNumberMatch = False
                   }
    in
        { model
        | time = .time model
        , grid = makeGrid (.gridSize model) (Just (position, number))
        , seed = seed2
        , stepTime = .time model
        , stepHistory = nextStep :: (.stepHistory model)
        }

tryMatch dimension model =
    { model
    | stepHistory =
        case (.stepHistory model) of
            step :: rest ->
                case dimension of
                    PositionDimension ->
                        { step | triedPositionMatch = True } :: rest
                    NumberDimension ->
                        { step | triedNumberMatch = True } :: rest
            steps -> steps
    }

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Tick time ->
            ( if (.startTime model) > 0 && (time - (.stepTime model) > (.stepInterval model))
                 then model |> updateTick time |> nextStep
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
                  , stepHistory = []
                  }
                , Effects.none
                )
        Stop ->
            ( { model
              | startTime = 0
              , stepTime = 0
              }
            , Effects.none
            )
        TryMatch dimension ->
            ( tryMatch dimension model
            , Effects.none
            )

-- VIEW

cellStyle =
    style [ ("float", "left")
          , ("width", "80px")
          , ("height", "80px")
          , ("font-size", "80px")
          , ("line-height", "80px")
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
    (List.map cellView row) ++ [ br [ style [ ("clear", "left") ] ] [] ]

gridView grid =
    div [] (List.concatMap rowView grid)

outcomes stepHistory =
    let positionPairs = stepHistory |> List.map (\step -> (.isPositionMatch step, .triedPositionMatch step))
        numberPairs = stepHistory |> List.map (\step -> (.isNumberMatch step, .triedNumberMatch step))
    in
        positionPairs ++ numberPairs
        |> List.filterMap (\(isMatch, triedMatch) ->
            case (isMatch, triedMatch) of
                (True, True) -> Just Hit
                (True, False) -> Just Miss
                (False, True) -> Just FalseHit
                (False, False) -> Nothing)

view : Signal.Address Action -> Model -> Html
view address model =
    let outcomeList = outcomes (.stepHistory model)
        countOutcome outcome = outcomeList |> List.filter ((==) outcome) |> List.length
    in
        div [] [ gridView (.grid model)
               , div [] (
                     if (.startTime model) > 0 then
                         [ button [onClick address (TryMatch PositionDimension)] [text "Position match!"]
                         , button [onClick address (TryMatch NumberDimension)] [text "Number match!"]
                         , div [onClick address Stop] [ button [] [text "Stop"] ]
                         ]
                     else
                         [ button [onClick address Start] [text "Start"] ] )
               , div [] ["Hits: " ++ (countOutcome Hit |> toString) |> text]
               , div [] ["Misses: " ++ (countOutcome Miss |> toString) |> text]
               , div [] ["False hits: " ++ (countOutcome FalseHit |> toString) |> text]
               ]
