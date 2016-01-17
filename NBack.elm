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
    , n : Int
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
          , n = 2
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
    | DecreaseSize
    | IncreaseSize
    | DecreaseN
    | IncreaseN
    | Start
    | Stop
    | TryMatch Dimension

updateTick time model =
    { model | time = time }

nextStep model =
    let positionsCount = (.gridSize model) ^ 2
        (position, seed1) = Random.generate (Random.int 0 (positionsCount - 1)) (.seed model)
        (number, seed2) = Random.generate (Random.int 1 positionsCount) seed1
        nBackStep = model |> .stepHistory |> List.drop ((.n model) - 1) |> List.head
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

changeSize size model =
    { model
    | gridSize = size
    , grid = makeGrid size Nothing
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
        DecreaseSize ->
            let size = max 2 ((.gridSize model) - 1)
            in
                ( changeSize size model , Effects.none)
        IncreaseSize ->
            let size = (.gridSize model) + 1
            in
                ( changeSize size model , Effects.none)
        DecreaseN ->
            ( { model | n = (max 1 (.n model - 1)) } , Effects.none )
        IncreaseN ->
            ( { model | n = (.n model + 1) } , Effects.none )
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
              , grid = makeGrid (.gridSize model) Nothing
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
    div [style [("float", "left")]] (List.concatMap rowView grid)

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

big = style [ ("font-size", "130%") ]

nView address n =
    div [style [("float", "left")]] [ button [onClick address IncreaseN] [text "+"]
                                    , div [] [ n |> toString |> text]
                                    , button [onClick address DecreaseN] [text "-"]
                                    ]

resizeButtons address =
    [ div [] [ button [onClick address DecreaseSize, big] [text "<<"] ]
    , div [] [ button [onClick address IncreaseSize, big] [text ">>"] ]
    ]

matchButtons address =
    [ button [onClick address (TryMatch PositionDimension), big] [text "Position match!"]
    , button [onClick address (TryMatch NumberDimension), big] [text "Number match!"]
    , div [] [ button [onClick address Stop, big] [text "Stop"] ]
    ]

titleView address n =
    h1 [] [ div [style [("float", "left")]] [text "Dual "]
          , nView address n
          , div [style [("float", "left")]] [text "-Back"]
          , br [ style [ ("clear", "left") ] ] []
          ]

view : Signal.Address Action -> Model -> Html
view address model =
    let previousOutcomes = outcomes (.stepHistory model |> List.drop 1)
        countOutcome outcome = previousOutcomes |> List.filter ((==) outcome) |> List.length
    in
        div [] [ titleView address (.n model)
               , p [] [a [Html.Attributes.href "https://en.wikipedia.org/wiki/N-back"] [text "What is n-back?"]]
               , span [] [ gridView (.grid model)
                         , div [] (if (.startTime model) == 0 then resizeButtons address else [])
                         ]
               , br [ style [ ("clear", "left") ] ] []
               , div [] (
                     if (.startTime model) > 0 then
                         matchButtons address
                     else
                         [ button [onClick address Start, big] [text "Start"] ])
               , div [] ["Hits: " ++ (countOutcome Hit |> toString) |> text]
               , div [] ["Misses: " ++ (countOutcome Miss |> toString) |> text]
               , div [] ["False hits: " ++ (countOutcome FalseHit |> toString) |> text]
               , div [] ["Steps: " ++ (model |> .stepHistory |> List.length |> toString) |> text]
               , br [] []
               , div [] [text "Written in ", a [Html.Attributes.href "http://elm-lang.org/"] [text "Elm"]]
               , div [] [a [Html.Attributes.href "https://github.com/theprash/NBack"] [text "View the source code"]]
               ]
