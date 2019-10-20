-- Dice roll simulator.
-- Press "Add die" to add a die to the simulation.
-- Press "Delete die" to remove a die from the simulation.
-- Press "Roll" to roll the dice!
--
-- Based on the example at:
--   https://guide.elm-lang.org/effects/random.html
--


module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (disabled)
import Html.Events exposing (..)
import Process
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias DieState =
    { dieFace : Int
    , countDown : Int
    }


type alias Model =
    { roll : Bool
    , dieState : List DieState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model False []
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | Tick Time.Posix
    | NewFace (List Int)
    | RollAmount (List Int)
    | AddDie
    | DelDie


randomVal : Int -> Model -> Random.Generator (List Int)
randomVal upper model =
    Random.list (List.length model.dieState) (Random.int 1 upper)


updateDieFace : DieState -> Int -> DieState
updateDieFace ds val =
    if ds.countDown > 0 then
        { ds | dieFace = val, countDown = ds.countDown - 1 }

    else
        ds


setRollAmount : Model -> List Int -> Model
setRollAmount m amts =
    { m
        | dieState = List.map2 (\a b -> { a | countDown = b }) m.dieState amts
        , roll = True
    }


setDieFaces : Model -> List Int -> Model
setDieFaces m faces =
    { m | dieState = List.map2 updateDieFace m.dieState faces }


anyRolls : Model -> Bool
anyRolls model =
    List.foldr (\a b -> b || a.countDown > 0) False model.dieState


changeDieCount : Model -> Int -> Model
changeDieCount m a =
    { m | dieState = List.repeat (List.length m.dieState + a) (DieState 1 0) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDie ->
            ( changeDieCount model 1
            , Cmd.none
            )

        DelDie ->
            ( changeDieCount model -1
            , Cmd.none
            )

        Roll ->
            ( model
            , Random.generate RollAmount (randomVal 15 model)
            )

        RollAmount amt ->
            ( setRollAmount model amt
            , Cmd.none
            )

        Tick _ ->
            if model.roll then
                ( { model | roll = anyRolls model }
                , Random.generate NewFace (randomVal 6 model)
                )

            else
                ( model
                , Cmd.none
                )

        NewFace newFaces ->
            ( setDieFaces model newFaces
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 150 Tick



-- VIEW


type alias Dot =
    ( Int, Int )


dotLayouts : Dict Int (List Dot)
dotLayouts =
    Dict.fromList
        [ ( 1, [ ( 60, 60 ) ] )
        , ( 2, [ ( 30, 30 ), ( 90, 90 ) ] )
        , ( 3, [ ( 30, 30 ), ( 60, 60 ), ( 90, 90 ) ] )
        , ( 4, [ ( 30, 30 ), ( 30, 90 ), ( 90, 30 ), ( 90, 90 ) ] )
        , ( 5, [ ( 30, 30 ), ( 30, 90 ), ( 60, 60 ), ( 90, 30 ), ( 90, 90 ) ] )
        , ( 6, [ ( 30, 30 ), ( 30, 90 ), ( 30, 60 ), ( 90, 60 ), ( 90, 30 ), ( 90, 90 ) ] )
        ]


drawDots : Dot -> Html msg
drawDots ( xLoc, yLoc ) =
    circle
        [ cx (String.fromInt xLoc)
        , cy (String.fromInt yLoc)
        , r "10"
        , fill "white"
        ]
        []


drawDie : List Dot -> Html Msg
drawDie dots =
    svg
        [ width "120"
        , height "120"
        , viewBox "0, 0, 120, 120"
        ]
        (rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            ]
            []
            :: List.map
                (\a -> drawDots a)
                dots
        )


drawDieOf : DieState -> Html Msg
drawDieOf ds =
    drawDie (Maybe.withDefault [] (Dict.get ds.dieFace dotLayouts))


drawDice : Model -> List (Svg Msg)
drawDice model =
    List.map drawDieOf model.dieState


setDisabled : Model -> Html.Attribute Msg
setDisabled model =
    disabled (List.length model.dieState <= 0)


rollButton : Model -> Html Msg
rollButton model =
    button [ onClick Roll, setDisabled model ] [ Html.text "Roll" ]


addButton : Model -> Html Msg
addButton model =
    button [ onClick AddDie ] [ Html.text "Add die" ]


delButton : Model -> Html Msg
delButton model =
    button [ onClick DelDie, setDisabled model ] [ Html.text "Delete die" ]


drawButtons : Model -> List (Svg Msg)
drawButtons model =
    [ rollButton model
    , addButton model
    , delButton model
    ]


drawMessage : Model -> List (Svg Msg)
drawMessage model =
    if model.roll then
        [ p [] [ Html.text "The dice are Rolling!" ] ]

    else
        [ p []
            [ Html.text ("The total is " ++ String.fromInt (dieTotal model))
            ]
        ]


dieTotal : Model -> Int
dieTotal model =
    List.foldr (\a b -> a.dieFace + b) 0 model.dieState


drawBreak : List (Html Msg)
drawBreak =
    [ br [] [] ]


view : Model -> Html Msg
view model =
    div []
        (List.concat
            [ drawDice model
            , drawBreak
            , drawMessage model
            , drawBreak
            , drawButtons model
            ]
        )
