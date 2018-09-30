module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFace1 : Face
    , dieFace2 : Face
    }


type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model One One
    , Cmd.none
    )


diceFaces : List ( Int, Face )
diceFaces =
    [ ( 1, One ), ( 2, Two ), ( 3, Three ), ( 4, Four ), ( 5, Five ), ( 6, Six ) ]


faceValue : Face -> Int
faceValue face =
    let
        value =
            diceFaces
                |> List.filter (\n -> Tuple.second n == face)
                |> List.head
    in
    case value of
        Just ( n, _ ) ->
            n

        _ ->
            0


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


isOdd : Int -> Bool
isOdd n =
    modBy 2 n /= 0



-- UPDATE


type Msg
    = Roll
    | RollWeigthed
    | NewFace ( Face, Face )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace rollTwice
            )

        RollWeigthed ->
            ( model
            , Random.generate NewFace rollTwiceWeighted
            )

        NewFace ( newFace1, newFace2 ) ->
            ( Model newFace1 newFace2
            , Cmd.none
            )


rollTwice : Random.Generator ( Face, Face )
rollTwice =
    Random.pair roll roll


roll : Random.Generator Face
roll =
    Random.uniform
        One
        [ Two, Three, Four, Five, Six ]


rollTwiceWeighted : Random.Generator ( Face, Face )
rollTwiceWeighted =
    Random.pair rollWeigthed rollWeigthed


rollWeigthed : Random.Generator Face
rollWeigthed =
    Random.weighted
        ( 5, One )
        [ ( 10, Two )
        , ( 10, Three )
        , ( 10, Four )
        , ( 15, Five )
        , ( 50, Six )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


diceBackground : Svg Msg
diceBackground =
    rect
        [ x "10"
        , y "10"
        , width "100"
        , height "100"
        , rx "15"
        , ry "15"
        ]
        []


dicePoint : Int -> Svg Msg
dicePoint number =
    let
        cxValue =
            (if isOdd number then
                1

             else
                2
            )
                * 33
                + 10
                |> String.fromInt

        cyValue =
            (((number - 1) // 2) + 1) * 25 + 10 |> String.fromInt
    in
    circle
        [ cx cxValue
        , cy cyValue
        , r "10"
        , fill "white"
        ]
        []


dicePoints : Int -> List (Svg Msg)
dicePoints face =
    List.range 1 face
        |> List.map dicePoint


dice : Face -> Svg Msg
dice face =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        (diceBackground
            :: dicePoints (face |> faceValue)
        )


diceWithTitle : Face -> Html Msg
diceWithTitle face =
    div []
        [ h1 [] [ Html.text (face |> faceValue |> String.fromInt) ]
        , dice face
        ]


view : Model -> Html Msg
view model =
    div []
        [ diceWithTitle model.dieFace1
        , diceWithTitle model.dieFace2
        , button [ onClick Roll ] [ Html.text "Roll" ]
        , button [ onClick RollWeigthed ] [ Html.text "Roll weighted" ]
        ]
