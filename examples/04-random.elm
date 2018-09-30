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
    { dieFace : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1
    , Cmd.none
    )


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


isOdd : Int -> Bool
isOdd n =
    modBy 2 n /= 0



-- UPDATE


type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( Model newFace
            , Cmd.none
            )



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


dice : Int -> Svg Msg
dice face =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        (diceBackground
            :: dicePoints face
        )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text (String.fromInt model.dieFace) ]
        , dice model.dieFace
        , button [ onClick Roll ] [ Html.text "Roll" ]
        ]
