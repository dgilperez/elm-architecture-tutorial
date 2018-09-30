module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TooglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        TooglePause ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.paused of
        True ->
            Sub.none

        False ->
            Time.every 1000 Tick



-- VIEW


pauseButtonText : Model -> String
pauseButtonText model =
    if model.paused then
        "Restart"

    else
        "Pause"


type alias TimeRecord =
    { hour : Int
    , minute : Int
    , second : Int
    }


timeToRecord : Model -> TimeRecord
timeToRecord model =
    { hour = Time.toHour model.zone model.time
    , minute = Time.toMinute model.zone model.time
    , second = Time.toSecond model.zone model.time
    }


type alias Point =
    ( Int, Int )


type alias PointString =
    ( String, String )


getCenter : Int -> Int -> Point
getCenter radius offset =
    ( radius + offset, radius + offset )


getPointString : Point -> PointString
getPointString point =
    ( point |> Tuple.first |> String.fromInt, point |> Tuple.second |> String.fromInt )


clockHandlePoint : Int -> Int -> Int -> Point -> Int -> Point
clockHandlePoint minuteHourOrSecond parts radius center padding =
    let
        minuteHourOrSecondFloat =
            minuteHourOrSecond |> toFloat

        radiusFloat =
            radius - padding |> toFloat

        x =
            radiusFloat
                * cos (pi / -2 + (2 * minuteHourOrSecondFloat * pi) / toFloat parts)
                + (center |> Tuple.first |> toFloat)
                - (padding |> toFloat)

        y =
            radiusFloat
                * sin (pi / -2 + (2 * minuteHourOrSecondFloat * pi) / toFloat parts)
                + (center |> Tuple.second |> toFloat)
                - (padding |> toFloat)
    in
    ( x |> round, y |> round )


clockHandle : Int -> Int -> Int -> String -> String -> Int -> Svg Msg
clockHandle timeValue parts radius color width padding =
    let
        center =
            getCenter radius padding

        point =
            clockHandlePoint (timeValue |> modBy parts) parts radius center padding
                |> getPointString

        radiusValue =
            radius |> String.fromInt
    in
    line
        [ x1 (point |> Tuple.first)
        , y1 (point |> Tuple.second)
        , x2 radiusValue
        , y2 radiusValue
        , stroke color
        , strokeWidth width
        ]
        []


clockHourHandle : TimeRecord -> Int -> Int -> Svg Msg
clockHourHandle time radius padding =
    clockHandle time.hour 12 radius "orange" "8" padding


clockMinuteHandle : TimeRecord -> Int -> Int -> Svg Msg
clockMinuteHandle time radius padding =
    clockHandle time.minute 60 radius "black" "2" padding


clockSecondHandle : TimeRecord -> Int -> Int -> Svg Msg
clockSecondHandle time radius padding =
    clockHandle time.second 60 radius "gray" "1" padding


clockHourMark : Int -> Int -> Int -> Svg Msg
clockHourMark radius padding hour =
    let
        center =
            getCenter radius padding

        position =
            clockHandlePoint (hour |> modBy 12) 12 radius center padding
                |> getPointString
    in
    circle
        [ cx (position |> Tuple.first)
        , cy (position |> Tuple.second)
        , r "5"
        , fill "black"
        ]
        []


clockHourMarks : Int -> Int -> List (Svg Msg)
clockHourMarks radius padding =
    List.map (clockHourMark radius padding) (List.range 0 11)


clockSphereBase : Int -> Int -> Svg Msg
clockSphereBase radius padding =
    let
        radiusValue =
            radius |> String.fromInt
    in
    circle
        [ cx radiusValue
        , cy radiusValue
        , r (radius - padding |> String.fromInt)
        , fill "lightgray"
        ]
        []


clockSphere : Int -> Int -> List (Svg Msg)
clockSphere radius padding =
    [ clockSphereBase radius padding ]
        ++ clockHourMarks radius padding


clockSvg : TimeRecord -> Int -> Int -> Svg Msg
clockSvg time radius padding =
    let
        boundingBox =
            (radius * 2) + padding |> String.fromInt
    in
    svg
        [ width boundingBox
        , height boundingBox
        , viewBox ("0 0 " ++ boundingBox ++ " " ++ boundingBox)
        ]
        (List.append
            (clockSphere radius padding)
            [ clockHourHandle time radius padding
            , clockMinuteHandle time radius padding
            , clockSecondHandle time radius padding
            ]
        )


view : Model -> Html Msg
view model =
    let
        timeRecord =
            timeToRecord model

        timeTitle =
            (timeRecord.hour |> String.fromInt) ++ ":" ++ (timeRecord.minute |> String.fromInt) ++ ":" ++ (timeRecord.second |> String.fromInt)
    in
    div []
        [ h1
            [ Html.Attributes.style "color" "red"
            ]
            [ Html.text timeTitle ]
        , clockSvg timeRecord 100 25
        , button [ onClick TooglePause ]
            [ Html.text (model |> pauseButtonText) ]
        ]
