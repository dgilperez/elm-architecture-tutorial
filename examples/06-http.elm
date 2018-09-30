module Main exposing (Model, Msg(..), getRandomGif, gifDecoder, init, main, subscriptions, toGiphyUrl, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Task
import Time
import Url.Builder as Url



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
    { topic : String
    , url : String
    , errorMessage : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "cat" "waiting.gif" ""
    , getRandomGif "cat"
    )



-- UPDATE


type Msg
    = MorePlease
    | ChangeTopic String
    | NewGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model
            , getRandomGif model.topic
            )

        ChangeTopic newTopic ->
            ( { model | topic = newTopic }
            , Task.perform (always MorePlease) Time.now
            )

        NewGif result ->
            case result of
                Ok newUrl ->
                    ( { model | url = newUrl, errorMessage = "" }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | errorMessage = "There was an error with Giphy API" }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , input [ type_ "text", placeholder "Enter a funny topic", value model.topic, onInput ChangeTopic ] []
        , p [] [ text model.errorMessage ]
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , img [ src model.url ] []
        ]



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    Http.send NewGif (Http.get (toGiphyUrl topic) gifDecoder)


toGiphyUrl : String -> String
toGiphyUrl topic =
    Url.crossOrigin "https://api.giphy.com"
        [ "v1", "gifs", "random" ]
        [ Url.string "api_key" "dc6zaTOxFJmzC"
        , Url.string "tag" topic
        ]


gifDecoder : Decode.Decoder String
gifDecoder =
    Decode.field "data" (Decode.field "image_url" Decode.string)
