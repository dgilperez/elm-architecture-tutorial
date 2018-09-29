import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Char exposing (isDigit, isUpper, isLower, isAlphaNum)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type Validation =
  Blank |
  Ok |
  Ko String

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , valid : Validation
  }

init : Model
init =
  Model "" "" "" Blank

validationSetError : String -> Model -> Model
validationSetError errorMessage model =
  let
    errorText =
      case model.valid of
        Ko message ->
          if String.contains errorMessage message then
            message
          else
            message ++ ", " ++ errorMessage
        _ -> errorMessage
  in
    { model | valid = Ko errorText }

validationRemoveError : String -> Model -> Model
validationRemoveError errorMessage model =
  case model.valid of
    Ko message ->
      if onlyErrorMessage message errorMessage then
        { model | valid = Ok }
      else
        let
          invalid = message
                    |> String.replace (errorMessage ++ ",") ""
                    |> String.replace errorMessage ""
        in
          { model | valid = Ko invalid }
    _ -> { model | valid = Ok }

onlyErrorMessage : String -> String -> Bool
onlyErrorMessage text message =
  let
    prefix = errorMessagePrefix |> String.trim
  in
    text
    |> String.replace prefix ""
    |> String.replace message ""
    |> String.trim
    |> String.isEmpty

errorMessagePrefix = "Please check errors: "

validatePasswordLength : Model -> Model
validatePasswordLength model =
  let
    errorMessage = "Password should be at least 8 characters long"
  in
    if String.length model.password < 8 then
      model |> validationSetError(errorMessage)
    else
      model |> validationRemoveError(errorMessage)

validatePasswordChars : Model -> Model
validatePasswordChars model =
  let
    errorMessage = "Password does not contain uppcase, lowercase, numbers and symbols"
  in
    if String.any isDigit model.password &&
      String.any isLower model.password &&
      String.any isUpper model.password &&
      not(String.all isAlphaNum model.password) then

      model |> validationRemoveError(errorMessage)
    else
      model |> validationSetError(errorMessage)

validatePasswordMatch : Model -> Model
validatePasswordMatch model =
  let
    errorMessage = "Password confirmation does not match password"
  in
    if model.password /= model.passwordAgain then
      model |> validationSetError(errorMessage)
    else
      model |> validationRemoveError(errorMessage)

-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }
      |> validatePasswordLength
      |> validatePasswordChars
      |> validatePasswordMatch

    PasswordAgain password ->
      { model | passwordAgain = password }
      |> validatePasswordMatch

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "text" "Password" model.password Password
    , viewInput "text" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      case model.valid of
        Ok -> ("green", "Good to go!")
        Ko errorMessage -> ("red", errorMessagePrefix ++ errorMessage)
        Blank -> ("black", "Please enter your name and password")
  in
    div [ style "color" color ] [ text message ]
