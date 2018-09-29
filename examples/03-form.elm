import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Char exposing (isDigit, isUpper, isLower, isAlphaNum)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""

validatePasswordLength : Model -> (Bool, String)
validatePasswordLength model =
  if String.length model.password < 8 then
    (False, "Password should be at least 8 characters long")
  else
    (True, model.password |> String.length |> String.fromInt)

validatePasswordChars : Model -> (Bool, String)
validatePasswordChars model =
  if String.any isDigit model.password &&
    String.any isLower model.password &&
    String.any isUpper model.password &&
    not(String.all isAlphaNum model.password) then
    (True, "match")
  else
    (False, model.password ++ " does not contain ...")

validatePasswordMatch : Model -> (Bool, String)
validatePasswordMatch model =
  if model.password /= model.passwordAgain then
    (False, "Passwords do not match!")
  else
    (True, "do match")

validations model = [
    validatePasswordLength model,
    validatePasswordChars model,
    validatePasswordMatch model
  ]

isValid model = validations model |> List.all Tuple.first


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

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidationErrorText model =
  validations model
    |> List.filter (\n -> not(Tuple.first n))
    |> List.map Tuple.second
    |> String.join ","

viewValidation : Model -> Html msg
viewValidation model =
  if isValid model then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text(viewValidationErrorText model) ]
