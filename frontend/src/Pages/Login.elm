module Pages.Login exposing (..)

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional)
import Material
import Material.Button as Button
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Menu as Menu
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Request.User
import Route
import Utils exposing (..)

-- dummy type for now
type alias User = ()

type alias Model =
  { errors : List String
  , username : String
  , password : String
  , mdl : Material.Model
  }

type Msg
  = SetUsername String
  | SetPassword String
  | Submit
  | LoginCompleted (Result Http.Error User)
  | Mdl (Material.Msg Msg)

type ExternalMsg
  = NoOp

initialModel : Model
initialModel = { errors = [], username = "", password = "", mdl = Material.model }

view : Model -> List (Html Msg)
view model =
  let
    mdlCtx = { model = model.mdl, msg = Mdl }
  in renderHeader mdlCtx (defaultHeader "Login")
    ++ [ Html.form []
        [ Textfield.render Mdl
          [5]
          model.mdl
          [ Textfield.label "Username"
          , Textfield.floatingLabel
          , Textfield.text_
          , Textfield.onInput SetUsername
          , Options.css "display" "block" ]
        , Textfield.render Mdl
          [6]
          model.mdl
          [ Textfield.label "Password"
          , Textfield.floatingLabel
          , Textfield.text_
          , Textfield.password
          , Textfield.onInput SetPassword
          , Options.css "display" "block" ]
        , Button.render Mdl
          [7]
          model.mdl
          [ Button.raised
          , Button.colored
--        , Button.onClick (SetUsername "")
          ]
          [ text "Login" ]
        ]
      ]

update : Msg -> Model -> (( Model, Cmd Msg ), ExternalMsg)
update msg model =
  case msg of
    Submit ->
      -- TODO validation
      Debug.log "Submit:" model
--        => Http.send LoginCompleted (Request.User.login "http://localhost:8080" model)
        => Cmd.none
        => NoOp

    SetUsername s ->
      { model | username = s }
        => Cmd.none
        => NoOp
    SetPassword s ->
      { model | password = s }
        => Cmd.none
        => NoOp
    LoginCompleted (Err error) ->
      model => Cmd.none => NoOp
{-      let
        errorMessages = case error of
          Http.BadStatus response ->
            response.body
              |> decodeString (field "errors" errorsDecoder)
              |> Result.withDefault []

          _ -> [ "unable to process registration" ]
      in
        { model | errors = List.map (\errorMessage -> Form => errorMessage) errorMessages }
          => Cmd.none
          => NoOp-}

    LoginCompleted (Ok user) ->
      Debug.log "BLA" model
        => Cmd.batch [ Route.modifyUrl Route.Home]
        => NoOp
    Mdl msg_ ->
      Material.update msg_ model
        => NoOp


errorsDecoder : Decoder (List String)
errorsDecoder =
  decode (\email username password -> List.concat [ email, username, password ])
    |> optionalError "email"
    |> optionalError "username"
    |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
  let
    errorToString errorMessage = String.join " " [ fieldName, errorMessage ]
  in
    optional fieldName (Decode.list (Decode.map errorToString string)) []
