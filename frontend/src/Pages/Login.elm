module Pages.Login exposing (..)

import Debug
import Html exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional)
import Material
import Material.Button as Button
import Material.Options as Options
import Material.Textfield as Textfield
import Models.AppEnv exposing (..)
import OAuth
import OAuth.Password as OPassword
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
  | LoginAnswer (Result Http.Error OAuth.ResponseToken)
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
        ]
      , Button.render Mdl
          [7]
          model.mdl
          [ Button.raised
          , Button.colored
        , Button.onClick Submit
          ]
          [ text "Login" ]
      ]

update : AppEnv -> Msg -> Model -> (( Model, Cmd Msg ), ExternalMsg)
update env msg model =
  case msg of
    Submit ->
      -- TODO validation
      model
        => Http.send LoginAnswer
          (OPassword.authenticate (OAuth.Password
          { credentials = Nothing
          , scope = []
          , state = Nothing
          , username = model.username
          , password = model.password
          , url = env.backendURL ++ "/login"
          }))
        => NoOp

    SetUsername s ->
      { model | username = s }
        => Cmd.none
        => NoOp
    SetPassword s ->
      { model | password = s }
        => Cmd.none
        => NoOp
    LoginAnswer (Err error) ->
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

    LoginAnswer (Ok user) ->
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
