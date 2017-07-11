module Pages.Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Material
import Material.Button as Button
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Menu as Menu
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Utils exposing (..)

type alias Model =
  { username : String
  , password : String
  , mdl : Material.Model
  }

type Msg
  = SetUsername String
  | SetPassword String
  | Submit
--  | LoginCompleted (Result Http.Error User)
  | Mdl (Material.Msg Msg)

type ExternalMsg
  = NoOp

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
          , Button.onClick Submit
          ]
          [ text "Login" ]
        ]
      ]

update : Msg -> Model -> (( Model, Cmd Msg ), ExternalMsg)
update msg model =
  case msg of
    Submit -> Debug.crash "TODO"
    SetUsername s ->
      { model | username = s }
        => Cmd.none
        => NoOp
    SetPassword s ->
      { model | password = s }
        => Cmd.none
        => NoOp
    Mdl msg_ ->
      Material.update msg_ model
        => NoOp
