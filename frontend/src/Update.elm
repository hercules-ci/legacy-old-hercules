module Update exposing (..)

import Debug
import Material
import Models exposing (..)
import Msg exposing (..)
import Components.LiveSearch as LiveSearch
import Route exposing (..)
import Pages.Login as Login
import Utils exposing ((=>))

update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg, model.currentPage) of
        (Mdl msg_, _) ->
            Material.update msg_ model

        (FetchSucceed init, _) ->
            ( model, Cmd.none )

        (FetchFail msg, _) ->
            ( model, Cmd.none )

        (LoginUserClick, _) ->
            ( model, Route.modifyUrl Login)

        (LoginMsg subMsg, LoginPage subModel) ->
            let
              ((pageModel, cmd), msgFromPage) = Login.update model.appEnv subMsg subModel
              newModel = case msgFromPage of
                Login.NoOp -> model
            in
              { newModel | currentPage = LoginPage pageModel }
                => Cmd.map LoginMsg cmd

        (LogoutUserClick, _) ->
            -- TODO: well, should we cleanup something?
            ( { model | user = Nothing }, Cmd.none )

        (PreferencesClick, _) ->
            ( model, Cmd.none )

        (LiveSearchMsg searchmsg, _) ->
            let
                ( newmodel, cmds ) =
                    LiveSearch.update searchmsg model
            in
                ( newmodel, Cmd.map LiveSearchMsg cmds )

        (GotoRoute route, _) ->
          (model, Route.modifyUrl route)

        (UnsafeSetRoute route, _) ->
            setRoute route model

        (ClickCreateProject, _) ->
            -- TODO: http
            ( model, Cmd.none )

        ( _, _ ) ->
          -- Disregard incoming messages that arrived for the wrong page
          model => Cmd.none

setRoute : Maybe Route -> AppModel -> ( AppModel, Cmd Msg)
setRoute route model =
  case route of

    Nothing -> { model | currentPage = HomePage } => Cmd.none

    Just Home -> { model | currentPage = HomePage } => Cmd.none

    Just Login -> { model | currentPage = LoginPage Login.initialModel } => Cmd.none

    Just (Project p) -> { model | currentPage = ProjectPage p } => Cmd.none
    
    Just NewProject -> { model | currentPage = NewProjectPage } => Cmd.none

    Just (Jobset a b) -> { model | currentPage = JobsetPage2 a b } => Cmd.none
