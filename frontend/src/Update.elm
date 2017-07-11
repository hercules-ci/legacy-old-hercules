port module Update exposing (..)

import Material
import Navigation
import Models exposing (..)
import Msg exposing (..)
import Components.LiveSearch as LiveSearch
import Urls exposing (..)
import UrlParser exposing (parsePath)
import Pages.Login as Login


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case (msg, page) of
        (Mdl msg_, _) ->
            Material.update msg_ model

        (FetchSucceed init, _) ->
            ( model, Cmd.none )

        (FetchFail msg, _) ->
            ( model, Cmd.none )

        (LoginUserClick, _) ->
            ( model, Navigation.newUrl Login)

        (LoginMsg subMsg, Login subModel) ->
            let
              ((pageModel, cmd), msgFromPage) = Login.update subMsg subModel
              newModel = case msgFromPage of
                Login.NoOp -> model
            in
              ( newModel, Cmd.map LoginMsg cmd )

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

        (NewPage page, _) ->
            ( model, Navigation.newUrl (pageToURL page) )

        (ClickCreateProject, _) ->
            -- TODO: http
            ( model, Cmd.none )

        (UrlChange location, _) ->
            let
                page = Maybe.withDefault Home (parsePath pageParser location)
            in
            ( { model | currentPage = page }
            , title (pageToTitle page)
            )

-- Ports


port title : String -> Cmd msg
