port module Update exposing (..)

import Material
import Navigation
import Models exposing (..)
import Msg exposing (..)
import Components.LiveSearch as LiveSearch
import Urls exposing (..)


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of
        Mdl msg' ->
              Material.update msg' model

        FetchSucceed init ->
            ( model, Cmd.none )

        FetchFail msg ->
            ( model, Cmd.none )

        LoginUserClick loginType ->
            let
                -- TODO: well, actually do the login proceedure
                user =
                    { id = "domenkozar"
                    , name = "Domen Kožar"
                    , email = "domen@dev.si"
                    , roles = []
                    , recieveEvaluationErrors = False
                    }
            in
                case loginType of
                    Hydra ->
                        ( { model | user = Just user }, Cmd.none )

                    Google ->
                        ( { model | user = Just user }, Cmd.none )

        LogoutUserClick ->
            -- TODO: well, should we cleanup something?
            ( { model | user = Nothing }, Cmd.none )

        PreferencesClick ->
            ( model, Cmd.none )

        LiveSearchMsg searchmsg ->
            let
                ( newmodel, cmds ) =
                    LiveSearch.update searchmsg model
            in
                ( newmodel, Cmd.map LiveSearchMsg cmds )

        NewPage page ->
            ( model, Navigation.newUrl (pageToURL page) )

        ClickCreateProject ->
            -- TODO: http
            ( model, Cmd.none )


urlUpdate : Result String Page -> AppModel -> ( AppModel, Cmd b )
urlUpdate result model =
    case result of
        Err msg ->
            let
                msg =
                    (Debug.log "urlUpdate:" msg)

                alert =
                    { kind = Danger
                    , msg = "Given URL returned 404."
                    }
            in
                { model | alert = Just alert } ! []

        Ok page ->
            ( { model
                | currentPage = page
                , alert = Nothing
              }
            , title (pageToTitle page)
            )



-- Ports


port title : String -> Cmd msg
