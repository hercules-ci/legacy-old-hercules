module Msg exposing (..)

import Material
import Http
import Navigation
import Components.LiveSearch as LiveSearch
import Urls exposing (Page)
import Pages.Login as Login


type Msg
    = Mdl (Material.Msg Msg)
    | FetchSucceed String
    | FetchFail Http.Error
    | LoginUserClick
    | LoginMsg Login.Msg
    | LogoutUserClick
    | PreferencesClick
    | LiveSearchMsg LiveSearch.Msg
    | NewPage Page
    | ClickCreateProject
    | UrlChange Navigation.Location
