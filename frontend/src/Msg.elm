module Msg exposing (..)

import Material
import Http
import Components.LiveSearch as LiveSearch
import Route exposing (..)
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
    | ClickCreateProject
    | GotoRoute Route

    | UnsafeSetRoute (Maybe Route)
