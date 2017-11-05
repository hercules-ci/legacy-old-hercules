module Main exposing (..)

import Maybe
import Material
import Navigation
import UrlParser exposing (parsePath)
import Msg exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)
import Route exposing (..)
import Ports


init : Flags -> Navigation.Location -> ( AppModel, Cmd Msg )
init flags location =
    let
        route = Maybe.withDefault Home (parsePath routeParser location)
        model = initialModel route flags
    in model ! [ Material.init Mdl
               , Ports.title (routeToTitle route)
               ]


main : Program Flags AppModel Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> UnsafeSetRoute)
        { init = init
        , update = update
        , view = view
        , subscriptions = Material.subscriptions Mdl
        }
