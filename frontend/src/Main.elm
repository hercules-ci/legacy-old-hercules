module Main exposing (..)

import Maybe
import Material
import Navigation
import UrlParser exposing (parsePath)
import Hercules exposing (..)
import Msg exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)
import Urls exposing (..)


init : Navigation.Location -> ( AppModel, Cmd Msg )
init location =
    let
        page = Maybe.withDefault Home (parsePath pageParser location)
        model = initialModel page
    in model ! [ Material.init Mdl
               , title (pageToTitle page)
               ]


main : Program Never AppModel Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = Material.subscriptions Mdl
        }
