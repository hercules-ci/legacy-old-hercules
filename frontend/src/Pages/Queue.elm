module Pages.Queue exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models exposing (..)
import Msg exposing (Msg)
import Page exposing (..)


queueView : AppModel -> List (Html Msg)
queueView jobset =
    []
