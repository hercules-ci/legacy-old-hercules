module Components.Breadcrumbs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Material.Icon as Icon
import Material.Options as Options
import Msg exposing (..)
import Models exposing (..)
import Route as Route exposing (..)
import Utils exposing (onClickPage)


type alias Breadcrumb =
    { name : String
    , page : Maybe Route
    }


renderBreadcrumbs : List Breadcrumb -> List (Html Msg)
renderBreadcrumbs breadcrumbs =
    let
        home =
            a (onClickPage GotoRoute Home) [ text "Hydra" ]

        render breadcrumb =
            case breadcrumb.page of
                Just page ->
                    a (onClickPage GotoRoute page)
                        [ text breadcrumb.name ]

                Nothing ->
                    span [ class "active" ]
                        [ text breadcrumb.name ]
    in
        List.intersperse
            (Icon.view "keyboard_arrow_right"
                [ Icon.size36
                , Options.css "top" "10px"
                , Options.css "position" "relative"
                ]
            )
            (home :: List.map render breadcrumbs)


breadCrumbs : AppModel -> List (Html Msg)
breadCrumbs model =
    let
        breadcrumbs =
            case model.currentPage of
                HomePage ->
                    []

                LoginPage _ ->
                    [ Breadcrumb "Login" Nothing ]

                NewProjectPage ->
                    [ Breadcrumb "New Project" Nothing ]

                ProjectPage project ->
                    [ Breadcrumb project Nothing ]

                JobsetPage2 project jobset ->
                    [ Breadcrumb project (Just (Route.Project project))
                    , Breadcrumb jobset Nothing
                    ]
    in
        renderBreadcrumbs breadcrumbs
