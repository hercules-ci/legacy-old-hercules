module Components.Breadcrumbs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Material.Icon as Icon
import Material.Options as Options
import Msg exposing (..)
import Models exposing (..)
import Urls as Urls exposing (..)
import Utils exposing (onClickPage)


type alias Breadcrumb =
    { name : String
    , page : Maybe Page
    }


renderBreadcrumbs : List Breadcrumb -> List (Html Msg)
renderBreadcrumbs breadcrumbs =
    let
        home =
            a (onClickPage NewPage Home) [ text "Hydra" ]

        render breadcrumb =
            case breadcrumb.page of
                Just page ->
                    a (onClickPage NewPage page)
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
                Home ->
                    []

                Login ->
                    [ Breadcrumb "Login" Nothing ]

                NewProject ->
                    [ Breadcrumb "New Project" Nothing ]

                Project project ->
                    [ Breadcrumb project Nothing ]

                Jobset project jobset ->
                    [ Breadcrumb project (Just (Urls.Project project))
                    , Breadcrumb jobset Nothing
                    ]
    in
        renderBreadcrumbs breadcrumbs
