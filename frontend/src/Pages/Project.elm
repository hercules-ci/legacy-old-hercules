module Pages.Project exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe
import List
import Material.Button as Button
import Material.Options as Options
import Material.Elevation as Elevation
import Material.Menu as Menu
import Material.Table as Table
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Components.LiveSearch exposing (search)
import Components.Help exposing (..)
import Msg exposing (..)
import Models exposing (Project, AppModel)
import Urls exposing (..)
import Utils exposing (..)


view : AppModel -> Page -> List (Html Msg)
view model page =
    case page of
        Home ->
            projectsView model model.projects

        Project name ->
            case List.head (List.filter (\p -> p.name == name) model.projects) of
                Just project ->
                    [ renderProject model 0 project ]

                Nothing ->
                    render404 ("Project " ++ name ++ " does not exist.")

        NewProject ->
            newProjectView model

        -- TODO: get rid of this
        _ ->
            []


projectsView : AppModel -> List Project -> List (Html Msg)
projectsView model projects =
    let
        mdlCtx = { model = model.mdl, msg = Mdl }
        newprojects =
            List.indexedMap (renderProject model) (search projects)
    in
        renderHeader mdlCtx (defaultHeader "Projects" |> createButton (NewPage NewProject))
            ++ if List.isEmpty newprojects then
                render404 "Zero projects. Maybe add one?"
               else
                newprojects


newProjectView : AppModel -> List (Html Msg)
newProjectView model =
    let
        mdlCtx = { model = model.mdl, msg = Mdl }
    in renderHeader mdlCtx (defaultHeader "Add a new project")
        ++ [ Html.form []
                [ Textfield.render Mdl
                    [ 5 ]
                    model.mdl
                    [ Textfield.label "Identifier (e.g. hydra)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Options.css "display" "block"
                    ]
                , Textfield.render Mdl
                    [ 6 ]
                    model.mdl
                    [ Textfield.label "Display name (e.g. Hydra)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Options.css "display" "block"
                    ]
                , Textfield.render Mdl
                    [ 7 ]
                    model.mdl
                    [ Textfield.label "Description (e.g. Builds Nix expressions and provides insight about the process)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Options.css "display" "block"
                    ]
                , Textfield.render Mdl
                    [ 8 ]
                    model.mdl
                    [ Textfield.label "URL (e.g. https://github.com/NixOS/hydra)"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Options.css "display" "block"
                    ]
                ]
           , Toggles.checkbox Mdl
                [ 9 ]
                model.mdl
                [ Toggles.ripple
                  --, Toggles.onClick MyToggleMsg
                ]
                [ text "Is visible on the project list?" ]
           , Toggles.checkbox Mdl
                [ 10 ]
                model.mdl
                [ Toggles.ripple
                  --, Toggles.onClick MyToggleMsg
                ]
                [ text "Is enabled?" ]
           , Textfield.render Mdl
                [ 11 ]
                model.mdl
                [ Textfield.label "Owner"
                , Textfield.floatingLabel
                , Textfield.text_
                , Options.css "display" "block"
                , Textfield.value (Maybe.withDefault "" (Maybe.map (\u -> u.id) model.user))
                ]
           , Button.render Mdl
                [ 12 ]
                model.mdl
                [ Button.raised
                , Button.colored
                  --, Button.onClick MyClickMsg
                ]
                [ text "Create project" ]
           ]


renderProject : AppModel -> Int -> Project -> Html Msg
renderProject model i project =
    Options.div
        [ Elevation.e2
        , Options.css "margin" "30px"
        , Options.css "padding" "8px"
        ]
        [ h3
            []
            [ a (onClickPage (Urls.Project project.name))
                [ Options.span
                    [ Options.css "margin" "16px" ]
                    [ text (project.name) ]
                ]
            , small
                [ class "hidden-xs" ]
                [ text ("(" ++ project.description ++ ")") ]
              -- TODO: correct index
            , Menu.render Mdl
                [ i + 10 ]
                model.mdl
                [ Menu.ripple
                , Menu.bottomRight
                , Options.css "float" "right"
                ]
                [ Menu.item []
                    [ menuIcon "add"
                    , text "Add a jobset"
                    ]
                , Menu.item []
                    [ menuIcon "settings"
                    , text "Configuration"
                    ]
                , Menu.item []
                    [ menuIcon "delete"
                    , text "Delete the project"
                    ]
                ]
            ]
        , if List.isEmpty project.jobsets then
            Options.span
                [ Options.center
                , Options.css "margin" "30px"
                ]
                [ text "No Jobsets configured yet." ]
          else
            Table.table [ Options.css "width" "100%" ]
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [] [ text "Jobset", jobsetHelp model ]
                        , Table.th [] [ text "Description" ]
                        , Table.th [] [ text "Job status" ]
                        , Table.th [] [ text "Last evaluation" ]
                        ]
                    ]
                , Table.tbody []
                    (search project.jobsets
                        |> List.map
                            (\jobset ->
                                Table.tr []
                                    [ Table.td []
                                        [ a
                                            (onClickPage (Urls.Jobset project.name jobset.id))
                                            [ text jobset.name ]
                                        ]
                                    , Table.td [] [ text jobset.description ]
                                    , Table.td [] (statusLabels jobset.succeeded jobset.failed jobset.queued)
                                    , Table.td [] [ text jobset.lastEvaluation ]
                                    ]
                            )
                    )
                ]
        ]
