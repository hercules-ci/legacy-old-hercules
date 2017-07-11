module Components.Navbar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe
import Material.Layout as Layout
import Material.Icon as Icon
import Material.Menu as Menu
import Material.Options as Options
import Msg exposing (..)
import Models exposing (AppModel)
import Components.Breadcrumbs exposing (breadCrumbs)
import Components.LiveSearch as LiveSearch
import Urls exposing (..)
import Utils exposing (..)


tabs : AppModel -> List (Html Msg)
tabs model =
    [ span [] [ whiteBadge [] [ text (toString model.queueStats.numBuilding) ], text " in progress" ]
    , span [] [ whiteBadge [] [ text (toString model.queueStats.numWaiting) ], text " in queue" ]
    , span [] [ whiteBadge [] [ text (toString model.queueStats.numMachines) ], text " machines" ]
    , text "evaluations"
    , text "builds"
    , text "steps"
    ]


view : AppModel -> List (Html Msg)
view model =
    let
        menuItems =
            case model.user of
                Nothing ->
                    [ Menu.item
                        [ Menu.onSelect <| LoginUserClick ]
                        [ menuIcon "input"
                        , text "Sign in"
                        ]
                    ]

                Just user ->
                    [ Menu.item
                        [ Menu.onSelect <| PreferencesClick ]
                        [ menuIcon "settings"
                        , text "Preferences"
                        ]
                    , Menu.item
                        [ Menu.onSelect <| LogoutUserClick ]
                        [ Icon.view "power_settings_new"
                            [ Options.css "width" "40px"
                            , Options.css "color" "red"
                            ]
                        , text "Sign out"
                        ]
                    ]
    in
        [ Layout.row []
            [ Layout.title
                []
                ([ if model.hydraConfig.logo == "" then
                    text ""
                   else
                    img
                        ([ src model.hydraConfig.logo
                         , alt "Hydra Logo"
                         , class "logo"
                         , style [ ( "height", "37px" ), ( "margin", "5px" ) ]
                         ]
                            ++ (onClickPage NewPage Home)
                        )
                        []
                 ]
                    ++ (breadCrumbs model)
                )
            , Layout.spacer
            , Layout.navigation []
                [ Html.map LiveSearchMsg (LiveSearch.view model)
                , span [] (Maybe.withDefault [] (Maybe.map (\user -> [ text user.name ]) model.user))
                , Menu.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Menu.ripple
                    , Menu.bottomRight
                    , Menu.icon "account_circle"
                    ]
                    menuItems
                ]
            ]
        ]
