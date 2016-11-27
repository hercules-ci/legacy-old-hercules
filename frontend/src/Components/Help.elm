module Components.Help exposing (..)

import Html exposing (..)
import Material.Icon as Icon
import Material.Options as Options
import Material.Tooltip as Tooltip

import Msg exposing (..)
import Models exposing (..)


{-| Uses ports to communicate with jQuery and initialize twitter
    bootstrap popover plugin. Each help element is a questionmark
    icon which on popover shows some help text.
-}
popoverHelp : AppModel -> List (Html Msg) -> Html Msg
popoverHelp model html =
   span
     []
     [ Icon.view "help"
         [ Options.css "margin" "0 6px"
         , Options.css "color" "#0088CC"
         , Options.css "cursor" "help"
         , Tooltip.attach Mdl [3]
         ]
     , Tooltip.render Mdl [3] model.mdl
        [ Tooltip.large ]
        html
     ]

projectHelp : AppModel -> Html Msg
projectHelp model =
  popoverHelp model [ text "TODO" ]

jobsetHelp : AppModel -> Html Msg
jobsetHelp model =
  popoverHelp
    model
    [ text "Jobsets evaluate a Nix expression and provide an overview of successful/failed builds." ]


evaluationHelp : AppModel -> Html Msg
evaluationHelp model =
    popoverHelp model [ text "" ]


buildHelp : AppModel -> Html Msg
buildHelp model =
    popoverHelp model [ text "" ]


buildStepHelp : AppModel -> Html Msg
buildStepHelp model =
    popoverHelp model [ text ""]
