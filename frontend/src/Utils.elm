module Utils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Material
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Color as Color
import Material.Icon as Icon
import Material.Options as Options
import Msg exposing (..)
import Urls exposing (..)


menuIcon : String -> Html Msg
menuIcon name =
    Icon.view name [ Options.css "width" "40px" ]


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click" { defaultOptions | preventDefault = True } (Json.succeed message)


onClickPage : Page -> List (Attribute Msg)
onClickPage page =
    [ style [ ( "pointer", "cursor" ) ]
    , href (pageToURL page)
    , onPreventDefaultClick (NewPage page)
    ]


optionalTag : Bool -> Html Msg -> Html Msg
optionalTag doInclude html =
    if doInclude then
        html
    else
        text ""


statusLabels : Int -> Int -> Int -> List (Html Msg)
statusLabels succeeded failed queued =
    [ optionalTag (succeeded > 0)
        (badge
            (Color.color Color.Green Color.S500)
            [ Options.attribute <| title "Jobs succeeded" ]
            [ text (toString succeeded) ]
        )
    , optionalTag (failed > 0)
        (badge
            (Color.color Color.Red Color.S500)
            [ Options.attribute <| title "Jobs failed" ]
            [ text (toString failed) ]
        )
    , optionalTag (queued > 0)
        (badge
            (Color.color Color.Grey Color.S500)
            [ Options.attribute <| title "Jobs in queue" ]
            [ text (toString queued) ]
        )
    ]


badge : Color.Color -> List (Options.Property c Msg) -> List (Html Msg) -> Html Msg
badge color properties content =
    Options.span
        ([ Options.css "border-radius" "9px"
         , Options.css "padding" "3px 5px"
         , Options.css "line-height" "14px"
         , Options.css "white-space" "nowrap"
         , Options.css "font-weight" "bold"
         , Options.css "font-size" "12px"
         , Options.css "margin" "0 3px"
         , Options.css "cursor" "help"
         , Options.css "color"
            (if color == Color.white then
                "#000"
             else
                "#FFF"
            )
         , Color.background color
         ]
            ++ properties
        )
        content


whiteBadge : List (Options.Property c Msg) -> List (Html Msg) -> Html Msg
whiteBadge properties content =
    badge Color.white properties content


render404 : String -> List (Html Msg)
render404 reason =
    [ Options.div
        [ Elevation.e2
        , Options.css "padding" "40px"
        , Options.center
        ]
        [ text reason ]
    ]

type alias Header msg =
  { title : String
  , subtitle : Maybe String
  , createMsg : Maybe msg -- ^ Show create button sending given msg on click
  }

type alias MdlCtx msg =
  { model : Material.Model
  , msg : Material.Msg msg -> msg
  }

defaultHeader : String -> Header msg
defaultHeader title = { title = title, subtitle = Nothing, createMsg = Nothing }

subtitle : String -> Header msg -> Header msg
subtitle s h = { h | subtitle = Just s }

createButton : msg -> Header msg -> Header msg
createButton msg h = { h | createMsg = Just msg }

renderHeader : MdlCtx msg -> Header msg -> List (Html msg)
renderHeader mdlCtx h =
  let
    subtitleHtml = Maybe.map (
      small [
        style [ ( "margin-left", "10px" ) ]
      ] << List.singleton << text)
      h.subtitle

    pageHtml = Maybe.map (\p ->
      Button.render mdlCtx.msg
        [2]
        mdlCtx.model
        [ Button.fab
        , Button.colored
        , Button.onClick p
        , Options.css "margin-left" "20px"
        ]
        [ Icon.i "add" ]
      ) h.createMsg
  in
    [ h1
        [ style [ ( "margin-bottom", "30px" ) ] ]
        ([ text h.title ] ++ catMaybes [subtitleHtml, pageHtml])
    ]
  

catMaybes : List (Maybe a) -> List a
catMaybes = List.filterMap identity
