module Urls exposing (..)

import Debug
import Navigation
import String
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)


{-| Main type representing current url/page
-}
type Page
    = Home
    | Project String
    | NewProject
    | Jobset String String


urlParser : Navigation.Location -> Result String Page
urlParser location =
    String.dropLeft 1 location.pathname
        |> Debug.log "pathname"
        |> UrlParser.parse identity pageParser


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format Home (s "")
        , format Project (s "project" </> string)
        , format NewProject (s "create-project")
        , format Jobset (s "jobset" </> string </> string)
        ]


pageToURL : Page -> String
pageToURL page =
    case page of
        Home ->
            "/"

        Project name ->
            "/project/" ++ name

        NewProject ->
            "/create-project"

        Jobset project jobset ->
            "/jobset/" ++ project ++ "/" ++ jobset


pageToTitle : Page -> String
pageToTitle page =
    case page of
        Home ->
            "Projects"

        Project name ->
            "Project " ++ name

        NewProject ->
            "New Project"

        Jobset project jobset ->
            "Jobset " ++ jobset ++ " of project " ++ project
