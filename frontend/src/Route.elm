module Route exposing (..)

import Navigation exposing (Location)
import String
import UrlParser exposing (Parser, (</>), map, int, oneOf, s, string, parsePath)


{-| Main type representing current url/route
-}
type Route
    = Home
    | Login
    | Project String
    | NewProject
    | Jobset String String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home (s "")
        , map Login (s "login")
        , map Project (s "project" </> string)
        , map NewProject (s "create-project")
        , map Jobset (s "jobset" </> string </> string)
        ]


routeToURL : Route -> String
routeToURL route =
    case route of
        Home ->
            "/"

        Login ->
            "/login"

        Project name ->
            "/project/" ++ name

        NewProject ->
            "/create-project"

        Jobset project jobset ->
            "/jobset/" ++ project ++ "/" ++ jobset


routeToTitle : Route -> String
routeToTitle route =
    case route of
        Home ->
            "Projects"

        Login ->
            "Login"

        Project name ->
            "Project " ++ name

        NewProject ->
            "New Project"

        Jobset project jobset ->
            "Jobset " ++ jobset ++ " of project " ++ project

modifyUrl : Route -> Cmd msg
modifyUrl = routeToURL >> Navigation.modifyUrl

fromLocation : Location -> Maybe Route
fromLocation location =
  if String.isEmpty location.pathname then
    Just Home
  else
    parsePath routeParser location
