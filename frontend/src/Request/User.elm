module Request.User exposing (login)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Utils exposing ((=>))

login : String -> { r | username: String, password: String } -> Http.Request ()
login apiUrl {username, password} =
  let
    user = Encode.object
             [ "username" => Encode.string username
              , "password" => Encode.string password
              ]

    body = Encode.object
             [ "user" => user ]
             |> Http.jsonBody
  in
    Decode.succeed ()
      |> Http.post (apiUrl ++ "/users/login") body
