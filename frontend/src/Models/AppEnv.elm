-- | The app environment containing global state like backendUrl
module Models.AppEnv exposing (..)

type alias AppEnv =
  { backendURL : String
  }
