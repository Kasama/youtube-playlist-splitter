port module Ports exposing (authStateChanged, nativeError, signIn, signOut)

import Json.Decode exposing (Value)


port nativeError : (Value -> msg) -> Sub msg


port authStateChanged : (Value -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg
