port module Ports exposing (..)

import Json.Decode exposing (Value)


port upload : Value -> Cmd msg

port readFile : (String -> msg) -> Sub msg  -- TODO: make this a Value -> Sub msg

