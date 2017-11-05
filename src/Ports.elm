port module Ports exposing (..)

port upload : String -> Cmd msg

port readFile : (String -> msg) -> Sub msg  -- TODO: make this a Value -> Sub msg

