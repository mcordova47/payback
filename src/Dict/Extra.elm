module Dict.Extra exposing (..)

import Dict exposing (Dict)


oneOf : List comparable -> Dict comparable a -> Maybe a
oneOf keys dict =
    case keys of
        [] ->
            Nothing

        key :: restKeys ->
            case Dict.get key dict of
                Nothing ->
                    oneOf restKeys dict

                value ->
                    value
