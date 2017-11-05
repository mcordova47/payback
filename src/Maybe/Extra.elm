module Maybe.Extra exposing (..)

import Maybe exposing (Maybe)

andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap maybeA maybeFn =
    case ( maybeFn, maybeA ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just fn, Just a ) ->
            Just (fn a)
