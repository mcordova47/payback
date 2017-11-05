module List.Extra exposing (..)

takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list =
    case list of
        [] ->
            []

        x :: xs ->
            if p x then
                x :: (takeWhile p xs)
            else
                []
