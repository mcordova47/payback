module Transaction exposing (..)

import Dict exposing (Dict)


type alias Transaction =
    { payFrom : Maybe String
    , amount : Float
    , transDate : String
    , postDate : String
    , description : String
    , transType : String
    }


fromDict : (Dict String String) -> Maybe Transaction
fromDict dict =
    let
        amount =
            dict
                |> Dict.get "Amount"
                |> Maybe.andThen (Result.toMaybe << String.toFloat)
    in
        Maybe.map5
            (Transaction Nothing)
            amount
            (Dict.get "Trans Date" dict)
            (Dict.get "Post Date" dict)
            (Dict.get "Description" dict)
            (Dict.get "Type" dict)


setPayFrom : Maybe String -> Transaction -> Transaction
setPayFrom value transaction =
    { transaction | payFrom = value }
