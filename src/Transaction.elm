module Transaction exposing (..)

import Dict exposing (Dict)
import Dict.Extra as Dict


type alias Transaction =
    { payFrom : Maybe String
    , transType : String
    , amount : Float
    , transDate : String
    , postDate : String
    , description : String
    }


fromDict : Dict String String -> Maybe Transaction
fromDict dict =
    let
        amount =
            dict
                |> Dict.oneOf [ "Amount", "Credit" ]
                |> Maybe.andThen (Result.toMaybe << String.toFloat)
                |> Maybe.map negate
    in
        Maybe.map4
            (Transaction Nothing (getType dict))
            amount
            (Dict.oneOf [ "Trans Date", "Transaction Date" ] dict)
            (Dict.oneOf [ "Post Date", "Posted Date" ] dict)
            (Dict.get "Description" dict)


setPayFrom : Maybe String -> Transaction -> Transaction
setPayFrom value transaction =
    { transaction | payFrom = value }


getType : Dict String String -> String
getType dict =
    dict
        |> Dict.get "Type"
        |> Maybe.withDefault "Purchase"
