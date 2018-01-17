module Transaction exposing (..)

import Quicken
import Regex exposing (Regex)


type alias Transaction =
    { payFrom : Maybe String
    , date : String
    , description : String
    , amount : Float
    }


fromQuicken : Quicken.Transaction -> Transaction
fromQuicken { date, description, amount } =
    Transaction Nothing date (decodeHtml description) (-amount)


setPayFrom : Maybe String -> Transaction -> Transaction
setPayFrom value transaction =
    { transaction | payFrom = value }


amp : Regex
amp =
    Regex.regex "&amp;"


decodeHtml : String -> String
decodeHtml =
    Regex.replace Regex.All amp (\_ -> "&")
