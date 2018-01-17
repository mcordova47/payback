module Quicken exposing (QIFFile, Transaction, parse)

import Parser exposing (Parser, Count(..), (|=), (|.))


type alias QIFFile =
    { type_ : String
    , transactions : List Transaction
    }


type alias Transaction =
    { date : String
    , description : String
    , amount : Float

    -- TODO: add optional fields
    -- , clearedStatus : String
    -- , referenceNumber : String
    -- , memo : String
    }


type TransactionField
    = DateField String
    | DescriptionField String
    | AmountField Float
    | ClearedStatusField String
    | ReferenceNumberField String
    | MemoField String


fromFields : List TransactionField -> Maybe Transaction
fromFields fields =
    Maybe.map3 Transaction
        (getField getDate fields)
        (getField getDescription fields)
        (getField getAmount fields)


getField : (TransactionField -> Maybe a) -> List TransactionField -> Maybe a
getField getter =
    List.head << List.filterMap getter


getDate : TransactionField -> Maybe String
getDate field =
    case field of
        DateField date ->
            Just date

        _ ->
            Nothing


getDescription : TransactionField -> Maybe String
getDescription field =
    case field of
        DescriptionField desc ->
            Just desc

        _ ->
            Nothing


getAmount : TransactionField -> Maybe Float
getAmount field =
    case field of
        AmountField amount ->
            Just amount

        _ ->
            Nothing


getClearedStatus : TransactionField -> Maybe String
getClearedStatus field =
    case field of
        ClearedStatusField status ->
            Just status

        _ ->
            Nothing


getReferenceNumber : TransactionField -> Maybe String
getReferenceNumber field =
    case field of
        ReferenceNumberField refNum ->
            Just refNum

        _ ->
            Nothing


getMemo : TransactionField -> Maybe String
getMemo field =
    case field of
        MemoField memo ->
            Just memo

        _ ->
            Nothing


parse : String -> Result Parser.Error QIFFile
parse =
    Parser.run qif


qif : Parser QIFFile
qif =
    Parser.succeed QIFFile
        |. Parser.ignore Parser.zeroOrMore (charIsIn "\n\x0D")
        |= qifType
        |= Parser.repeat Parser.zeroOrMore transaction


transaction : Parser Transaction
transaction =
    Parser.succeed fromFields
        |= Parser.repeat (AtLeast 3) transactionField
        |. Parser.oneOf
            [ newLineSymbol "^"
            , newLineSymbol ""
            ]
        |> Parser.andThen (parseMaybe "unable to parse transaction")


transactionField : Parser TransactionField
transactionField =
    Parser.oneOf
        [ dateField
        , descriptionField
        , amountField
        , clearedStatusField
        , referenceNumberField
        , memoField
        ]


dateField : Parser TransactionField
dateField =
    Parser.succeed DateField
        |. newLineSymbol "D"
        |= fullLine


descriptionField : Parser TransactionField
descriptionField =
    Parser.succeed DescriptionField
        |. newLineSymbol "P"
        |= fullLine


amountField : Parser TransactionField
amountField =
    Parser.succeed AmountField
        |. newLineSymbol "T"
        |= float


clearedStatusField : Parser TransactionField
clearedStatusField =
    Parser.succeed ClearedStatusField
        |. newLineSymbol "C"
        |= fullLine


referenceNumberField : Parser TransactionField
referenceNumberField =
    Parser.succeed ReferenceNumberField
        |. newLineSymbol "N"
        |= fullLine


memoField : Parser TransactionField
memoField =
    Parser.succeed MemoField
        |. newLineSymbol "M"
        |= fullLine


qifType : Parser String
qifType =
    Parser.succeed identity
        |. Parser.ignore Parser.zeroOrMore (charIsIn "\x0D\n")
        |. Parser.keyword "!Type:"
        |= fullLine


newLineSymbol : String -> Parser ()
newLineSymbol symbol =
    Parser.oneOf
        [ Parser.symbol ("\x0D\n" ++ symbol)
        , Parser.symbol ("\n" ++ symbol)
        ]


float : Parser Float
float =
    Parser.keep Parser.oneOrMore (charIsIn "0123456789-.")
        |> Parser.andThen parseFloat


parseMaybe : String -> Maybe a -> Parser a
parseMaybe msg maybeA =
    case maybeA of
        Just a ->
            Parser.succeed a

        Nothing ->
            Parser.fail msg


parseFloat : String -> Parser Float
parseFloat str =
    case String.toFloat str of
        Ok float ->
            Parser.succeed float

        Err err ->
            Parser.fail ("bad float: " ++ toString err)


fullLine : Parser String
fullLine =
    Parser.keep Parser.oneOrMore (not << charIsIn "\x0D\n")


charIsIn : String -> Char -> Bool
charIsIn str chr =
    String.any ((==) chr) str
