module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder, Value)
import Dict
import Regex
import FormatNumber as Number
import FormatNumber.Locales as Number
import Ports
import Transaction exposing (Transaction)
import List.Extra as List


-- MODEL


type alias Model =
    { transactions : List Transaction
    , accounts : List String
    , draftAccount : String
    , message : Maybe String
    , dragging : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { transactions = []
      , accounts = []
      , draftAccount = ""
      , message = Nothing
      , dragging = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UploadFile Value
    | ReadFile String
    | SelectAccount Int String
    | SetDraftAccount String
    | AddAccount
    | DragOver
    | DragLeave


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadFile file ->
            ( { model | dragging = False }, Ports.upload file )

        ReadFile text ->
            let
                transactions =
                    parseCSV text

                message =
                    if List.isEmpty transactions then
                        Just "Sorry, I wasn't able to read any transactions from the uploaded file."
                    else
                        Nothing

                updated =
                    List.map
                        (Transaction.setPayFrom (List.head model.accounts))
                        transactions
            in
                ( { model
                    | transactions = updated
                    , message = message
                  }
                , Cmd.none
                )

        SelectAccount index account ->
            let
                transactions =
                    updateList index (\t -> { t | payFrom = Just account }) model.transactions
            in
                ( { model | transactions = transactions }, Cmd.none )

        SetDraftAccount name ->
            ( { model | draftAccount = name }, Cmd.none )

        AddAccount ->
            ( { model
                | draftAccount = ""
                , accounts = model.accounts ++ [ model.draftAccount ]
              }
            , Cmd.none
            )

        DragOver ->
            ( { model | dragging = True }, Cmd.none )

        DragLeave ->
            ( { model | dragging = False }, Cmd.none )


updateList : Int -> (a -> a) -> List a -> List a
updateList index update list =
    let
        mapFn i row =
            if i == index then
                update row
            else
                row
    in
        List.indexedMap mapFn list


parseCSV : String -> List Transaction
parseCSV text =
    let
        rows =
            text
                |> Regex.replace Regex.All (Regex.regex "&amp;") (\_ -> "&")
                |> String.lines
                |> List.map (String.split ",")
    in
        case rows of
            [] -> []

            headers :: data ->
                data
                    |> List.map
                        (Transaction.fromDict << Dict.fromList << (zip headers))
                    |> List.filterMap identity
                    |> List.takeWhile ((==) "Sale" << .transType)


zip : (List a) -> (List b) -> List (a, b)
zip =
    List.map2 (,)


fileInputEventDecoder : Decoder Value
fileInputEventDecoder =
    Decode.at [ "target", "files", "0" ] Decode.value


dropEventDecoder : Decoder Value
dropEventDecoder =
    Decode.at [ "dataTransfer", "files", "0" ] Decode.value


-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.class "payback" ]
        [ newAccountInput model.draftAccount
        , aggregateTable model
        , transactionTable model
        ]


fileUploadId : String
fileUploadId = "file-upload"


fileUploader : Html Msg
fileUploader =
    Html.div
        [ Attributes.class "file-upload-container" ]
        [ Html.input
            [ Attributes.type_ "file"
            , Attributes.id fileUploadId
            , Events.on "change" (Decode.map UploadFile fileInputEventDecoder)
            ]
            []
        , Html.label
            [ Attributes.for fileUploadId ]
            [ Html.i
                [ Attributes.class "material-icons" ]
                [ Html.text "file_upload" ]
            , Html.span []
                [ Html.text "Upload" ]
            ]
        ]


fileDrop : Maybe String -> Bool -> Html Msg
fileDrop message dragging =
    Html.div
        [ Events.onWithOptions
            "drop"
            { stopPropagation = False, preventDefault = True }
            (Decode.map UploadFile dropEventDecoder)
        , Events.onWithOptions
            "dragover"
            { stopPropagation = False, preventDefault = True }
            (Decode.succeed DragOver)
        , Events.onWithOptions
            "dragleave"
            { stopPropagation = False, preventDefault = True }
            (Decode.succeed DragLeave)
        , Attributes.classList
            [ ( "file-drop", True )
            , ( "file-drop--dragging", dragging )
            ]
        ]
        [ error message
        , Html.div []
            [ fileUploader
            , Html.span [] [ Html.text " or drop file." ]
            ]
        ]


newAccountInput : String -> Html Msg
newAccountInput value =
    Html.form
        [ Events.onSubmit AddAccount
        , Attributes.class "new-account"
        ]
        [ Html.input
            [ Attributes.placeholder "Add Account"
            , Attributes.value value
            , Events.onInput SetDraftAccount
            ]
            []
        ]


aggregateTable : Model -> Html Msg
aggregateTable { transactions, accounts } =
    case accounts of
        [] ->
            Html.text "Add some accounts to begin."

        _ ->
            Html.div
                [ Attributes.class "aggregate-table" ]
                [ Html.table []
                    [ Html.thead []
                        [ Html.tr [] <|
                            (tableHeader "Total")
                            :: (List.map tableHeader accounts)
                        ]
                    , Html.tbody []
                        [ Html.tr [] <|
                            (tableCell (formatTotal transactions))
                            :: (List.map (tableCell << formatSubTotal transactions) accounts)
                        ]
                    ]
                ]


tableCell : String -> Html Msg
tableCell text =
    Html.td [] [ Html.text text ]


tableHeader : String -> Html Msg
tableHeader text =
    Html.th [] [ Html.text text ]


formatSubTotal : List Transaction -> String -> String
formatSubTotal transactions account =
    transactions
        |> List.filter ((==) (Just account) << .payFrom)
        |> formatTotal


formatTotal : List Transaction -> String
formatTotal transactions =
    transactions
        |> List.map .amount
        |> List.sum
        |> Number.format Number.usLocale


transactionTable : Model -> Html Msg
transactionTable { transactions, accounts, message, dragging } =
    case ( accounts, transactions ) of
        ( [], _ ) ->
            Html.text ""

        ( _, [] ) ->
            fileDrop message dragging

        _ ->
            Html.div
                [ Attributes.class "transactions" ]
                [ Html.table []
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th [] [ Html.text "Description" ]
                            , Html.th [] [ Html.text "Amount" ]
                            , Html.th [] [ Html.text "Transaction Date" ]
                            , Html.th [] [ Html.text "Post Date" ]
                            , Html.th [] [ Html.text "Type" ]
                            , Html.th [] [ Html.text "Pay From" ]
                            ]
                        ]
                    , Html.tbody [] (List.indexedMap (transactionRow accounts) transactions)
                    ]
                ]


transactionRow : List String -> Int -> Transaction -> Html Msg
transactionRow accounts index transaction =
    Html.tr []
        [ Html.td [] [ Html.text transaction.description ]
        , Html.td [] [ Html.text (toString transaction.amount) ]
        , Html.td [] [ Html.text transaction.transDate ]
        , Html.td [] [ Html.text transaction.postDate ]
        , Html.td [] [ Html.text transaction.transType ]
        , Html.td [] [ accountPicker accounts index transaction.payFrom ]
        ]


accountPicker : List String -> Int -> Maybe String -> Html Msg
accountPicker accounts index selected =
    Html.select
        [ Attributes.value (Maybe.withDefault "" selected)
        , Events.on "change" (Decode.map (SelectAccount index) Events.targetValue)
        ]
        (List.map accountOption accounts)


accountOption : String -> Html Msg
accountOption account =
    Html.option
        [ Attributes.value account ]
        [ Html.text account ]


error : Maybe String -> Html Msg
error maybeMessage =
    case maybeMessage of
        Nothing ->
            Html.text ""

        Just msg ->
            Html.div
                [ Attributes.class "error-container" ]
                [ Html.div []
                    [ Html.text "¯\\_(ツ)_/¯" ]
                , Html.div []
                    [ Html.text msg ]
                ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.readFile ReadFile


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
