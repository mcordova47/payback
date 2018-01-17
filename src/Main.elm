module Main exposing (main)

import Html as H exposing (program)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Css exposing (Color, sansSerif)
import Css.Colors as Colors
import Json.Decode as Decode exposing (Decoder, Value)
import Dict exposing (Dict)
import FormatNumber as Number
import FormatNumber.Locales as Number
import Ports
import Transaction exposing (Transaction)
import Dropdown
import Mouse
import Styles
import Quicken


-- MODEL


type alias Model =
    { transactions : List Transaction
    , accounts : List String
    , draftAccount : String
    , message : Maybe String
    , dragging : Bool
    , openedDropdown : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { transactions = []
      , accounts = []
      , draftAccount = ""
      , message = Nothing
      , dragging = False
      , openedDropdown = Nothing
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
    | OpenDropdown Int
    | DragOver
    | DragLeave
    | MouseClick Mouse.Position
    | Delete Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadFile file ->
            ( { model | dragging = False }, Ports.upload file )

        ReadFile text ->
            let
                transactions =
                    Quicken.parse text
                        |> Debug.log "parsed text"
                        |> Result.map .transactions
                        |> Result.withDefault []
                        |> List.map Transaction.fromQuicken

                message =
                    if List.isEmpty transactions then
                        Just "Sorry, I wasn't able to read any transactions from the uploaded file."
                    else
                        Nothing

                updatedTransactions =
                    List.map
                        (Transaction.setPayFrom (List.head model.accounts))
                        transactions
            in
                ( { model
                    | transactions = updatedTransactions
                    , message = message
                  }
                , Cmd.none
                )

        SelectAccount index account ->
            let
                transactions =
                    updateList index (\t -> { t | payFrom = Just account }) model.transactions
            in
                ( { model
                    | transactions = transactions
                    , openedDropdown = Nothing
                  }
                , Cmd.none
                )

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

        OpenDropdown index ->
            ( { model | openedDropdown = Just index }, Cmd.none )

        MouseClick _ ->
            ( { model | openedDropdown = Nothing }, Cmd.none )

        Delete index ->
            let
                transactions =
                    model.transactions
                        |> List.indexedMap (,)
                        |> List.filter ((/=) index << Tuple.first)
                        |> List.map Tuple.second
            in
                ( { model | transactions = transactions }, Cmd.none )


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


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)


fileInputEventDecoder : Decoder Value
fileInputEventDecoder =
    Decode.at [ "target", "files", "0" ] Decode.value


dropEventDecoder : Decoder Value
dropEventDecoder =
    Decode.at [ "dataTransfer", "files", "0" ] Decode.value



-- VIEW


view : Model -> H.Html Msg
view model =
    Html.div
        [ css
            [ Css.fontFamilies [ "Roboto", sansSerif.value ]
            , Css.color Styles.fontColor
            ]
        ]
        [ banner
        , appContent model
        , Styles.global
        ]
        |> Html.toUnstyled


appContent : Model -> Html Msg
appContent model =
    Html.div
        [ css [ Css.padding (Css.px 10) ] ]
        [ newAccountInput model.draftAccount
        , aggregateTable model
        , transactionTable model
        ]


banner : Html Msg
banner =
    Html.div
        [ css
            [ Css.backgroundColor Styles.primaryColor
            , Css.color Colors.white
            , Css.padding (Css.px 10)
            ]
        ]
        [ Html.div
            [ css
                [ Css.fontFamilies [ "Raleway", sansSerif.value ]
                , Css.fontSize (Css.px 32)
                ]
            ]
            [ Html.text "Payback" ]
        ]


fileUploader : Html Msg
fileUploader =
    Html.div
        [ css
            [ Css.display Css.inlineBlock
            , Css.marginBottom (Css.px -7)
            ]
        ]
        [ Html.input
            [ Attributes.type_ "file"
            , Attributes.id "file-upload"
            , Events.on "change" (Decode.map UploadFile fileInputEventDecoder)
            , css [ Css.display Css.none ]
            ]
            []
        , Html.label
            [ Attributes.for "file-upload"
            , css [ Css.cursor Css.pointer ]
            ]
            [ Html.i
                [ Attributes.class "material-icons"
                , css
                    [ Css.float Css.left
                    , Css.color (Css.hex "a0a0a0")
                    ]
                ]
                [ Html.text "file_upload" ]
            , Html.span
                [ css
                    [ Css.float Css.left
                    , Css.padding2 (Css.px 2) (Css.px 5)
                    ]
                ]
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
        , css
            [ Css.height (Css.px 250)
            , Css.width (Css.px 500)
            , Css.border3 (Css.px 2) Css.dashed Styles.gray
            , Css.borderRadius (Css.px 5)
            , Css.textAlign (Css.center)
            , Css.paddingTop (Css.px 235)
            ]
        , Styles.cssIf dragging
            [ Css.backgroundColor Styles.grayHighlight ]
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
        [ Events.onSubmit AddAccount ]
        [ Html.input
            [ Attributes.placeholder "Add Account"
            , Attributes.value value
            , Events.onInput SetDraftAccount
            , css
                [ Css.borderWidth4 (Css.px 0) (Css.px 0) (Css.px 3) (Css.px 0)
                , Css.outline Css.none
                , Css.padding (Css.px 5)
                , Css.fontSize (Css.px 16)
                , Css.marginBottom (Css.px 10)
                , Css.focus
                    [ Css.borderColor Styles.primaryColor ]
                ]
            ]
            []
        ]


aggregateTable : Model -> Html Msg
aggregateTable { transactions, accounts } =
    case accounts of
        [] ->
            Html.text "Add some bank accounts to begin."

        _ ->
            Html.div []
                [ Styles.table
                    [ css
                        [ Css.borderCollapse Css.collapse
                        , Css.marginBottom (Css.px 10)
                        ]
                    ]
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
    Styles.td [] [ Html.text text ]


tableHeader : String -> Html Msg
tableHeader text =
    Styles.th [] [ Html.text text ]


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
transactionTable { transactions, accounts, message, dragging, openedDropdown } =
    case ( accounts, transactions ) of
        ( [], _ ) ->
            Html.text ""

        ( _, [] ) ->
            fileDrop message dragging

        _ ->
            Html.div []
                [ Styles.table []
                    [ Html.thead []
                        [ Html.tr []
                            [ Styles.th [] [ Html.text "Description" ]
                            , Styles.th [] [ Html.text "Amount" ]
                            , Styles.th [] [ Html.text "Date" ]
                            , Styles.th [] [ Html.text "Pay From" ]
                            , Styles.th [] [ Html.text "" ]
                            ]
                        ]
                    , Html.tbody [] (List.indexedMap (transactionRow accounts openedDropdown) transactions)
                    ]
                ]


transactionRow : List String -> Maybe Int -> Int -> Transaction -> Html Msg
transactionRow accounts openedDropdown index transaction =
    Html.tr []
        [ Styles.td [] [ Html.text transaction.description ]
        , Styles.td [] [ Html.text (toString transaction.amount) ]
        , Styles.td [] [ Html.text transaction.date ]
        , Styles.td [] [ accountPicker accounts openedDropdown index transaction.payFrom ]
        , Styles.td [] [ deleteButton index ]
        ]


deleteButton : Int -> Html Msg
deleteButton index =
    Html.div
        [ Events.onClick (Delete index)
        , css
            [ Css.cursor Css.pointer ]
        ]
        [ Html.i
            [ Attributes.class "material-icons delete-icon"
            , css
                [ Css.color Styles.gray ]
            ]
            [ Html.text "delete" ]
        ]


accountPicker : List String -> Maybe Int -> Int -> Maybe String -> Html Msg
accountPicker accounts openedDropdown index selected =
    let
        opened =
            openedDropdown
                |> Maybe.map ((==) index)
                |> Maybe.withDefault False
    in
        Dropdown.view
            { options = accounts
            , placeholder = "Select Account"
            , selected = selected
            , opened = opened
            , handleOpen = OpenDropdown index
            , handleSelect = SelectAccount index
            }
            { primary = Styles.primaryColor
            , highlight = Styles.grayHighlight
            }


error : Maybe String -> Html Msg
error maybeMessage =
    case maybeMessage of
        Nothing ->
            Html.text ""

        Just msg ->
            Html.div
                [ css
                    [ Css.textAlign Css.center
                    , Css.width (Css.px 500)
                    , Css.marginTop (Css.px -30)
                    , Css.marginBottom (Css.px 10)
                    ]
                ]
                [ Html.div [] [ Html.text "¯\\_(ツ)_/¯" ]
                , Html.div [] [ Html.text msg ]
                ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.readFile ReadFile
        , Mouse.clicks MouseClick
        ]


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
