module Dropdown exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes


-- MODEL


type alias Config =
    { options : List String
    , selected : Maybe String
    , id : Int
    , placeholder : String
    }


type State
    = Opened
    | Closed


init : State
init = Closed


-- UPDATE


type Msg
    = Open Int
    | Close Int
--     | Select Int String


update : Msg -> State -> State
update msg state =
    case msg of
        Open id ->
            Opened

        Close id ->
            Closed

{-         Select id option ->
             -}


-- VIEW


view : Config -> State -> Html Msg
view config state =
    Html.div
        [ Attributes.class "dropdown" ]
        [ dropdownBody config state ]


dropdownBody : Config -> State -> Html Msg
dropdownBody config state =
    case state of
        Closed ->
            Html.div
                [ Attributes.class "dropdown__label"
                , Events.onClick (Open config.id)
                ]
                [ config.selected
                    |> Maybe.withDefault config.placeholder
                    |> Html.text
                ]

        Opened ->
            Html.div
                [ Attributes.class "dropdown__list" ]
                (List.map dropdownOption config.options)


dropdownOption : String -> Html Msg
dropdownOption option =
    Html.div
        [ Attributes.class "dropdown__list__item" ]
        [ Html.text option ]
