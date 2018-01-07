module Dropdown exposing (Props, view)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode


type alias Props msg =
    { options : List String
    , placeholder : String
    , selected : Maybe String
    , opened : Bool
    , handleOpen : msg
    , handleSelect : String -> msg
    }


view : Props msg -> Html msg
view props =
    Html.div
        [ Attributes.class "dropdown" ]
        [ dropdownBody props ]


dropdownBody : Props msg -> Html msg
dropdownBody props =
    if props.opened then
        Html.div
            [ Attributes.class "dropdown__list" ]
            (List.map (dropdownOption props) props.options)
    else
        Html.div
            [ Attributes.class "dropdown__label"
            , Events.onWithOptions
                "click"
                { stopPropagation = True, preventDefault = False }
                (Decode.succeed props.handleOpen)
            ]
            [ props.selected
                |> Maybe.withDefault props.placeholder
                |> Html.text
            , Html.i
                [ Attributes.class "material-icons" ]
                [ Html.text "arrow_drop_down" ]
            ]


dropdownOption : Props msg -> String -> Html msg
dropdownOption props option =
    Html.div
        [ Attributes.classList
            [ ( "dropdown__list__item", True )
            , ( "dropdown__list__item--selected"
              , props.selected == Just option
              )
            ]
        , Events.onClick (props.handleSelect option)
        ]
        [ Html.text option ]
