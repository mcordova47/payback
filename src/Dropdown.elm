module Dropdown exposing (Props, view)

import Css
import Css.Colors as Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Styles


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
    Html.div []
        [ dropdownBody props ]


dropdownBody : Props msg -> Html msg
dropdownBody props =
    if props.opened then
        Html.div
            [ css
                [ Css.position Css.absolute
                , Css.top (Css.px 0)
                , Css.backgroundColor Colors.white
                , Css.zIndex (Css.int 1)
                , Css.boxShadow5
                    (Css.px -1)
                    (Css.px 2)
                    (Css.px 2)
                    (Css.px 1)
                    (Css.rgba 0 0 0 0.2)
                ]
            ]
            (List.map (dropdownOption props) props.options)
    else
        Html.div
            [ Events.onWithOptions
                "click"
                { stopPropagation = True, preventDefault = False }
                (Decode.succeed props.handleOpen)
            , css [ Css.cursor Css.pointer ]
            ]
            [ props.selected
                |> Maybe.withDefault props.placeholder
                |> Html.text
            , Html.i
                [ Attributes.class "material-icons"
                , css
                    [ Css.position Css.absolute
                    , Css.top (Css.px 7)
                    ]
                ]
                [ Html.text "arrow_drop_down" ]
            ]


dropdownOption : Props msg -> String -> Html msg
dropdownOption props option =
    Html.div
        [ Events.onClick (props.handleSelect option)
        , css
            [ Css.padding (Css.px 10)
            , Css.cursor Css.pointer
            , Css.hover
                [ Css.backgroundColor Styles.grayHighlight ]
            ]
        , Styles.cssIf
            (props.selected == Just option)
            [ Css.color Styles.primaryColor ]
        ]
        [ Html.text option ]
